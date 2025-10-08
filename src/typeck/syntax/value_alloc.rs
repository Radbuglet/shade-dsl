use std::{
    cell::{Cell, RefCell},
    fmt,
    hash::{self, BuildHasherDefault, Hash, Hasher},
    mem,
    ops::ControlFlow,
    ptr::NonNull,
};

use bumpalo::Bump;
use thunderdome::Arena;

use crate::{
    base::{
        analysis::{
            IsoSccPortrait, LabelledDiGraph, LabelledSuccessor, SccNode, portrait_keys, tarjan,
        },
        arena::{LateInit, Obj},
    },
    match_pair,
    typeck::{
        analysis::TyCtxt,
        syntax::{AnyFuncValue, ScalarKind, Ty, Value, ValuePlace, ValueScalar},
    },
    utils::hash::{FxHashMap, FxHashSet, FxHasher, hash_map},
};

// === ValueArenaLike === //

thread_local! {
    static CURRENT_VALUE_ARENA: Cell<Option<NonNull<dyn ValueArenaLike>>>
        = const { Cell::new(None) };

    static REENTRANT_FMT: RefCell<FxHashSet<ValuePlace>> =
        const { RefCell::new(FxHashSet::with_hasher(BuildHasherDefault::new()) )};
}

pub trait ValueArenaLike {
    fn read(&self, ptr: ValuePlace) -> &Value;
}

impl fmt::Debug for ValuePlace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}v{}", self.0.slot(), self.0.generation())?;

        if let Some(arena) = CURRENT_VALUE_ARENA.get()
            && REENTRANT_FMT.with_borrow_mut(|v| v.insert(*self))
        {
            let _guard = scopeguard::guard((), |()| {
                REENTRANT_FMT.with_borrow_mut(|v| v.remove(self));
            });

            let arena = unsafe { arena.as_ref() };

            f.write_str(": ")?;
            arena.read(*self).fmt(f)?;
        }

        Ok(())
    }
}

pub struct ArenaFmtWrapper<'a> {
    arena: &'a (dyn ValueArenaLike + 'a),
    target: ValuePlace,
}

impl fmt::Debug for ArenaFmtWrapper<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let _guard = scopeguard::guard(
            CURRENT_VALUE_ARENA.replace(Some(
                NonNull::new(self.arena as *const dyn ValueArenaLike as *mut dyn ValueArenaLike)
                    .unwrap(),
            )),
            |old| CURRENT_VALUE_ARENA.set(old),
        );

        self.target.fmt(f)
    }
}

impl ValuePlace {
    pub fn debug(self, arena: &dyn ValueArenaLike) -> ArenaFmtWrapper<'_> {
        ArenaFmtWrapper {
            arena,
            target: self,
        }
    }
}

// === ValueArena === //

#[derive(Default)]
pub struct ValueArena {
    arena: Arena<Option<Value>>,
}

impl fmt::Debug for ValueArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ValueArena").finish_non_exhaustive()
    }
}

impl ValueArena {
    pub fn reserve(&mut self) -> ValuePlace {
        ValuePlace(self.arena.insert(None))
    }

    pub fn alloc(&mut self, value: Value) -> ValuePlace {
        ValuePlace(self.arena.insert(Some(value)))
    }

    pub fn free(&mut self, ptr: ValuePlace) {
        let mut stack = vec![ptr];

        while let Some(ptr) = stack.pop() {
            let Some(value) = self.arena.remove(ptr.0).unwrap() else {
                continue;
            };

            if matches!(value, Value::Pointer(_)) {
                // (do not follow)
                continue;
            }

            cbit::cbit!(for pointee in follow_node_ref(&value) {
                stack.push(pointee);
            });
        }
    }

    pub fn is_init(&self, ptr: ValuePlace) -> bool {
        self.arena[ptr.0].is_some()
    }

    pub fn init(&mut self, ptr: ValuePlace, value: Value) {
        debug_assert!(!self.is_init(ptr));
        self.arena[ptr.0] = Some(value);
    }

    pub fn read(&self, ptr: ValuePlace) -> &Value {
        self.arena[ptr.0].as_ref().expect("place never initialized")
    }

    pub fn write_unchecked(&mut self, ptr: ValuePlace, value: Value) {
        self.arena[ptr.0] = Some(value);
    }

    #[expect(clippy::diverging_sub_expression)]
    pub fn write_terminal(&mut self, ptr: ValuePlace, value: Value) {
        #[cfg(debug_assertions)]
        cbit::cbit!(for _ in follow_node_ref(&value) {
            panic!("value is not terminal");
        });

        self.write_unchecked(ptr, value);
    }

    pub fn assign(&mut self, from: ValuePlace, to: ValuePlace) {
        let mut stack = vec![(from, to)];

        while let Some((from_handle, to_handle)) = stack.pop() {
            let (Some(from), Some(to)) = self.arena.get2_mut(from_handle.0, to_handle.0) else {
                unreachable!()
            };

            let Some(from) = &*from else {
                continue;
            };

            let Some(to) = to else {
                self.copy_from_with_initial_root(
                    None::<&ValueArena>,
                    from_handle,
                    to_handle,
                    CopyDepth::Shallow,
                );
                continue;
            };

            match_pair!((from, to) => {
                // External references are interned so terminal copies are okay.
                (Value::MetaType(from), Value::MetaType(to)) => {
                    *to = *from;
                }
                (Value::MetaFunc(from), Value::MetaFunc(to)) => {
                    *to = *from;
                }
                (Value::MetaString(from), Value::MetaString(to)) => {
                    *to = *from;
                }
                (Value::Func(from), Value::Func(to)) => {
                    *to = *from;
                }

                // Pointers are raw pointers. Just copy the address.
                (Value::Pointer(from), Value::Pointer(to)) => {
                    *to = *from;
                }

                // These are naturally terminal.
                (Value::Scalar(from), Value::Scalar(to)) => {
                    *to = *from;
                }

                // Here are the actually interesting cases.
                (Value::Tuple(from), Value::Tuple(to))
                | (Value::Array(from), Value::Array(to)) => {
                    for (&from, &mut to) in from.iter().zip(to) {
                        stack.push((from, to));
                    }
                }
                (
                    Value::AdtAggregate { def: from_def, fields: from },
                    Value::AdtAggregate { def: to_def, fields: to },
                ) => {
                    debug_assert_eq!(from_def, to_def);
                    for (&from, &mut to) in from.iter().zip(to) {
                        stack.push((from, to));
                    }
                }
                (Value::MetaArray(from), Value::MetaArray(to)) => {
                    if from.len() == to.len() {
                        for (&from, &mut to) in from.iter().zip(to) {
                            stack.push((from, to));
                        }
                    } else {
                        let old_to = mem::take(to);
                        let mut new_to = from.clone();

                        for place in old_to {
                            self.free(place);
                        }

                        for elem in &mut new_to {
                            *elem = self.copy(*elem, CopyDepth::Shallow);
                        }

                        let Some(Value::MetaArray(to)) = &mut self.arena[to_handle.0] else {
                            unreachable!()
                        };

                        *to = new_to;
                    }
                }
                (
                    Value::AdtVariant { def: from_def, variant: from_variant, inner: from_pointee },
                    Value::AdtVariant { def: to_def, variant: to_variant, inner: to_pointee },
                ) => {
                    if from_variant == to_variant {
                        stack.push((*from_pointee, *to_pointee));
                    } else {
                        *to_variant = *from_variant;

                        let from_pointee = *from_pointee;
                        let old_to_pointee = *to_pointee;

                        self.free(old_to_pointee);

                        let new_to_pointee = self.copy(from_pointee, CopyDepth::Shallow);

                        let Some(Value::Pointer(to_pointee)) = &mut self.arena[to_handle.0] else {
                            unreachable!()
                        };

                        *to_pointee = new_to_pointee;
                    }
                }
                _ => {
                    unreachable!()
                }
            })
        }
    }

    pub fn copy(&mut self, target: ValuePlace, depth: CopyDepth) -> ValuePlace {
        self.copy_from(Option::<&Self>::None, target, depth)
    }

    pub fn copy_from(
        &mut self,
        from_arena: Option<&impl ValueArenaLike>,
        from_root: ValuePlace,
        depth: CopyDepth,
    ) -> ValuePlace {
        let to_root = self.reserve();
        self.copy_from_with_initial_root(from_arena, from_root, to_root, depth);
        to_root
    }

    pub fn copy_from_with_initial_root(
        &mut self,
        from_arena: Option<&impl ValueArenaLike>,
        from_root: ValuePlace,
        to_root: ValuePlace,
        depth: CopyDepth,
    ) {
        let mut stack = vec![(from_root, to_root)];
        let mut mapping = FxHashMap::from_iter([(from_root, to_root)]);

        while let Some((from, to)) = stack.pop() {
            let mut value = from_arena
                .map_or_else(|| self.read(from), |other| other.read(from))
                .clone();

            if depth.is_deep() || !matches!(value, Value::Pointer(_)) {
                cbit::cbit!(for edge in follow_node_mut(&mut value) {
                    let edge_from = *edge;
                    *edge = *mapping.entry(edge_from).or_insert_with(|| {
                        let edge_to = self.reserve();
                        stack.push((edge_from, edge_to));
                        edge_to
                    });
                });
            }

            self.write_unchecked(to, value);
        }
    }

    pub fn init_tuple(&mut self, ptr: ValuePlace, fields: u32) {
        if let Some(existing) = &self.arena[ptr.0] {
            let Value::Tuple(existing) = existing else {
                unreachable!();
            };

            assert_eq!(existing.len(), fields as usize);
            return;
        }

        let inner = Value::Tuple((0..fields).map(|_| self.reserve()).collect());

        self.write_unchecked(ptr, inner);
    }
}

impl ValueArenaLike for ValueArena {
    fn read(&self, ptr: ValuePlace) -> &Value {
        self.read(ptr)
    }
}

impl LabelledDiGraph for ValueArena {
    type Node = ValuePlace;
    type Placeholder = ();

    fn successors<B>(
        &self,
        &node: &Self::Node,
        mut f: impl FnMut(LabelledSuccessor<Self::Node, Self::Placeholder>) -> ControlFlow<B>,
    ) -> ControlFlow<B> {
        let _ = f;
        follow_node_ref(self.read(node), |node| f(LabelledSuccessor::Node(node)))
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum CopyDepth {
    Deep,
    Shallow,
}

impl CopyDepth {
    pub fn is_deep(self) -> bool {
        matches!(self, Self::Deep)
    }

    pub fn is_shallow(self) -> bool {
        matches!(self, Self::Shallow)
    }
}

// === ValueBump === //

#[derive(Default)]
pub struct ValueBump {
    bump: Bump,
    arena: RefCell<Arena<NonNull<LateInit<Value>>>>,
}

impl fmt::Debug for ValueBump {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ValueBump").finish_non_exhaustive()
    }
}

impl ValueBump {
    pub fn reserve(&self) -> ValuePlace {
        let value = self.bump.alloc(LateInit::uninit());
        ValuePlace(self.arena.borrow_mut().insert(NonNull::from(value)))
    }

    pub fn fetch(&self, ptr: ValuePlace) -> &LateInit<Value> {
        unsafe { self.arena.borrow()[ptr.0].as_ref() }
    }

    pub fn init(&self, ptr: ValuePlace, value: Value) {
        LateInit::init(self.fetch(ptr), value);
    }

    pub fn alloc(&self, value: Value) -> ValuePlace {
        let ptr = self.reserve();
        self.init(ptr, value);
        ptr
    }

    pub fn read(&self, ptr: ValuePlace) -> &Value {
        self.fetch(ptr)
    }
}

impl ValueArenaLike for ValueBump {
    fn read(&self, ptr: ValuePlace) -> &Value {
        self.read(ptr)
    }
}

// === ValueInterner === //

#[derive(Default)]
pub struct ValueInterner {
    value_arena: ValueBump,
    value_interns: RefCell<FxHashMap<InternEntry, ()>>,
}

struct InternEntry {
    portrait: IsoSccPortrait<ValuePlace, ValuePlace, ()>,
    hash: u64,
}

impl fmt::Debug for ValueInterner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ValueInterner").finish_non_exhaustive()
    }
}

impl ValueInterner {
    pub fn intern(&self, user_graph: &ValueArena, user_root: ValuePlace) -> ValuePlace {
        let mut resolved_canonicals = FxHashMap::<ValuePlace, ValuePlace>::default();
        let mut value_interns = self.value_interns.borrow_mut();

        cbit::cbit!(for scc in tarjan(user_graph, [user_root]) {
            let keys = portrait_keys(user_graph, scc, |&node| {
                let mut hasher = FxHasher::default();
                hash_value_terminal(user_graph.read(node), &mut hasher);
                hasher.finish()
            });

            // See whether an existing canonical graph exists.
            let mut user_portrait_if_no_canon = None;
            let user_and_canonical_portrait = keys
                .into_iter()
                .filter_map(|&key| {
                    let portrait = IsoSccPortrait::new(user_graph, key, |&node| {
                        match resolved_canonicals.get(&node).cloned() {
                            Some(ext) => SccNode::External(ext),
                            None => SccNode::Internal(node),
                        }
                    });

                    let portrait_hash = {
                        let mut hasher = FxHasher::default();
                        portrait.hash(
                            |&node, hasher| hash_value_terminal(user_graph.read(node), hasher),
                            &mut hasher,
                        );
                        hasher.finish()
                    };

                    let canonical_portrait =
                        value_interns.raw_entry().from_hash(portrait_hash, |entry| {
                            if entry.hash != portrait_hash {
                                return false;
                            }

                            entry.portrait.eq(&portrait, |&lhs, &rhs| {
                                eq_value_terminal(
                                    self.read(ValuePlace(lhs.0)),
                                    user_graph.read(rhs),
                                )
                            })
                        });

                    if let Some((canonical_portrait, ())) = canonical_portrait {
                        Some((portrait, &canonical_portrait.portrait))
                    } else {
                        user_portrait_if_no_canon = Some((portrait, portrait_hash));
                        None
                    }
                })
                .next();

            // If it doesn't, create the canonical graph.
            if let Some((user_portrait, canonical_sub_graph)) = user_and_canonical_portrait {
                // Record the mapping between input nodes and their canonicals forms into
                // the `resolved_canonicals` map.
                for (&user_node, &canonical_node) in user_portrait
                    .internal_nodes
                    .iter()
                    .zip(&canonical_sub_graph.internal_nodes)
                {
                    resolved_canonicals.insert(user_node, canonical_node);
                }
            } else {
                let (user_portrait, user_portrait_hash) = user_portrait_if_no_canon.unwrap();

                // Intern every internal node, recording into the `resolved_canonicals` map.
                for &user_value in &user_portrait.internal_nodes {
                    let interned_value = self.value_arena.reserve();
                    resolved_canonicals.insert(user_value, interned_value);
                }

                for &user in &user_portrait.internal_nodes {
                    let mut value = user_graph.read(user).clone();

                    cbit::cbit!(for out_ref in follow_node_mut(&mut value) {
                        *out_ref = resolved_canonicals[out_ref];
                    });

                    self.value_arena.init(resolved_canonicals[&user], value);
                }

                let hash_map::RawEntryMut::Vacant(entry) = value_interns
                    .raw_entry_mut()
                    .from_hash(user_portrait_hash, |_| false)
                else {
                    unreachable!()
                };

                entry.insert_with_hasher(
                    user_portrait_hash,
                    InternEntry {
                        portrait: user_portrait.map(|ptr| resolved_canonicals[&ptr]),
                        hash: user_portrait_hash,
                    },
                    (),
                    |entry| entry.hash,
                );
            }
        });

        resolved_canonicals[&user_root]
    }

    pub fn read(&self, value: ValuePlace) -> &Value {
        self.value_arena.read(value)
    }

    pub fn arena(&self) -> &ValueBump {
        &self.value_arena
    }
}

// === Value Introspection === //

pub fn hash_value_terminal(value: &Value, state: &mut impl hash::Hasher) {
    mem::discriminant(value).hash(state);

    match value {
        Value::MetaType(ty) => ty.hash(state),
        Value::MetaFunc(func) => {
            func.hash(state);
        }
        Value::MetaArray(list) => {
            list.len().hash(state);
        }
        Value::MetaString(sym) => {
            sym.hash(state);
        }
        Value::Pointer(_) => {
            // (nothing)
        }
        Value::Func(func) => {
            func.hash(state);
        }
        Value::Scalar(scalar) => {
            scalar.hash(state);
        }
        Value::Tuple(list) => {
            state.write_usize(list.len());
        }
        Value::Array(list) => {
            state.write_usize(list.len());
        }
        Value::AdtAggregate { def, fields: _ } => {
            def.hash(state);
        }
        Value::AdtVariant {
            def,
            variant: id,
            inner: _,
        } => {
            def.hash(state);
            id.hash(state);
        }
    }
}

pub fn eq_value_terminal(lhs: &Value, rhs: &Value) -> bool {
    match_pair!((lhs, rhs) => {
        (Value::MetaType(lhs), Value::MetaType(rhs)) => {
            lhs == rhs
        }
        (Value::MetaFunc(lhs), Value::MetaFunc(rhs)) => {
            lhs == rhs
        }
        (Value::MetaArray(lhs), Value::MetaArray(rhs)) => {
            lhs.len() == rhs.len()
        }
        (Value::MetaString(lhs), Value::MetaString(rhs)) => {
            lhs == rhs
        }
        (Value::Pointer(_lhs_heap), Value::Pointer(_rhs_heap)) => {
            true
        }
        (Value::Func(lhs), Value::Func(rhs)) => {
            lhs == rhs
        }
        (Value::Scalar(lhs), Value::Scalar(rhs)) => {
            lhs == rhs
        }
        (Value::Tuple(lhs), Value::Tuple(rhs)) => {
            lhs.len() == rhs.len()
        }
        (Value::Array(lhs), Value::Array(rhs)) => {
            lhs.len() == rhs.len()
        }
        (
            Value::AdtAggregate { def: lhs_def, fields: _ },
            Value::AdtAggregate { def: rhs_def, fields: _ },
        ) => {
            lhs_def == rhs_def
        }
        (
            Value::AdtVariant { def: lhs_def, variant: lhs_variant, inner: _ },
            Value::AdtVariant { def: rhs_def, variant: rhs_variant, inner: _ },
        ) => {
            lhs_def == rhs_def && lhs_variant == rhs_variant
        }
        _ => {
            false
        }
    })
}

macro_rules! follow_node {
    ($value:expr, $f:expr) => {{
        let mut f = $f;

        match $value {
            Value::Tuple(values)
            | Value::Array(values)
            | Value::MetaArray(values)
            | Value::AdtAggregate {
                def: _,
                fields: values,
            } => {
                for value in values {
                    f(value)?;
                }
            }
            Value::Pointer(value)
            | Value::AdtVariant {
                def: _,
                variant: _,
                inner: value,
            } => {
                f(value)?;
            }
            Value::MetaType(_)
            | Value::MetaFunc(_)
            | Value::MetaString(_)
            | Value::Func(_)
            | Value::Scalar(_) => {
                // (nothing to follow)
            }
        }

        ControlFlow::Continue(())
    }};
}

pub fn follow_node_ref<B>(
    value: &Value,
    mut f: impl FnMut(ValuePlace) -> ControlFlow<B>,
) -> ControlFlow<B> {
    follow_node!(value, |v: &ValuePlace| f(*v))
}

pub fn follow_node_mut<B>(
    value: &mut Value,
    f: impl FnMut(&mut ValuePlace) -> ControlFlow<B>,
) -> ControlFlow<B> {
    follow_node!(value, f)
}

pub fn canonical_value_type(tcx: &TyCtxt, arena: &impl ValueArenaLike, ptr: ValuePlace) -> Obj<Ty> {
    let mut visit_set = FxHashSet::default();

    fn inner(
        tcx: &TyCtxt,
        arena: &impl ValueArenaLike,
        visit_set: &mut FxHashSet<ValuePlace>,
        ptr: ValuePlace,
    ) -> Obj<Ty> {
        if !visit_set.insert(ptr) {
            // In case someone abuses magic to get a recursive type. This should not trigger in the
            // general case.
            return tcx.intern_ty(Ty::Unknown);
        }

        match arena.read(ptr) {
            Value::MetaType(_) => tcx.intern_ty(Ty::MetaTy),
            Value::MetaFunc(_) => tcx.intern_ty(Ty::MetaFunc),
            Value::MetaArray(values) => {
                tcx.intern_ty(Ty::MetaArray(if let Some(ptr) = values.first() {
                    inner(tcx, arena, visit_set, *ptr)
                } else {
                    tcx.intern_ty(Ty::Never)
                }))
            }
            Value::MetaString(_) => tcx.intern_ty(Ty::MetaString),
            Value::Pointer(ptr) => tcx.intern_ty(Ty::Pointer(inner(tcx, arena, visit_set, *ptr))),
            Value::Func(func) => match func {
                AnyFuncValue::Intrinsic(instance) => instance.fn_ty(&tcx.session),
                AnyFuncValue::Instance(instance) => tcx.instance_fn_ty(*instance).unwrap(),
            },
            Value::Scalar(scalar) => tcx.intern_ty(Ty::Scalar(match scalar {
                ValueScalar::Bool(_) => ScalarKind::Bool,
                ValueScalar::Char(_) => ScalarKind::Char,
                ValueScalar::U8(_) => ScalarKind::U8,
                ValueScalar::I8(_) => ScalarKind::I8,
                ValueScalar::U16(_) => ScalarKind::U16,
                ValueScalar::I16(_) => ScalarKind::I16,
                ValueScalar::U32(_) => ScalarKind::U32,
                ValueScalar::I32(_) => ScalarKind::I32,
                ValueScalar::U64(_) => ScalarKind::U64,
                ValueScalar::I64(_) => ScalarKind::I64,
                ValueScalar::U128(_) => ScalarKind::U128,
                ValueScalar::I128(_) => ScalarKind::I128,
                ValueScalar::F32(_) => ScalarKind::F32,
                ValueScalar::F64(_) => ScalarKind::F64,
                ValueScalar::USize(_) => ScalarKind::USize,
                ValueScalar::ISize(_) => ScalarKind::ISize,
            })),
            Value::Tuple(fields) => tcx.intern_ty(Ty::Tuple(
                tcx.intern_tys(
                    &fields
                        .iter()
                        .map(|ptr| inner(tcx, arena, visit_set, *ptr))
                        .collect::<Vec<_>>(),
                ),
            )),
            Value::Array(values) => tcx.intern_ty(Ty::Array(
                if let Some(ptr) = values.first() {
                    inner(tcx, arena, visit_set, *ptr)
                } else {
                    tcx.intern_ty(Ty::Never)
                },
                values.len(),
            )),
            Value::AdtAggregate { def, fields: _ }
            | Value::AdtVariant {
                def,
                variant: _,
                inner: _,
            } => tcx.intern_ty(Ty::Adt(*def)),
        }
    }

    inner(tcx, arena, &mut visit_set, ptr)
}
