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
    parse::ast::AdtKind,
    typeck::{
        analysis::TyCtxt,
        syntax::{Ty, Value, ValueKind, ValuePlace},
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

            // FIXME: This doesn't handle mixed arenas (e.g. value and intern) very well.
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

pub struct ValueArena {
    arena: Arena<Value>,
    tcx: TyCtxt,
}

impl fmt::Debug for ValueArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ValueArena").finish_non_exhaustive()
    }
}

impl ValueArena {
    pub fn new(tcx: TyCtxt) -> Self {
        Self {
            arena: Arena::new(),
            tcx,
        }
    }

    pub fn reserve_placeholder_value(&mut self, ty: Obj<Ty>) -> ValuePlace {
        ValuePlace(self.arena.insert(Value {
            ty,
            kind: ValueKind::Placeholder,
        }))
    }

    pub fn reserve_placeholder_value_and_ty(&mut self) -> ValuePlace {
        ValuePlace(self.arena.insert(Value {
            ty: self.tcx.intern_ty(Ty::Never),
            kind: ValueKind::Placeholder,
        }))
    }

    pub fn reserve(&mut self, ty: Obj<Ty>) -> ValuePlace {
        let s = self.tcx.session.clone();
        let s = &s;

        let root = self.reserve_placeholder_value(ty);
        let mut stack = vec![root];

        while let Some(place) = stack.pop() {
            self.mutate_unchecked(place).kind = match self.read(place).ty.r(s) {
                // Value-like
                Ty::MetaTy => ValueKind::MetaType(None),
                Ty::MetaFunc => ValueKind::MetaFunc(None),
                Ty::MetaArray(..) => ValueKind::MetaArray(Vec::new()),
                Ty::MetaString => ValueKind::MetaString(None),
                Ty::Pointer(..) => ValueKind::Pointer(None),
                Ty::Func(..) => ValueKind::Func(None),
                Ty::Scalar(..) => ValueKind::Scalar(None),
                Ty::MetaAny => ValueKind::MetaAny(None),
                Ty::Never => ValueKind::Placeholder,
                Ty::Error(_) => ValueKind::Placeholder,

                // Aggregate
                Ty::Tuple(fields) => {
                    let fields = fields
                        .r(s)
                        .iter()
                        .map(|&ty| {
                            let field = self.reserve_placeholder_value(ty);
                            stack.push(field);
                            field
                        })
                        .collect::<Vec<_>>();

                    ValueKind::Tuple(fields)
                }
                Ty::Array(ty, count) => {
                    let elems = (0..*count)
                        .map(|_| {
                            let elem = self.reserve_placeholder_value(*ty);
                            stack.push(elem);
                            elem
                        })
                        .collect::<Vec<_>>();

                    ValueKind::Array(elems)
                }
                Ty::Adt(adt) => match adt.adt.r(s).kind {
                    AdtKind::Mod => unreachable!(),
                    AdtKind::Union | AdtKind::Enum => ValueKind::AdtVariant(None),
                    AdtKind::Struct => {
                        let fields = adt
                            .adt
                            .r(s)
                            .fields
                            .iter()
                            .map(|field| {
                                let (Ok(ty) | Err(ty)) = self
                                    .tcx
                                    .eval_paramless_for_meta_ty(
                                        self.tcx.intern_fn_instance(field.ty, Some(adt.owner)),
                                    )
                                    .map_err(|e| self.tcx.intern_ty(Ty::Error(e)));

                                let field = self.reserve_placeholder_value(ty);
                                stack.push(field);
                                field
                            })
                            .collect::<Vec<_>>();

                        ValueKind::AdtAggregate(fields)
                    }
                },
            }
        }

        root
    }

    pub fn free(&mut self, ptr: ValuePlace) {
        let mut stack = vec![ptr];

        while let Some(ptr) = stack.pop() {
            let value = self.arena.remove(ptr.0).unwrap().kind;

            if matches!(value, ValueKind::Pointer(_)) {
                // (do not follow)
                continue;
            }

            cbit::cbit!(for pointee in follow_node_ref(&value) {
                stack.push(pointee);
            });
        }
    }

    pub fn read(&self, ptr: ValuePlace) -> &Value {
        &self.arena[ptr.0]
    }

    pub fn mutate_unchecked(&mut self, ptr: ValuePlace) -> &mut Value {
        &mut self.arena[ptr.0]
    }

    #[expect(clippy::diverging_sub_expression)]
    pub fn write_terminal(&mut self, ptr: ValuePlace, value: ValueKind) {
        let place = self.mutate_unchecked(ptr);

        // Type should not change.
        debug_assert_eq!(
            mem::discriminant(&place.kind),
            mem::discriminant(&value),
            "expected type was: {:#?}\ngot value of kind: {:#?}",
            place.ty,
            value
        );

        // Value should be terminal.
        #[cfg(debug_assertions)]
        if !matches!(value, ValueKind::Pointer(_)) {
            cbit::cbit!(for _ in follow_node_ref(&value) {
                panic!("value is not terminal");
            });
        }

        self.mutate_unchecked(ptr).kind = value;
    }

    pub fn alloc_terminal(&mut self, ty: Obj<Ty>, value: ValueKind) -> ValuePlace {
        let place = self.reserve(ty);
        self.write_terminal(place, value);
        place
    }

    pub fn assign(&mut self, from: ValuePlace, to: ValuePlace) {
        let mut stack = vec![(from, to)];

        while let Some((from_handle, to_handle)) = stack.pop() {
            let (Some(from), Some(to)) = self.arena.get2_mut(from_handle.0, to_handle.0) else {
                unreachable!()
            };

            debug_assert_eq!(from.ty, to.ty);

            match_pair!((&from.kind, &mut to.kind) => {
                // External references are interned so terminal copies are okay.
                (ValueKind::MetaType(from), ValueKind::MetaType(to)) => {
                    *to = *from;
                }
                (ValueKind::MetaFunc(from), ValueKind::MetaFunc(to)) => {
                    *to = *from;
                }
                (ValueKind::MetaString(from), ValueKind::MetaString(to)) => {
                    *to = *from;
                }
                (ValueKind::Func(from), ValueKind::Func(to)) => {
                    *to = *from;
                }

                // Pointers are raw pointers. Just copy the address.
                (ValueKind::Pointer(from), ValueKind::Pointer(to)) => {
                    *to = *from;
                }

                // These are naturally terminal.
                (ValueKind::Scalar(from), ValueKind::Scalar(to)) => {
                    *to = *from;
                }

                // Here are the simple aggregate cases.
                (ValueKind::Tuple(from), ValueKind::Tuple(to))
                | (ValueKind::Array(from), ValueKind::Array(to)) => {
                    for (&from, &mut to) in from.iter().zip(to) {
                        stack.push((from, to));
                    }
                }
                (ValueKind::AdtAggregate(from), ValueKind::AdtAggregate(to)) => {
                    for (&from, &mut to) in from.iter().zip(to) {
                        stack.push((from, to));
                    }
                }

                // And here are the awful variant cases.
                (ValueKind::MetaArray(from), ValueKind::MetaArray(to)) => {
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

                        let ValueKind::MetaArray(to) = &mut self.arena[to_handle.0].kind else {
                            unreachable!()
                        };

                        *to = new_to;
                    }
                }
                (ValueKind::MetaAny(from), ValueKind::MetaAny(to)) => {
                    todo!()
                }
                (
                    ValueKind::AdtVariant(from),
                    ValueKind::AdtVariant(to),
                ) => {
                    match (*from, *to) {
                        (Some((from_variant, from_pointee)), Some((to_variant, to_pointee))) => {
                            if from_variant == to_variant  {
                                stack.push((from_pointee, to_pointee));
                            } else {
                                self.free(to_pointee);

                                let new_pointee = self.copy(from_pointee, CopyDepth::Shallow);

                                let (ValueKind::AdtVariant(Some((to_variant, to_pointee)))) = &mut self.mutate_unchecked(to_handle).kind else {
                                    unreachable!();
                                };

                                *to_variant = from_variant;
                                *to_pointee = new_pointee;
                            }
                        }
                        (None, Some((_, to_pointee))) => {
                            *to = None;
                            self.free(to_pointee);
                        }
                        (Some((from_variant, from_pointee)), None) => {
                            let new_pointee = self.copy(from_pointee, CopyDepth::Shallow);

                            let (ValueKind::AdtVariant(Some((to_variant, to_pointee)))) = &mut self.mutate_unchecked(to_handle).kind else {
                                unreachable!();
                            };

                            *to_variant = from_variant;
                            *to_pointee = new_pointee;
                        }
                        (None, None) => {
                            // (no-op)
                        }
                    }
                }
                (ValueKind::Placeholder, ValueKind::Placeholder) => {
                    // (no-op)
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
        let to_root = self.reserve_placeholder_value_and_ty();
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

            if depth.is_deep() || !matches!(value.kind, ValueKind::Pointer(_)) {
                cbit::cbit!(for edge in follow_node_mut(&mut value.kind) {
                    let edge_from = *edge;

                    *edge = *mapping.entry(edge_from).or_insert_with(|| {
                        let edge_to = self.reserve_placeholder_value_and_ty();
                        stack.push((edge_from, edge_to));
                        edge_to
                    });
                });
            }

            *self.mutate_unchecked(to) = value;
        }
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
        follow_node_ref(&self.read(node).kind, |node| {
            f(LabelledSuccessor::Node(node))
        })
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

                    cbit::cbit!(for out_ref in follow_node_mut(&mut value.kind) {
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
    value.ty.hash(state);

    mem::discriminant(&value.kind).hash(state);

    match &value.kind {
        ValueKind::MetaType(ty) => ty.hash(state),
        ValueKind::MetaFunc(func) => {
            func.hash(state);
        }
        ValueKind::MetaArray(list) => {
            list.len().hash(state);
        }
        ValueKind::MetaString(sym) => {
            sym.hash(state);
        }
        ValueKind::Pointer(_) => {
            // (nothing)
        }
        ValueKind::MetaAny(_) => {
            // (nothing)
        }
        ValueKind::Func(func) => {
            func.hash(state);
        }
        ValueKind::Scalar(scalar) => {
            scalar.hash(state);
        }
        ValueKind::Tuple(list) => {
            state.write_usize(list.len());
        }
        ValueKind::Array(list) => {
            state.write_usize(list.len());
        }
        ValueKind::AdtAggregate(_fields) => {
            // (nothing)
        }
        ValueKind::AdtVariant(inner) => {
            // Hash variant
            inner.map(|v| v.0).hash(state);
        }
        ValueKind::Placeholder => {
            // (nothing)
        }
    }
}

pub fn eq_value_terminal(lhs: &Value, rhs: &Value) -> bool {
    if lhs.ty != rhs.ty {
        return false;
    }

    match_pair!((&lhs.kind, &rhs.kind) => {
        (ValueKind::MetaType(lhs), ValueKind::MetaType(rhs)) => {
            lhs == rhs
        }
        (ValueKind::MetaFunc(lhs), ValueKind::MetaFunc(rhs)) => {
            lhs == rhs
        }
        (ValueKind::MetaArray(lhs), ValueKind::MetaArray(rhs)) => {
            lhs.len() == rhs.len()
        }
        (ValueKind::MetaString(lhs), ValueKind::MetaString(rhs)) => {
            lhs == rhs
        }
        (ValueKind::MetaAny(_lhs_heap), ValueKind::MetaAny(_rhs_heap)) => {
            true
        }
        (ValueKind::Pointer(_lhs_heap), ValueKind::Pointer(_rhs_heap)) => {
            true
        }
        (ValueKind::Func(lhs), ValueKind::Func(rhs)) => {
            lhs == rhs
        }
        (ValueKind::Scalar(lhs), ValueKind::Scalar(rhs)) => {
            lhs == rhs
        }
        (ValueKind::Tuple(lhs), ValueKind::Tuple(rhs)) => {
            // (shape discriminated by type already)
            true
        }
        (ValueKind::Array(lhs), ValueKind::Array(rhs)) => {
            // (shape discriminated by type already)
            true
        }
        (
            ValueKind::AdtAggregate(_),
            ValueKind::AdtAggregate(_),
        ) => {
            // (shape discriminated by type already)
            true
        }
        (
            ValueKind::AdtVariant(lhs),
            ValueKind::AdtVariant(rhs),
        ) => {
            match (lhs, rhs) {
                (Some((lhs_variant, _)), Some((rhs_variant, _))) =>lhs_variant == rhs_variant,
                (None, None) => true,
                _ => false
            }
        }
        (ValueKind::Placeholder, ValueKind::Placeholder) => {
            true
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
            ValueKind::Tuple(values)
            | ValueKind::Array(values)
            | ValueKind::MetaArray(values)
            | ValueKind::AdtAggregate(values) => {
                for value in values {
                    f(value)?;
                }
            }
            ValueKind::Pointer(Some(value))
            | ValueKind::MetaAny(Some(value))
            | ValueKind::AdtVariant(Some((_, value))) => {
                f(value)?;
            }
            ValueKind::Pointer(None) | ValueKind::MetaAny(None) | ValueKind::AdtVariant(None) => {
                // (nothing to follow, uninit)
            }
            ValueKind::MetaType(_)
            | ValueKind::MetaFunc(_)
            | ValueKind::MetaString(_)
            | ValueKind::Func(_)
            | ValueKind::Scalar(_)
            | ValueKind::Placeholder => {
                // (nothing to follow)
            }
        }

        ControlFlow::Continue(())
    }};
}

pub fn follow_node_ref<B>(
    value: &ValueKind,
    mut f: impl FnMut(ValuePlace) -> ControlFlow<B>,
) -> ControlFlow<B> {
    follow_node!(value, |v: &ValuePlace| f(*v))
}

pub fn follow_node_mut<B>(
    value: &mut ValueKind,
    f: impl FnMut(&mut ValuePlace) -> ControlFlow<B>,
) -> ControlFlow<B> {
    follow_node!(value, f)
}
