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
        arena::LateInit,
    },
    typeck::syntax::{SemiFuncInstance, Value, ValueKind, ValuePlace},
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
        self.0.fmt(f)?;

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

            if matches!(value.kind, ValueKind::Pointer(_)) {
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

    pub fn write(&mut self, ptr: ValuePlace, value: Value) {
        self.arena[ptr.0] = Some(value);
    }

    pub fn mutate(&mut self, ptr: ValuePlace) -> &mut Value {
        self.arena[ptr.0].as_mut().expect("place never initialized")
    }

    pub fn assign(&mut self, from: ValuePlace, to: ValuePlace) {
        let mut stack = vec![(from, to)];

        while let Some((from_handle, to_handle)) = stack.pop() {
            let (Some(Some(from)), Some(to)) = self.arena.get2_mut(from_handle.0, to_handle.0)
            else {
                unreachable!()
            };

            let from = &*from;

            let Some(to) = to else {
                self.copy_from_with_initial_root(
                    None::<&ValueArena>,
                    from_handle,
                    to_handle,
                    CopyDepth::Shallow,
                );
                continue;
            };

            debug_assert_eq!(from.ty, to.ty);

            match (&from.kind, &mut to.kind) {
                // External references are interned so terminal copies are okay.
                (ValueKind::MetaType(from), ValueKind::MetaType(to)) => {
                    *to = *from;
                }
                (ValueKind::MetaFunc(from), ValueKind::MetaFunc(to)) => {
                    *to = from.clone();
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

                // Here are the actually interesting cases.
                (ValueKind::Tuple(from), ValueKind::Tuple(to))
                | (ValueKind::Array(from), ValueKind::Array(to))
                | (ValueKind::AdtAggregate(from), ValueKind::AdtAggregate(to)) => {
                    for (&from, &mut to) in from.iter().zip(to) {
                        stack.push((from, to));
                    }
                }
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

                        let ValueKind::MetaArray(to) =
                            &mut self.arena[to_handle.0].as_mut().unwrap().kind
                        else {
                            unreachable!()
                        };

                        *to = new_to;
                    }
                }
                (
                    ValueKind::AdtVariant(from_variant, from_pointee),
                    ValueKind::AdtVariant(to_variant, to_pointee),
                ) => {
                    if from_variant == to_variant {
                        stack.push((*from_pointee, *to_pointee));
                    } else {
                        *to_variant = *from_variant;

                        let from_pointee = *from_pointee;
                        let old_to_pointee = *to_pointee;

                        self.free(old_to_pointee);

                        let new_to_pointee = self.copy(from_pointee, CopyDepth::Shallow);

                        let ValueKind::Pointer(to_pointee) =
                            &mut self.arena[to_handle.0].as_mut().unwrap().kind
                        else {
                            unreachable!()
                        };

                        *to_pointee = new_to_pointee;
                    }
                }

                _ => unreachable!(),
            }
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

            if depth.is_deep() || !matches!(value.kind, ValueKind::Pointer(_)) {
                cbit::cbit!(for edge in follow_node_mut(&mut value) {
                    let edge_from = *edge;
                    *edge = *mapping.entry(edge_from).or_insert_with(|| {
                        let edge_to = self.reserve();
                        stack.push((edge_from, edge_to));
                        edge_to
                    });
                });
            }

            *self.mutate(to) = value;
        }
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
            let mut user_portrait = None;
            let canonical_portrait = keys
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
                        Some(&canonical_portrait.portrait)
                    } else {
                        user_portrait = Some((portrait, portrait_hash));
                        None
                    }
                })
                .next();

            // If it doesn't, create the canonical graph.
            if let Some(canonical_sub_graph) = canonical_portrait {
                // Record the mapping between input nodes and their canonicals forms into
                // the `resolved_canonicals` map.
                for (&user_node, &canonical_node) in canonical_sub_graph
                    .internal_nodes
                    .iter()
                    .zip(&canonical_sub_graph.internal_nodes)
                {
                    resolved_canonicals.insert(user_node, canonical_node);
                }
            } else {
                let (user_portrait, user_portrait_hash) = user_portrait.unwrap();

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

fn hash_value_terminal(value: &Value, state: &mut impl hash::Hasher) {
    value.ty.hash(state);

    mem::discriminant(&value.kind).hash(state);

    match &value.kind {
        ValueKind::MetaType(ty) => ty.hash(state),
        ValueKind::MetaFunc(func) => {
            func.hash(state);
        }
        ValueKind::MetaArray(list) => {
            state.write_usize(list.len());
        }
        ValueKind::Pointer(_) => {
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
        ValueKind::AdtAggregate(_) => {
            // (nothing)
        }
        ValueKind::AdtVariant(id, _) => {
            id.hash(state);
        }
    }
}

fn eq_value_terminal(lhs: &Value, rhs: &Value) -> bool {
    if lhs.ty != rhs.ty {
        return false;
    }

    match (&lhs.kind, &rhs.kind) {
        (ValueKind::MetaType(lhs), ValueKind::MetaType(rhs)) => lhs == rhs,
        (ValueKind::MetaFunc(lhs), ValueKind::MetaFunc(rhs)) => lhs == rhs,
        (ValueKind::MetaArray(lhs), ValueKind::MetaArray(rhs)) => lhs.len() == rhs.len(),
        (ValueKind::Pointer(_lhs_heap), ValueKind::Pointer(_rhs_heap)) => true,
        (ValueKind::Func(lhs), ValueKind::Func(rhs)) => lhs == rhs,
        (ValueKind::Scalar(lhs), ValueKind::Scalar(rhs)) => lhs == rhs,
        (ValueKind::Tuple(lhs), ValueKind::Tuple(rhs)) => lhs.len() == rhs.len(),
        (ValueKind::Array(lhs), ValueKind::Array(rhs)) => lhs.len() == rhs.len(),
        (ValueKind::AdtAggregate(_), ValueKind::AdtAggregate(_)) => true,
        (ValueKind::AdtVariant(lhs, _), ValueKind::AdtVariant(rhs, _)) => lhs == rhs,
        _ => false,
    }
}

macro_rules! follow_node {
    ($value:expr, $f:expr) => {{
        let mut f = $f;

        match $value {
            ValueKind::MetaFunc(SemiFuncInstance {
                func: _,
                parent: _,
                generics: values,
            })
            | ValueKind::Tuple(values)
            | ValueKind::Array(values)
            | ValueKind::MetaArray(values)
            | ValueKind::AdtAggregate(values) => {
                for value in values {
                    f(value)?;
                }
            }
            ValueKind::Pointer(value) | ValueKind::AdtVariant(_, value) => {
                f(value)?;
            }
            ValueKind::MetaType(_) | ValueKind::Func(_) | ValueKind::Scalar(_) => {
                // (nothing to follow)
            }
        }

        ControlFlow::Continue(())
    }};
}

fn follow_node_ref<B>(
    value: &Value,
    mut f: impl FnMut(ValuePlace) -> ControlFlow<B>,
) -> ControlFlow<B> {
    follow_node!(&value.kind, |v: &ValuePlace| f(*v))
}

fn follow_node_mut<B>(
    value: &mut Value,
    f: impl FnMut(&mut ValuePlace) -> ControlFlow<B>,
) -> ControlFlow<B> {
    follow_node!(&mut value.kind, f)
}
