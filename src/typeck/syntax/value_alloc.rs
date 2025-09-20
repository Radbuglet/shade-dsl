use std::{
    cell::RefCell,
    fmt,
    hash::{self, Hash, Hasher},
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
    typeck::syntax::{AdtValue, SemiFuncInstance, Value, ValueKind, ValuePtr},
    utils::hash::{FxHashMap, FxHasher, hash_map},
};

// === ValueArenaLike === //

pub trait ValueArenaLike: Sized {
    fn read(&self, ptr: ValuePtr) -> &Value;
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
    pub fn reserve(&mut self) -> ValuePtr {
        ValuePtr(self.arena.insert(None))
    }

    pub fn init(&mut self, ptr: ValuePtr, value: Value) {
        assert!(self.arena[ptr.0].is_some());
        self.arena[ptr.0] = Some(value);
    }

    pub fn alloc(&mut self, value: Value) -> ValuePtr {
        ValuePtr(self.arena.insert(Some(value)))
    }

    pub fn free(&mut self, ptr: ValuePtr) {
        self.arena.remove(ptr.0).unwrap();
    }

    pub fn read(&self, ptr: ValuePtr) -> &Value {
        self.arena[ptr.0].as_ref().unwrap()
    }

    pub fn write(&mut self, ptr: ValuePtr) -> &mut Value {
        self.arena[ptr.0].as_mut().unwrap()
    }

    pub fn duplicate(&mut self, target: ValuePtr) -> ValuePtr {
        self.duplicate_from(Option::<&Self>::None, target)
    }

    pub fn duplicate_from(
        &mut self,
        from_arena: Option<&impl ValueArenaLike>,
        from_root: ValuePtr,
    ) -> ValuePtr {
        let to_root = self.reserve();

        let mut stack = vec![(from_root, to_root)];
        let mut mapping = FxHashMap::from_iter([(from_root, to_root)]);

        while let Some((from, to)) = stack.pop() {
            let mut value = from_arena
                .map_or_else(|| self.read(from), |other| other.read(from))
                .clone();

            cbit::cbit!(for edge in follow_node_mut(&mut value) {
                let edge_from = *edge;
                *edge = *mapping.entry(edge_from).or_insert_with(|| {
                    let edge_to = self.reserve();
                    stack.push((edge_from, edge_to));
                    edge_to
                });
            });

            *self.write(to) = value;
        }

        to_root
    }
}

impl ValueArenaLike for ValueArena {
    fn read(&self, ptr: ValuePtr) -> &Value {
        self.read(ptr)
    }
}

impl LabelledDiGraph for ValueArena {
    type Node = ValuePtr;
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
    pub fn reserve(&self) -> ValuePtr {
        let value = self.bump.alloc(LateInit::uninit());
        ValuePtr(self.arena.borrow_mut().insert(NonNull::from(value)))
    }

    pub fn fetch(&self, ptr: ValuePtr) -> &LateInit<Value> {
        unsafe { self.arena.borrow()[ptr.0].as_ref() }
    }

    pub fn init(&self, ptr: ValuePtr, value: Value) {
        LateInit::init(self.fetch(ptr), value);
    }

    pub fn alloc(&self, value: Value) -> ValuePtr {
        let ptr = self.reserve();
        self.init(ptr, value);
        ptr
    }

    pub fn read(&self, ptr: ValuePtr) -> &Value {
        self.fetch(ptr)
    }
}

impl ValueArenaLike for ValueBump {
    fn read(&self, ptr: ValuePtr) -> &Value {
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
    portrait: IsoSccPortrait<ValuePtr, ValuePtr, ()>,
    hash: u64,
}

impl fmt::Debug for ValueInterner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ValueInterner").finish_non_exhaustive()
    }
}

impl ValueInterner {
    pub fn intern(&self, user_graph: &ValueArena, user_root: ValuePtr) -> ValuePtr {
        let mut resolved_canonicals = FxHashMap::<ValuePtr, ValuePtr>::default();
        let mut value_interns = self.value_interns.borrow_mut();

        cbit::cbit!(for scc in tarjan(user_graph, [user_root]) {
            let keys = portrait_keys(user_graph, scc, |&node| {
                let mut hasher = FxHasher::default();
                hash_value(user_graph.read(node), &mut hasher);
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
                            |&node, hasher| hash_value(user_graph.read(node), hasher),
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
                                eq_value(self.read(ValuePtr(lhs.0)), user_graph.read(rhs))
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

    pub fn read(&self, value: ValuePtr) -> &Value {
        self.value_arena.read(value)
    }

    pub fn arena(&self) -> &ValueBump {
        &self.value_arena
    }
}

// === Value Introspection === //

fn hash_value(value: &Value, state: &mut impl hash::Hasher) {
    value.ty.hash(state);

    mem::discriminant(&value.kind).hash(state);

    match &value.kind {
        ValueKind::MetaType(ty) => ty.hash(state),
        ValueKind::MetaFunc(SemiFuncInstance {
            func,
            parent,
            generics: _,
        }) => {
            func.hash(state);
            parent.hash(state);
        }
        ValueKind::MetaList(list) => {
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
        ValueKind::Adt(instance, AdtValue::Composite(_)) => {
            state.write_u8(0);
            instance.hash(state);
        }
        ValueKind::Adt(instance, AdtValue::Variant(discriminant, _)) => {
            state.write_u8(1);
            instance.hash(state);
            state.write_u32(*discriminant);
        }
    }
}

fn eq_value(lhs: &Value, rhs: &Value) -> bool {
    if lhs.ty != rhs.ty {
        return false;
    }

    match (&lhs.kind, &rhs.kind) {
        (ValueKind::MetaType(lhs), ValueKind::MetaType(rhs)) => lhs == rhs,
        (
            ValueKind::MetaFunc(SemiFuncInstance {
                func: lhs_func,
                parent: lhs_parent,
                generics: _,
            }),
            ValueKind::MetaFunc(SemiFuncInstance {
                func: rhs_func,
                parent: rhs_parent,
                generics: _,
            }),
        ) => lhs_func == rhs_func && lhs_parent == rhs_parent,
        (ValueKind::MetaList(lhs), ValueKind::MetaList(rhs)) => lhs.len() == rhs.len(),
        (ValueKind::Pointer(_), ValueKind::Pointer(_)) => true,
        (ValueKind::Func(lhs), ValueKind::Func(rhs)) => lhs == rhs,
        (ValueKind::Scalar(lhs), ValueKind::Scalar(rhs)) => lhs == rhs,
        (ValueKind::Tuple(lhs), ValueKind::Tuple(rhs)) => lhs.len() == rhs.len(),
        (ValueKind::Array(lhs), ValueKind::Array(rhs)) => lhs.len() == rhs.len(),
        (
            ValueKind::Adt(lhs_inst, AdtValue::Composite(_)),
            ValueKind::Adt(rhs_inst, AdtValue::Composite(_)),
        ) => lhs_inst == rhs_inst,
        (
            ValueKind::Adt(lhs_inst, AdtValue::Variant(lhs_variant, _)),
            ValueKind::Adt(rhs_inst, AdtValue::Variant(rhs_variant, _)),
        ) => lhs_inst == rhs_inst && lhs_variant == rhs_variant,
        _ => false,
    }
}

fn follow_node_ref<B>(
    value: &Value,
    mut f: impl FnMut(ValuePtr) -> ControlFlow<B>,
) -> ControlFlow<B> {
    match &value.kind {
        ValueKind::MetaType(_) => {}
        ValueKind::MetaFunc(SemiFuncInstance {
            func: _,
            parent: _,
            generics: values,
        })
        | ValueKind::MetaList(values)
        | ValueKind::Tuple(values)
        | ValueKind::Array(values)
        | ValueKind::Adt(_, AdtValue::Composite(values)) => {
            for &value in values {
                f(value)?;
            }
        }
        ValueKind::Pointer(value) | ValueKind::Adt(_, AdtValue::Variant(_, value)) => {
            f(*value)?;
        }
        ValueKind::Func(_) | ValueKind::Scalar(_) => {}
    }

    ControlFlow::Continue(())
}

fn follow_node_mut<B>(
    value: &mut Value,
    mut f: impl FnMut(&mut ValuePtr) -> ControlFlow<B>,
) -> ControlFlow<B> {
    match &mut value.kind {
        ValueKind::MetaType(_) => {}
        ValueKind::MetaFunc(SemiFuncInstance {
            func: _,
            parent: _,
            generics: values,
        })
        | ValueKind::MetaList(values)
        | ValueKind::Tuple(values)
        | ValueKind::Array(values)
        | ValueKind::Adt(_, AdtValue::Composite(values)) => {
            for value in values {
                f(value)?;
            }
        }
        ValueKind::Pointer(value) | ValueKind::Adt(_, AdtValue::Variant(_, value)) => {
            f(value)?;
        }
        ValueKind::Func(_) | ValueKind::Scalar(_) => {}
    }

    ControlFlow::Continue(())
}
