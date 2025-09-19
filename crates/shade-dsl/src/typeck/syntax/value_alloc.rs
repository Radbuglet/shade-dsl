use std::{
    fmt,
    hash::{self, Hash, Hasher},
    mem,
    ops::ControlFlow,
};

use ctx2d_utils::hash::{FxHashMap, FxHasher, hash_map};
use thunderdome::Arena;

use crate::{
    base::analysis::{
        IsoSccPortrait, LabelledDiGraph, LabelledSuccessor, SccNode, portrait_keys, tarjan,
    },
    typeck::syntax::{AdtValue, SemiFuncInstance, Value, ValuePtr},
};

// === ValueArena === //

#[derive(Default)]
pub struct ValueArena {
    arena: Arena<Value>,
}

impl fmt::Debug for ValueArena {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ValueArena").finish_non_exhaustive()
    }
}

impl ValueArena {
    pub fn alloc(&mut self, value: Value) -> ValuePtr {
        ValuePtr(self.arena.insert(value))
    }

    pub fn free(&mut self, ptr: ValuePtr) {
        self.arena.remove(ptr.0).unwrap();
    }

    pub fn read(&self, ptr: ValuePtr) -> &Value {
        &self.arena[ptr.0]
    }

    pub fn write(&mut self, ptr: ValuePtr) -> &mut Value {
        &mut self.arena[ptr.0]
    }

    pub fn duplicate(&mut self, target: ValuePtr) -> ValuePtr {
        self.duplicate_ext(None, target)
    }

    pub fn duplicate_ext(
        &mut self,
        from_arena: Option<&ValueArena>,
        from_root: ValuePtr,
    ) -> ValuePtr {
        let to_root = self.alloc(Value::Placeholder);

        let mut stack = vec![(from_root, to_root)];
        let mut mapping = FxHashMap::from_iter([(from_root, to_root)]);

        while let Some((from, to)) = stack.pop() {
            let mut value = from_arena.unwrap_or(self).read(from).clone();

            cbit::cbit!(for edge in follow_node_mut(&mut value) {
                let edge_from = *edge;
                *edge = *mapping.entry(edge_from).or_insert_with(|| {
                    let edge_to = self.alloc(Value::Placeholder);
                    stack.push((edge_from, edge_to));
                    edge_to
                });
            });

            *self.write(to) = value;
        }

        to_root
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

// === ValueInterner === //

#[derive(Default)]
pub struct ValueInterner {
    value_arena: ValueArena,
    value_interns: FxHashMap<InternEntry, ()>,
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
    pub fn intern(&mut self, user_graph: &ValueArena, user_root: ValuePtr) -> ValuePtr {
        let mut resolved_canonicals = FxHashMap::<ValuePtr, ValuePtr>::default();

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
                        self.value_interns
                            .raw_entry()
                            .from_hash(portrait_hash, |entry| {
                                if entry.hash != portrait_hash {
                                    return false;
                                }

                                entry.portrait.eq(&portrait, |&lhs, &rhs| {
                                    eq_value(
                                        self.value_arena.read(ValuePtr(lhs.0)),
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
                    let interned_value = self.value_arena.alloc(Value::Placeholder);
                    resolved_canonicals.insert(user_value, interned_value);
                }

                for &user in &user_portrait.internal_nodes {
                    let mut value = user_graph.read(user).clone();

                    cbit::cbit!(for out_ref in follow_node_mut(&mut value) {
                        *out_ref = resolved_canonicals[out_ref];
                    });

                    *self.value_arena.write(resolved_canonicals[&user]) = value;
                }

                let hash_map::RawEntryMut::Vacant(entry) = self
                    .value_interns
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

    pub fn value_arena(&self) -> &ValueArena {
        &self.value_arena
    }
}

// === Value Introspection === //

fn hash_value(value: &Value, state: &mut impl hash::Hasher) {
    mem::discriminant(value).hash(state);

    match value {
        Value::MetaType(ty) => ty.hash(state),
        Value::MetaFunc(SemiFuncInstance {
            func,
            parent,
            generics: _,
        }) => {
            func.hash(state);
            parent.hash(state);
        }
        Value::MetaList(list) => {
            state.write_usize(list.len());
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
        Value::Adt(instance, AdtValue::Composite(_)) => {
            state.write_u8(0);
            instance.hash(state);
        }
        Value::Adt(instance, AdtValue::Variant(discriminant, _)) => {
            state.write_u8(1);
            instance.hash(state);
            state.write_u32(*discriminant);
        }
        Value::Placeholder => unreachable!(),
    }
}

fn eq_value(lhs: &Value, rhs: &Value) -> bool {
    match (lhs, rhs) {
        (Value::MetaType(lhs), Value::MetaType(rhs)) => lhs == rhs,
        (
            Value::MetaFunc(SemiFuncInstance {
                func: lhs_func,
                parent: lhs_parent,
                generics: _,
            }),
            Value::MetaFunc(SemiFuncInstance {
                func: rhs_func,
                parent: rhs_parent,
                generics: _,
            }),
        ) => lhs_func == rhs_func && lhs_parent == rhs_parent,
        (Value::MetaList(lhs), Value::MetaList(rhs)) => lhs.len() == rhs.len(),
        (Value::Pointer(_), Value::Pointer(_)) => true,
        (Value::Func(lhs), Value::Func(rhs)) => lhs == rhs,
        (Value::Scalar(lhs), Value::Scalar(rhs)) => lhs == rhs,
        (Value::Tuple(lhs), Value::Tuple(rhs)) => lhs.len() == rhs.len(),
        (Value::Array(lhs), Value::Array(rhs)) => lhs.len() == rhs.len(),
        (
            Value::Adt(lhs_inst, AdtValue::Composite(_)),
            Value::Adt(rhs_inst, AdtValue::Composite(_)),
        ) => lhs_inst == rhs_inst,
        (
            Value::Adt(lhs_inst, AdtValue::Variant(lhs_variant, _)),
            Value::Adt(rhs_inst, AdtValue::Variant(rhs_variant, _)),
        ) => lhs_inst == rhs_inst && lhs_variant == rhs_variant,
        (Value::Placeholder, _) | (_, Value::Placeholder) => unreachable!(),
        _ => false,
    }
}

fn follow_node_ref<B>(
    value: &Value,
    mut f: impl FnMut(ValuePtr) -> ControlFlow<B>,
) -> ControlFlow<B> {
    match value {
        Value::MetaType(_) => {}
        Value::MetaFunc(SemiFuncInstance {
            func: _,
            parent: _,
            generics: values,
        })
        | Value::MetaList(values)
        | Value::Tuple(values)
        | Value::Array(values)
        | Value::Adt(_, AdtValue::Composite(values)) => {
            for &value in values {
                f(value)?;
            }
        }
        Value::Pointer(value) | Value::Adt(_, AdtValue::Variant(_, value)) => {
            f(*value)?;
        }
        Value::Func(_) => todo!(),
        Value::Scalar(_) => todo!(),
        Value::Placeholder => unreachable!(),
    }

    ControlFlow::Continue(())
}

fn follow_node_mut<B>(
    value: &mut Value,
    mut f: impl FnMut(&mut ValuePtr) -> ControlFlow<B>,
) -> ControlFlow<B> {
    match value {
        Value::MetaType(_) => {}
        Value::MetaFunc(SemiFuncInstance {
            func: _,
            parent: _,
            generics: values,
        })
        | Value::MetaList(values)
        | Value::Tuple(values)
        | Value::Array(values)
        | Value::Adt(_, AdtValue::Composite(values)) => {
            for value in values {
                f(value)?;
            }
        }
        Value::Pointer(value) | Value::Adt(_, AdtValue::Variant(_, value)) => {
            f(value)?;
        }
        Value::Func(_) => todo!(),
        Value::Scalar(_) => todo!(),
        Value::Placeholder => unreachable!(),
    }

    ControlFlow::Continue(())
}
