//! An interner for cyclic data-structures. If you intern a given graph into the interner starting
//! at one node and intern the same graph starting at a different node, the interner will correctly
//! identify the isomorphism and return a handle into the former graph.
//!
//! This may make it seem as though this structure is solving the extremely computationally
//! expensive graph isomorphism problem but we're actually only solving a really limited form of the
//! problem where, notably, each edge has a name identifying it which is unique to the given source
//! node.
//!
//! Our overall process for actually interning a given graph looks as follows:
//!
//! 1. Break up our graph into strongly-connected components using the Tarjan SCC algorithm.
//! 2. Use the results from Tarjan to iterate through the components of our graph in reverse
//!    topological order.
//! 3. For each SCC, determine a small number of "key" nodes. This process should be deterministic
//!    for a given uniquely shaped SCC. For instance, this could be the nodes with the highest
//!    centralities and perhaps further break ties by selecting a node with the lowest unique
//!    degree.
//! 4. Hash the component graph from the perspective of each "key" node by representing cycles as
//!    indices into a virtual list of nodes we've already included in the hashing process. If a node
//!    points to another node outside of the SCC, use its unique interned ID. This is possible
//!    because we visit our SCCs in reverse topological order and thus must have already interned
//!    the node.
//! 5. Use that hash to look-up the interned SCC that matches our current SCC or insert the SCC if
//!    it hasn't yet been inserted.
//! 6. Repeat this process inductively until the entire graph has been interned.
//!

use std::{
    fmt,
    hash::{self, Hash, Hasher},
    ops::ControlFlow,
};

use ctx2d_utils::hash::{FxHashMap, FxHasher, hash_map};
use derive_where::derive_where;

// === IsoGraph === //

pub trait IsoGraph {
    type Node: fmt::Debug + Clone + hash::Hash + Eq;

    fn data_eq(&self, lhs: &Self::Node, rhs: &Self::Node) -> bool;

    fn data_hash(&self, node: &Self::Node, hasher: &mut impl hash::Hasher);

    fn successors<B>(
        &self,
        node: &Self::Node,
        f: impl FnMut(Self::Node) -> ControlFlow<B>,
    ) -> ControlFlow<B>;
}

// Adapted from: https://en.wikipedia.org/w/index.php?title=Tarjan%27s_strongly_connected_components_algorithm&oldid=1270884973#The_algorithm_in_pseudocode
fn tarjan<G, B>(
    graph: &G,
    starts: impl IntoIterator<Item = G::Node>,
    f: impl FnMut(&[G::Node]) -> ControlFlow<B>,
) -> ControlFlow<B>
where
    G: IsoGraph,
{
    #[derive_where(Default)]
    struct GraphAnnotator<G: IsoGraph, V: Default> {
        map: FxHashMap<G::Node, V>,
    }

    impl<G: IsoGraph, V: Default> GraphAnnotator<G, V> {
        fn get(&mut self, node: &G::Node) -> &mut V {
            self.map.entry(node.clone()).or_default()
        }
    }

    struct Tarjan<'a, G, F, B>
    where
        G: IsoGraph,
        F: FnMut(&[G::Node]) -> ControlFlow<B>,
    {
        graph: &'a G,
        index: u32,
        stack: Vec<G::Node>,
        anno: GraphAnnotator<G, Bookkeeping>,
        handler: F,
    }

    #[derive(Default)]
    struct Bookkeeping {
        index: Option<u32>,
        low_link: u32,
        on_stack: bool,
    }

    impl<G, F, B> Tarjan<'_, G, F, B>
    where
        G: IsoGraph,
        F: FnMut(&[G::Node]) -> ControlFlow<B>,
    {
        fn strong_connect(&mut self, v: G::Node) -> ControlFlow<B> {
            self.stack.push(v.clone());
            *self.anno.get(&v) = Bookkeeping {
                index: Some(self.index),
                low_link: self.index,
                on_stack: true,
            };
            self.index += 1;

            // Consider successors of `v`
            cbit::cbit!(for w in self.graph.successors(&v) {
                if self.anno.get(&w).index.is_none() {
                    // Successor `w` has not yet been visited; recurse on it
                    self.strong_connect(w.clone())?;

                    let w_low_link = self.anno.get(&w).low_link;
                    let v_low_link = &mut self.anno.get(&v).low_link;
                    *v_low_link = (*v_low_link).min(w_low_link);
                } else if self.anno.get(&w).on_stack {
                    // Successor `w` is in stack `S` and hence in the current SCC.
                    // If `w` is not on stack, then `(v, w)` is an edge pointing to an SCC
                    // already found and must be ignored.

                    let w_idx = self.anno.get(&w).index.unwrap();
                    let v_ll = &mut self.anno.get(&v).low_link;
                    *v_ll = (*v_ll).min(w_idx);
                }
            });

            // If `v` is a root node, pop the stack and generate an SCC.
            let v_anno = self.anno.get(&v);
            if v_anno.low_link == v_anno.index.unwrap() {
                let v_idx = self.stack.iter().rposition(|other| &v == other).unwrap();

                (self.handler)(&self.stack[v_idx..])?;

                for node in self.stack.drain(v_idx..) {
                    self.anno.get(&node).on_stack = false;
                }
            }

            ControlFlow::Continue(())
        }
    }

    let mut tj = Tarjan {
        graph,
        index: 0,
        stack: Vec::new(),
        anno: GraphAnnotator::default(),
        handler: f,
    };

    for start in starts {
        if tj.anno.get(&start).index.is_none() {
            tj.strong_connect(start)?;
        }
    }

    ControlFlow::Continue(())
}

#[derive_where(Debug; G::Node: fmt::Debug)]
struct IsoSubgraph<G: IsoGraph> {
    /// The internal nodes forming this sub-graph. The order of these nodes is deterministic w.r.t
    /// a given key and sub-graph structure.
    internal_nodes: Vec<G::Node>,

    /// Internal edges from source node indices to destination node indices.
    internal_edges: Vec<(u32, u32)>,

    /// Edges from internal nodes to pre-interned external nodes.
    external_edges: Vec<(u32, G::Node)>,
}

impl<G: IsoGraph> IsoSubgraph<G> {
    fn new(
        graph: &G,
        key: G::Node,
        mut map_external: impl FnMut(&G::Node) -> Option<G::Node>,
    ) -> Self {
        let mut internal_nodes = Vec::new();
        let mut internal_node_map = FxHashMap::default();

        let mut get_internal_id = |node: &G::Node| -> (u32, bool) {
            if let Some(&id) = internal_node_map.get(node) {
                return (id, false);
            }

            let id = internal_nodes.len() as u32;
            internal_nodes.push(node.clone());
            internal_node_map.insert(node.clone(), id);

            (id, true)
        };

        let mut internal_edges = Vec::new();
        let mut external_edges = Vec::new();

        let mut stack = Vec::new();

        debug_assert!(map_external(&key).is_none());
        stack.push((key.clone(), get_internal_id(&key).0));

        while let Some((curr, curr_idx)) = stack.pop() {
            cbit::cbit!(for successor in graph.successors(&curr) {
                if let Some(external) = map_external(&successor) {
                    external_edges.push((curr_idx, external));
                } else {
                    let (successor_idx, successor_is_new) = get_internal_id(&successor);

                    if successor_is_new {
                        stack.push((successor.clone(), successor_idx));
                    }

                    internal_edges.push((curr_idx, successor_idx));
                }
            });
        }

        Self {
            internal_nodes,
            internal_edges,
            external_edges,
        }
    }

    /// Compares two [`IsoScc`]s. If two graphs are comprised of distinct internal node instances
    /// but share the same internal edges with respect to isomorphic key nodes, this method will
    /// return `true`.
    fn eq(&self, graph: &G, other: &Self) -> bool {
        if self.internal_nodes.len() != other.internal_nodes.len() {
            return false;
        }

        if !self
            .internal_nodes
            .iter()
            .zip(&other.internal_nodes)
            .all(|(l, r)| graph.data_eq(l, r))
        {
            return false;
        }

        if self.internal_edges != other.internal_edges {
            return false;
        }

        if self.external_edges != other.external_edges {
            return false;
        }

        true
    }

    fn hash(&self, graph: &G, hasher: &mut impl hash::Hasher) {
        // TODO: Use `write_length_prefix` once it stabilizes.
        hasher.write_usize(self.internal_nodes.len());

        for elem in &self.internal_nodes {
            graph.data_hash(elem, hasher);
        }

        self.internal_edges.hash(hasher);
        self.external_edges.hash(hasher);
    }
}

// === IsoInterner === //

#[derive_where(Debug; G::Node: fmt::Debug)]
#[derive_where(Default)]
pub struct IsoInterner<G: IsoGraph> {
    roots: FxHashMap<(IsoSubgraph<G>, u64), ()>,
}

impl<G: IsoGraph> IsoInterner<G> {
    pub fn intern(&mut self, graph: &G, node: &G::Node) -> G::Node {
        let mut external_canonicals = FxHashMap::<G::Node, G::Node>::default();

        cbit::cbit!(for scc in tarjan(graph, [node.clone()]) {
            // Determine a small number of candidate "key" elements for this SCC based
            // deterministically off of component structure.
            let keys = {
                let mut keys = Vec::new();
                let mut min_hash = None;

                for candidate in scc {
                    let candidate_hash = {
                        let mut hasher = FxHasher::default();
                        graph.data_hash(candidate, &mut hasher);
                        hasher.finish()
                    };

                    if min_hash.is_none_or(|min_hash| candidate_hash < min_hash) {
                        keys.clear();
                        min_hash = Some(candidate_hash);
                    }

                    if min_hash.unwrap() == candidate_hash {
                        keys.push(candidate);
                    }
                }

                keys
            };

            // TODO: Further attempt to filter out key nodes by their structure (e.g. least-mode
            //  degree or in-between centrality).

            // Resolve (or create) the canonical sub-graph for this SCC.
            let tmp;
            let (src_sub_graph, canonical_sub_graph) = 'canonicalize: {
                // See if any of the keys lead to an interned version of this component.
                let mut first_sub_graph = None;

                for key in keys {
                    let sub_graph = IsoSubgraph::new(graph, key.clone(), |node| {
                        external_canonicals.get(node).cloned()
                    });

                    let hash = {
                        let mut hasher = FxHasher::default();
                        sub_graph.hash(graph, &mut hasher);
                        hasher.finish()
                    };

                    let canonical_sub_graph = self.roots.raw_entry().from_hash(
                        hash,
                        |&(ref other_sub_graph, other_hash)| {
                            hash == other_hash && sub_graph.eq(graph, other_sub_graph)
                        },
                    );

                    if let Some(((canonical_sub_graph, _), ())) = canonical_sub_graph {
                        tmp = sub_graph;
                        break 'canonicalize (&tmp, canonical_sub_graph);
                    }

                    _ = first_sub_graph.get_or_insert((sub_graph, hash));
                }

                // Otherwise, intern the SCC.
                let (sub_graph, hash) = first_sub_graph.unwrap();

                let hash_map::RawEntryMut::Vacant(entry) =
                    self.roots.raw_entry_mut().from_hash(hash, |_| false)
                else {
                    unreachable!()
                };

                let ((sub_graph, _), ()) =
                    entry.insert_with_hasher(hash, (sub_graph, hash), (), |&(_, hash)| hash);

                (sub_graph, sub_graph)
            };

            // Record the mapping between input nodes and their canonicals forms into
            // the `external_canonicals` map.
            for (src_node, canonical_node) in src_sub_graph
                .internal_nodes
                .iter()
                .zip(&canonical_sub_graph.internal_nodes)
            {
                external_canonicals.insert(src_node.clone(), canonical_node.clone());
            }
        });

        external_canonicals.remove(node).unwrap()
    }
}

// === Tests === //

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
    struct Cons(usize);

    #[derive(Default)]
    struct ConsGraph {
        nodes: Vec<ConsData>,
    }

    struct ConsData {
        label: &'static str,
        left: Option<Cons>,
        right: Option<Cons>,
    }

    impl ConsGraph {
        fn spawn(&mut self, label: &'static str) -> Cons {
            let handle = Cons(self.nodes.len());
            self.nodes.push(ConsData {
                label,
                left: None,
                right: None,
            });
            handle
        }

        pub fn set_left(&mut self, src: Cons, dst: Option<Cons>) {
            self.nodes[src.0].left = dst;
        }

        pub fn set_right(&mut self, src: Cons, dst: Option<Cons>) {
            self.nodes[src.0].right = dst;
        }
    }

    impl IsoGraph for ConsGraph {
        type Node = Cons;

        fn data_eq(&self, lhs: &Self::Node, rhs: &Self::Node) -> bool {
            self.nodes[lhs.0].label == self.nodes[rhs.0].label
        }

        fn data_hash(&self, node: &Self::Node, hasher: &mut impl hash::Hasher) {
            self.nodes[node.0].label.hash(hasher);
        }

        fn successors<B>(
            &self,
            node: &Self::Node,
            mut f: impl FnMut(Self::Node) -> ControlFlow<B>,
        ) -> ControlFlow<B> {
            if let Some(left) = self.nodes[node.0].left {
                f(left)?;
            }

            if let Some(right) = self.nodes[node.0].right {
                f(right)?;
            }

            ControlFlow::Continue(())
        }
    }

    #[test]
    fn simple() {
        let mut interner = IsoInterner::default();
        let mut graph = ConsGraph::default();

        // First alias
        let foo_1 = graph.spawn("foo");
        let bar_1 = graph.spawn("bar");

        graph.set_left(foo_1, Some(bar_1));
        graph.set_right(bar_1, Some(bar_1));

        assert!(interner.intern(&graph, &foo_1) == foo_1);
        assert!(interner.intern(&graph, &bar_1) == bar_1);

        // Second alias
        let foo_2 = graph.spawn("foo");
        let bar_2 = graph.spawn("bar");

        graph.set_left(foo_2, Some(bar_2));
        graph.set_right(bar_2, Some(bar_2));

        assert!(interner.intern(&graph, &foo_2) == foo_1);
        assert!(interner.intern(&graph, &bar_2) == bar_1);

        // Third alias
        let foo_3 = graph.spawn("foo");

        graph.set_left(foo_3, Some(bar_2));

        assert!(interner.intern(&graph, &foo_3) == foo_1);
    }
}
