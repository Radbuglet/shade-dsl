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
    hash::{self, Hash as _},
    ops::ControlFlow,
};

use ctx2d_utils::hash::FxHashMap;
use derive_where::derive_where;

// === LabelledDiGraph === //

/// A directed graph where each edge is implicitly "labelled" by its order in the successor list.
///
/// For example, a binary search tree would be a labelled directed graph because the left and right
/// edges are labelled as such. As a much more relevant example to our application, the graph formed
/// by ADTs referencing other values is a labelled directed graph because each other value an ADT
/// references is labelled by the field in which that value is contained.
///
/// Sometimes, a successor may be missing—e.g. a binary-search-tree node may be missing its left
/// child node. If we were to simply skip that successor during enumeration, however, the right edge
/// could be confused for the left edge. To prevent that, users must emit one or more placeholder
/// nodes to help disambiguate these cases.
pub trait LabelledDiGraph {
    type Node: Clone + hash::Hash + Eq;
    type Placeholder: Clone + hash::Hash + Eq;

    fn successors<B>(
        &self,
        node: &Self::Node,
        f: impl FnMut(LabelledSuccessor<Self::Node, Self::Placeholder>) -> ControlFlow<B>,
    ) -> ControlFlow<B>;
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum LabelledSuccessor<N, P> {
    Node(N),
    Placeholder(P),
}

// === Tarjan === //

// Adapted from: https://en.wikipedia.org/w/index.php?title=Tarjan%27s_strongly_connected_components_algorithm&oldid=1270884973#The_algorithm_in_pseudocode
pub fn tarjan<G: LabelledDiGraph, B>(
    graph: &G,
    starts: impl IntoIterator<Item = G::Node>,
    f: impl FnMut(&[G::Node]) -> ControlFlow<B>,
) -> ControlFlow<B> {
    #[derive_where(Default)]
    struct GraphAnnotator<G: LabelledDiGraph, V: Default> {
        map: FxHashMap<G::Node, V>,
    }

    impl<G: LabelledDiGraph, V: Default> GraphAnnotator<G, V> {
        fn get(&mut self, node: &G::Node) -> &mut V {
            self.map.entry(node.clone()).or_default()
        }
    }

    struct Tarjan<'a, G, F, B>
    where
        G: LabelledDiGraph,
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
        G: LabelledDiGraph,
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
                let LabelledSuccessor::Node(w) = w else {
                    continue;
                };

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

// === IsoSccPortrait === //

/// A "portrait" of a strongly-connected component from a given starting node. Two labelled directed
/// graph portraits with the same origin node will be treated as equal *iff* they are isomorphic.
///
/// - `NInt` is an internal node which has not yet been interned.
/// - `NExt` is an external node which has been interned.
/// - `P` is a successor placeholder type.
///
pub struct IsoSccPortrait<NInt, NExt, P> {
    /// The internal nodes forming this sub-graph. The order of these nodes is deterministic w.r.t
    /// a given key and sub-graph structure.
    internal_nodes: Vec<NInt>,

    /// Edges from internal source node indices to destination nodes.
    edges: Vec<(u32, EdgeDest<NExt, P>)>,
}

#[derive(Debug, Hash, Eq, PartialEq)]
enum EdgeDest<NExt, P> {
    Internal(u32),
    External(NExt),
    Placeholder(P),
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub enum SccNode<NInt, NExt> {
    Internal(NInt),
    External(NExt),
}

impl<NInt, NExt, P> IsoSccPortrait<NInt, NExt, P>
where
    NExt: Clone + hash::Hash + Eq,
    P: Clone + hash::Hash + Eq,
{
    pub fn new<G>(
        graph: &G,
        key: G::Node,
        mut import_int_normalize_ext: impl FnMut(&G::Node) -> SccNode<NInt, NExt>,
    ) -> Self
    where
        G: LabelledDiGraph<Placeholder = P>,
    {
        let mut internal_nodes = Vec::new();
        let mut internal_node_map = FxHashMap::default();
        let mut edges = Vec::new();
        let mut stack = Vec::new();

        // Import the root node.
        let SccNode::Internal(key_as_internal) = import_int_normalize_ext(&key) else {
            unreachable!();
        };
        let key_id = internal_nodes.len() as u32;
        internal_nodes.push(key_as_internal);
        internal_node_map.insert(key.clone(), key_id);

        stack.push((key.clone(), key_id));

        // DFS through the graph.
        while let Some((curr, curr_idx)) = stack.pop() {
            cbit::cbit!(for successor in graph.successors(&curr) {
                let successor = match successor {
                    LabelledSuccessor::Node(node) => node,
                    LabelledSuccessor::Placeholder(placeholder) => {
                        edges.push((curr_idx, EdgeDest::Placeholder(placeholder)));
                        continue;
                    }
                };

                if let Some(&successor_idx) = internal_node_map.get(&successor) {
                    edges.push((curr_idx, EdgeDest::Internal(successor_idx)));
                    continue;
                }

                match import_int_normalize_ext(&curr) {
                    SccNode::Internal(internal) => {
                        let successor_idx = internal_nodes.len() as u32;
                        internal_nodes.push(internal);
                        internal_node_map.insert(successor.clone(), successor_idx);
                        stack.push((successor, successor_idx));
                    }
                    SccNode::External(external) => {
                        edges.push((curr_idx, EdgeDest::External(external)));
                    }
                }
            });
        }

        Self {
            internal_nodes,
            edges,
        }
    }

    pub fn eq<OInt>(
        &self,
        other: &IsoSccPortrait<OInt, NExt, P>,
        mut cmp_internal_data: impl FnMut(&NInt, &OInt) -> bool,
    ) -> bool {
        // Trivial degree checks
        if self.internal_nodes.len() != other.internal_nodes.len() {
            return false;
        }

        if self.edges.len() != other.edges.len() {
            return false;
        }

        // Ensure that the internal nodes' values match.
        if !self
            .internal_nodes
            .iter()
            .zip(&other.internal_nodes)
            .all(|(l, r)| cmp_internal_data(l, r))
        {
            return false;
        }

        // Ensure that internal, external, and placeholder edges match.
        if self.edges != other.edges {
            return false;
        }

        true
    }

    pub fn hash<H: hash::Hasher>(
        &self,
        mut hash_internal_data: impl FnMut(&NInt, &mut H),
        hasher: &mut H,
    ) {
        // TODO: Use `write_length_prefix` once it stabilizes.
        hasher.write_usize(self.internal_nodes.len());

        for elem in &self.internal_nodes {
            hash_internal_data(elem, hasher);
        }

        self.edges.hash(hasher);
    }
}

// === Portrait Keys === //

/// Given the list of nodes in a strongly connected component, this function returns a
/// (hopefully small) set of nodes such that, if two graphs are isomorphic, their set of portrait
/// keys is equal up to isomorphism.
pub fn portrait_keys<'a, G: LabelledDiGraph>(
    graph: &G,
    scc: &'a [G::Node],
    mut hash_node_data: impl FnMut(&G::Node) -> u64,
) -> Vec<&'a G::Node> {
    _ = graph;

    let mut keys = Vec::new();
    let mut min_hash = None;

    for candidate in scc {
        let candidate_hash = hash_node_data(candidate);

        if min_hash.is_none_or(|min_hash| candidate_hash < min_hash) {
            keys.clear();
            min_hash = Some(candidate_hash);
        }

        if min_hash.unwrap() == candidate_hash {
            keys.push(candidate);
        }
    }

    // TODO: Use graph to solve for some property (e.g. centrality) to further disambiguate.

    keys
}

// === Tests === //

#[cfg(test)]
mod tests {
    use ctx2d_utils::hash::{FxHasher, hash_map};

    use std::hash::Hasher as _;

    use super::*;

    trait SimpleGraph: LabelledDiGraph {
        fn data_eq(&self, lhs: &Self::Node, rhs: &Self::Node) -> bool;

        fn data_hash(&self, node: &Self::Node, hasher: &mut impl hash::Hasher);
    }

    #[derive_where(Default)]
    struct SimpleInterner<G: SimpleGraph> {
        #[expect(clippy::type_complexity)]
        roots: FxHashMap<(IsoSccPortrait<G::Node, G::Node, G::Placeholder>, u64), ()>,
    }

    impl<G: SimpleGraph> SimpleInterner<G> {
        pub fn intern(&mut self, graph: &G, node: &G::Node) -> G::Node {
            let mut external_canonicals = FxHashMap::<G::Node, G::Node>::default();

            cbit::cbit!(for scc in tarjan(graph, [node.clone()]) {
                let keys = portrait_keys(graph, scc, |v| {
                    let mut hasher = FxHasher::default();
                    graph.data_hash(node, &mut hasher);
                    hasher.finish()
                });

                // Resolve (or create) the canonical sub-graph for this SCC.
                let tmp;
                let (src_sub_graph, canonical_sub_graph) = 'canonicalize: {
                    // See if any of the keys lead to an interned version of this component.
                    let mut first_sub_graph = None;

                    for key in keys {
                        let sub_graph =
                            IsoSccPortrait::new(
                                graph,
                                key.clone(),
                                |node| match external_canonicals.get(node).cloned() {
                                    Some(ext) => SccNode::External(ext),
                                    None => SccNode::Internal(node.clone()),
                                },
                            );

                        let hash = {
                            let mut hasher = FxHasher::default();
                            sub_graph
                                .hash(|node, hasher| graph.data_hash(node, hasher), &mut hasher);
                            hasher.finish()
                        };

                        let canonical_sub_graph = self.roots.raw_entry().from_hash(
                            hash,
                            |&(ref other_sub_graph, other_hash)| {
                                hash == other_hash
                                    && sub_graph
                                        .eq(other_sub_graph, |lhs, rhs| graph.data_eq(lhs, rhs))
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

    impl LabelledDiGraph for ConsGraph {
        type Node = Cons;
        type Placeholder = ();

        fn successors<B>(
            &self,
            node: &Cons,
            mut f: impl FnMut(LabelledSuccessor<Self::Node, Self::Placeholder>) -> ControlFlow<B>,
        ) -> ControlFlow<B> {
            f(match self.nodes[node.0].left {
                Some(node) => LabelledSuccessor::Node(node),
                None => LabelledSuccessor::Placeholder(()),
            })?;

            f(match self.nodes[node.0].right {
                Some(node) => LabelledSuccessor::Node(node),
                None => LabelledSuccessor::Placeholder(()),
            })?;

            ControlFlow::Continue(())
        }
    }

    impl SimpleGraph for ConsGraph {
        fn data_eq(&self, lhs: &Cons, rhs: &Cons) -> bool {
            self.nodes[lhs.0].label == self.nodes[rhs.0].label
        }

        fn data_hash(&self, node: &Cons, hasher: &mut impl hash::Hasher) {
            self.nodes[node.0].label.hash(hasher);
        }
    }

    #[test]
    fn simple() {
        let mut interner = SimpleInterner::default();
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

        // Disjoint graph 1
        let foo_4 = graph.spawn("foo");

        graph.set_left(foo_4, Some(foo_1));

        assert!(interner.intern(&graph, &foo_4) == foo_4);

        // Disjoint graph 2
        let foo_5 = graph.spawn("foo");
        let bar_5 = graph.spawn("bar");

        graph.set_left(foo_5, Some(bar_5));
        graph.set_left(bar_5, Some(bar_5));

        assert!(interner.intern(&graph, &foo_5) == foo_5);

        // Disjoint graph 3
        let foo_6 = graph.spawn("foo");
        let bar_6 = graph.spawn("bar");

        graph.set_left(foo_6, Some(bar_6));
        graph.set_left(bar_6, Some(bar_6));
        graph.set_right(bar_6, Some(bar_6));

        assert!(interner.intern(&graph, &foo_6) == foo_6);
    }
}
