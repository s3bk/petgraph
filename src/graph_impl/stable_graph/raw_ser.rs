
use serde::de::Error;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use std::marker::PhantomData;

use crate::prelude::*;

use crate::graph::Node;
use crate::graph::{Edge, IndexType};
use crate::serde_utils::CollectSeqWithLength;
use crate::serde_utils::MappedSequenceVisitor;
use crate::serde_utils::{FromDeserialized};
use crate::stable_graph::StableGraph;
use crate::EdgeType;

use super::super::serialization::{EdgeProperty};

impl<N, E, Ty, Ix> StableGraph<N, E, Ty, Ix>
    where Ix: IndexType, Ty: EdgeType
{
    pub fn serialize_raw<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where N: Serialize, E: Serialize, Ty: EdgeType, Ix: Serialize + IndexType
    {
        let raw = SerRawStableGraph {
            g: SerRawGraph::new(&self.g),
            common: RawStableGraphCommon {
                node_count: self.node_count,
                edge_count: self.edge_count,
                free_node: self.free_node,
                free_edge: self.free_edge,
            },
        };
        SerRawStableGraph::serialize(&raw, serializer)
    }

    pub fn deserialize_raw<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error>
        where N: Deserialize<'de>, E: Deserialize<'de>, Ix: Deserialize<'de>
    {
        let raw = DeSerRawStableGraph::deserialize(deserializer)?;
        let graph = StableGraph {
            g: raw.g.try_into_graph()?,
            node_count: raw.common.node_count,
            edge_count: raw.common.edge_count,
            free_node: raw.common.free_node,
            free_edge: raw.common.free_edge,
        };
        Ok(graph)
    }
}

pub struct RawStableGraph<N, E, Ty, Ix>(
    pub StableGraph<N, E, Ty, Ix>
);
impl<N, E, Ty, Ix> Serialize for RawStableGraph<N, E, Ty, Ix>
    where N: Serialize, E: Serialize, Ty: EdgeType, Ix: Serialize + IndexType
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.serialize_raw(serializer)
    }
}
impl<'de, N, E, Ty, Ix> Deserialize<'de> for RawStableGraph<N, E, Ty, Ix>
    where N: Deserialize<'de>, E: Deserialize<'de>, Ix: IndexType + Deserialize<'de>, Ty: EdgeType
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where D: Deserializer<'de>
    {
        Ok(RawStableGraph(StableGraph::deserialize_raw(deserializer)?))
    }
}

#[derive(Serialize)]
#[serde(bound(serialize = "N: Serialize, E: Serialize, Ix: IndexType + Serialize"))]
struct SerRawGraph<'a, N: 'a, E: 'a, Ix: 'a + IndexType> {
    #[serde(serialize_with = "ser_raw_graph_nodes")]
    nodes: &'a [Node<N, Ix>],
    #[serde(serialize_with = "ser_raw_graph_edges")]
    edges: &'a [Edge<E, Ix>],
    edge_property: EdgeProperty,
}
#[derive(Deserialize)]
#[serde(bound(deserialize = "N: Deserialize<'de>, E: Deserialize<'de>, Ix: IndexType + Deserialize<'de>"))]
struct DeSerRawGraph<N, E, Ix: IndexType> {
    #[serde(deserialize_with = "deser_raw_graph_nodes")]
    nodes: Vec<Node<N, Ix>>,
    #[serde(deserialize_with = "deser_raw_graph_edges")]
    edges: Vec<Edge<E, Ix>>,
    edge_property: EdgeProperty,
}

impl<'a, N: 'a, E: 'a, Ix: 'a + IndexType> SerRawGraph<'a, N, E, Ix> {
    fn new<Ty: EdgeType>(g: &'a Graph<N, E, Ty, Ix>) -> Self {
        SerRawGraph {
            nodes: &g.nodes,
            edges: &g.edges,
            edge_property: EdgeProperty::from(PhantomData::<Ty>)
        }
    }
}
impl<N, E, Ix: IndexType> DeSerRawGraph<N, E, Ix> {
    fn try_into_graph<Ty: EdgeType, E2: Error>(self) -> Result<Graph<N, E, Ty, Ix>, E2> {
        Ok(Graph {
            nodes: self.nodes,
            edges: self.edges,
            ty: PhantomData::from_deserialized(self.edge_property)?,
        })
    }
}

#[derive(Serialize)]
struct SerRawStableGraph<'a, N: 'a, E: 'a, Ix: 'a + IndexType> {
    g: SerRawGraph<'a, N, E, Ix>,
    common: RawStableGraphCommon<Ix>,
}

#[derive(Deserialize)]
struct DeSerRawStableGraph<N, E, Ix: IndexType> {
    g: DeSerRawGraph<N, E, Ix>,
    common: RawStableGraphCommon<Ix>,
}


#[derive(Serialize, Deserialize)]
#[serde(bound(serialize = "Ix: IndexType + Serialize"))]
#[serde(bound(deserialize = "Ix: IndexType + Deserialize<'de>"))]
struct RawStableGraphCommon<Ix> {
    node_count: usize,
    edge_count: usize,
    free_node: NodeIndex<Ix>,
    free_edge: EdgeIndex<Ix>,
}

fn ser_raw_graph_nodes<S, N, Ix>(
    nodes: &&[Node<N, Ix>],
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    N: Serialize,
    Ix: Serialize + IndexType,
{
    serializer.collect_seq_exact(nodes.iter().map(|node| {
        (&node.weight, node.next)
    }))
}
fn deser_raw_graph_nodes<'de, D, N, Ix>(deserializer: D) -> Result<Vec<Node<N, Ix>>, D::Error>
    where D: Deserializer<'de>, N: Deserialize<'de>, Ix: IndexType + Deserialize<'de>
{
    deserializer.deserialize_seq(MappedSequenceVisitor::new(|(weight, next)| Ok(Node { weight, next })))
}

fn ser_raw_graph_edges<S, E, Ix>(
    edges: &&[Edge<E, Ix>],
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
    E: Serialize,
    Ix: Serialize + IndexType,
{
    serializer.collect_seq_exact(edges.iter().map(|edge| {
        (&edge.weight, edge.next, edge.node)
    }))
}
fn deser_raw_graph_edges<'de, D, E, Ix>(deserializer: D) -> Result<Vec<Edge<E, Ix>>, D::Error>
    where D: Deserializer<'de>, E: Deserialize<'de>, Ix: IndexType + Deserialize<'de>
{
    deserializer.deserialize_seq(MappedSequenceVisitor::new(|(weight, next, node)| Ok(Edge { weight, next, node })))
}
