{-# LANGUAGE ExistentialQuantification #-}
{-
  Represents a polymorphic, directed graph. Mostly a convenience wrapper for some algorithms to
  avoid duplicating too much code, not intended for general use.
-}

module Compile.Graph.Graph where

import qualified Data.Map as Map
import qualified Data.Set as Set

{-
  Represents a directed graph on vertices v, where each vertex has three things:
    A set of in-neighbors (vertices u such that (u, v) is an edge).
    A set of out-neighbors (vertices u such that (v, u) is an edge).
    A state object of type s.
-}
data Graph v s = (Ord v) => G (Map.Map v (Set.Set v, Set.Set v, s))
instance (Show v, Show s) => Show (Graph v s) where
  show (G g) = show g


{-
  Returns a list of the vertices of the graph in ascending order.
-}
verts :: (Ord v) => Graph v s -> [v]
verts (G g) = Map.keys g

{-
  Returns a list of the states for the vertices of the graph, in ascending order of vertex.
-}
states :: (Ord v) => Graph v s -> [s]
states (G g) = map (\(_, _, s) -> s) $ Map.elems g

{-
  Turns the states and edges in a graph into a list.
-}
nodes :: (Ord v) => Graph v s -> [(Set.Set v, Set.Set v, s)]
nodes (G g) = Map.elems g

{-
  Does state lookup, just like in a map.
-}
(!) :: (Ord v) => Graph v s -> v -> s
(G g) ! v =
  let (_, _, s) = g Map.! v
  in s

{-
  Returns the state for a vertex, as well as its in and out-edges.
-}
getWithEdges :: (Ord v) => Graph v s -> v -> (Set.Set v, Set.Set v, s)
getWithEdges (G g) v = g Map.! v

{-
  Returns true if the graph contains the given vertex.
-}
member :: (Ord v) => v -> Graph v s -> Bool
member v (G g) = Map.member v g

{-
  Returns true if the graph doesn't contain the given vertex.
-}
notMember :: (Ord v) => v -> Graph v s -> Bool
notMember v (G g) = Map.notMember v g

{-
  Updates the state for the vertex v, assumed to be in the graph. If v isn't in the grpah it does
  nothing.
-}
update :: (Ord v) => v -> s -> Graph v s -> Graph v s
update v s (G g) = G $ Map.adjust (\(preds,succs,_) -> (preds, succs, s)) v g

{-
  Adds the given directed edge to the graph.
  NOTE: if one of the endpoints isn't in the graph, it will end in an inconsistent state.
-}
addEdge :: (Ord v) => v -> v -> Graph v s -> Graph v s
addEdge v v' (G g) =
  G $ Map.adjust (\(preds, succs, s) -> (preds, Set.insert v' succs, s)) v $
      Map.adjust (\(preds, succs, s) -> (Set.insert v preds, succs, s)) v' g

{-
  Removes the given directed edge from the graph.
  NOTE: if one of the endpoints isn't in the graph, it will end in an inconsistent state.
-}
deleteEdge :: (Ord v) => v -> v -> Graph v s -> Graph v s
deleteEdge v v' (G g) = G $ deleteEdgeSrc v v' $ deleteEdgeDst v v' g

{-
  Removes the given vertex from the graph.
-}
deleteVertex :: (Ord v) => v -> Graph v s -> Graph v s
deleteVertex v (G g) =
  let (Just (preds, succs, _), g') = Map.updateLookupWithKey (\_ -> \_ -> Nothing) v g
      g'' = Set.foldl (\g -> \predV -> deleteEdgeSrc predV v g) g' preds
      g''' = Set.foldl (\g -> \succV -> deleteEdgeDst v succV g) g'' succs
  in G g'''

{-
  Inserts the given vertex, along with initial sets of in-edges and out-edges, into the graph.
-}
insertVertex :: (Ord v) => v -> (Set.Set v, Set.Set v, s) -> Graph v s -> Graph v s
insertVertex v (preds, succs, s) (G g) =
  let g' = Map.insert v (preds, succs, s) g
      g'' = Set.foldl (\g -> \predV -> insertEdgeSrc predV v g) g' preds
      g''' = Set.foldl (\g -> \succV -> insertEdgeDst v succV g) g' succs
  in G g'''


{-
  Helper functions for edge deletion and insertion. NOT to be used outside this file.
-}
deleteEdgeSrc v v' g = Map.adjust (\(preds, succs, s) -> (preds, Set.delete v' succs, s)) v g
deleteEdgeDst v v' g = Map.adjust (\(preds, succs, s) -> (Set.delete v preds, succs, s)) v' g
insertEdgeSrc v v' g = Map.adjust (\(preds, succs, s) -> (preds, Set.insert v' succs, s)) v g
insertEdgeDst v v' g = Map.adjust (\(preds, succs, s) -> (Set.insert v preds, succs, s)) v' g


{-
  Given a contraction function, a directed edge (v, v'), and a graph containing the edge (v, v'),
  contracts the vertices v and v' along their shared edge in G, setting the new state equal to
  f v v'.
-}
contractEdge :: (Ord v) => (s -> s -> s) -> v -> v -> Graph v s -> Graph v s
contractEdge f v v' (G g) =
  case (Map.lookup v g, Map.lookup v' g) of
    (Nothing, _) -> error "Graph: tried to contract nonexistent edge "
    (_, Nothing) -> error "Graph: tried to contract nonexistent edge "
    (Just (preds, succs, s), Just (preds', succs', s')) ->
      if (v' `Set.notMember` succs || v `Set.notMember` preds')
      then error "Graph: tried to contrace nonexistent edge "
      else (let graph' = deleteVertex v $ deleteVertex v' $ G g
                preds'' = Set.union preds $ Set.delete v preds'
                succs'' = Set.union succs' $ Set.delete v' succs
                s'' = f s s'
            in insertVertex v (preds'', succs'', s'') graph')


{-
  Given a map from vertices to values and a function from values to lists of out-edges, produces
  a graph by computing the in- and out-edges for each vertex.
-}
fromMap :: (Ord v) => (a -> [v]) -> Map.Map v a -> Graph v a
fromMap = fromMapWith id

{-
  Given a map from vertices to values, a function from values to vertex states, and a function
  from values to lists of out-edges, produces, produces a directed graph from vertices to
  vertex states by first finding all the edges, then adding the state information.
-}
fromMapWith :: (Ord v) => (a -> s) -> (a -> [v]) -> Map.Map v a -> Graph v s
fromMapWith getState getOutNeighbors m =
  let oListMap = Map.map getOutNeighbors m
      iEdges = [(v2, Set.singleton v) | (v, v2s) <- Map.toList oListMap, v2 <- v2s]
      defaultIEdges = [(v, Set.empty) | v <- Map.keys m]
      iMap = Map.fromListWith Set.union $ iEdges ++ defaultIEdges
      eMap = Map.intersectionWith (\i -> (\o -> (i, o))) iMap $ Map.map Set.fromList oListMap
  in G $ Map.intersectionWith (\(i, o) -> (\s -> (i, o, s))) eMap $ Map.map getState m

{-
  Converts the graph to a map on vertices, where the values are the states in the graph.
-}
toMap :: (Ord v) => Graph v s -> Map.Map v s
toMap = toMapWith id

{-
  Converts the graph to a map on vertices, first applying a function to each state
-}
toMapWith :: (Ord v) => (s -> a) -> Graph v s -> Map.Map v a
toMapWith f (G g) = Map.map (\(_, _, s) -> f s) g

{-
  Converts the graph into a map from vertex to (set of in-edges, set of out-edges, data).
-}
toMapWithEdges :: (Ord v) => Graph v s -> Map.Map v (Set.Set v, Set.Set v, s)
toMapWithEdges (G g) = g

{-
  Given a graph and a map on the same domain of vertices, incorporates the data from each vertex
  in the map into that vertex's state object in the graph.
-}
addMapData :: (a -> s -> s2) -> Map.Map v a -> Graph v s -> Graph v s2
addMapData add m (G g) =
  let g2 = Map.intersectionWith (\x -> (\(prevs, succs, s) -> (prevs, succs, add x s))) m g
  in (if (Map.size g2 == Map.size g)
      then G g2
      else error "addMapData was passed a map and graph with different keysets")

{-
  Filters the edges in the graph, by applying a function f to each directed edge (u, v).
-}
filterEdges :: (v -> v -> Bool) -> Graph v s -> Graph v s
filterEdges f (G g) = G (Map.mapWithKey (filterFromVertex f) g)
  where filterFromVertex :: (Ord v) => (v -> v -> Bool) -> v -> (Set.Set v, Set.Set v, s)
                                    -> (Set.Set v, Set.Set v, s)
        filterFromVertex f v (preds, succs, s) =
          (Set.filter (\v' -> f v' v) preds, Set.filter (f v) succs, s)

{-
  Does a fold through the states in increasing order by vertex.
-}
fold :: (Ord v) => (a -> s -> a) -> a -> Graph v s -> a
fold f init (G g) = foldl (\acc -> (\(preds, succs, s) -> f acc s)) init $ Map.elems g

{-
  Does a mapAccumL through the states in increasing order by vertex, also giving the map function
  information about the in-neighbors of each vertex.
-}
mapAccumWithPredSetAndKey :: (Ord v, Show v) => (a -> v -> s -> Set.Set v -> (a, s2)) -> a
                                             -> Graph v s -> (a, Graph v s2)
mapAccumWithPredSetAndKey f init (G g) = 
  let (acc, g') = Map.mapAccumWithKey (updateNode f) init g
  in (acc, G g')
  where updateNode f a v (preds, succs, s) = 
          let (acc', s') = f a v s preds
          in (acc', (preds, succs, s'))


{-
  Maps over the states in a graph.
-}
mapStates :: (Ord v) => (s -> s2) -> Graph v s -> Graph v s2
mapStates f (G g) = G $ Map.map (\(preds, succs, s) -> (preds, succs, f s)) g

{-
  Map over the states in a graph, also giving the map function information about all the
  out-neighbors of each vertex.
-}
mapWithSuccs :: (Ord v, Show v) => (s -> Map.Map v s -> s2) -> Graph v s -> Graph v s2
mapWithSuccs f (G g) = G $ Map.map (updateNode f g) g
  where updateNode f g (preds, succs, s) = (preds, succs, f s $ Map.fromSet (G g !) succs)

{-
  Map over the states in a graph, also giving the map function information about all the
  in-neighbors of each vertex.
-}
mapWithPredsAndVertex :: (Ord v, Show v) => (v -> s -> Map.Map v s -> s2) -> Graph v s
                      -> Graph v s2
mapWithPredsAndVertex f (G g) = G $ Map.mapWithKey (updateNode f g) g
  where updateNode f g v (preds, succs, s) = (preds, succs, f v s $ Map.fromSet (G g !) preds)
