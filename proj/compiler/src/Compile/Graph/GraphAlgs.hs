{-
  Specialized graph algorithms used in various parts of our compiler.
-}

module Compile.Graph.GraphAlgs where

import Compile.Graph.Graph

import qualified Data.Map as Map
import qualified Data.Set as Set

{-
  Solves the following problem: given a directed graph with sets of elems at each vertex, and a
  rule for when they can move to neighbor vertices, compute the set of elems that can reach each
  vertex. It's propogateAllBackwards because the elems can only move backwards along in-edges;
  they can't move along out-edges at all.
-}
propogateAllBackwards :: (Ord v, Ord a) => (s -> [a]) -> (a -> s -> Bool) -> Graph v s
                                        -> Map.Map v (Set.Set a)
propogateAllBackwards getElems canProp (G g) =
  let startingPropMap = Map.map (\_ -> Set.empty) g
      G elemG = mapStates getElems $ G g
  in Map.foldl (processNode (G g) canProp) startingPropMap elemG
  where processNode :: (Ord v, Ord a) => Graph v s -> (a -> s -> Bool)
                                      -> Map.Map v (Set.Set a) -> (Set.Set v, Set.Set v, [a])
                                      -> Map.Map v (Set.Set a)
        processNode (G g) canProp propMap (preds, _, elems) =
          foldl (propTo canProp $ G g) propMap [(e, v) | v <- Set.elems preds, e <- elems]
        propTo :: (Ord v, Ord a) => (a -> s -> Bool) -> Graph v s
                                 -> Map.Map v (Set.Set a) -> (a, v) -> Map.Map v (Set.Set a)
        propTo canProp (G g) propMap (elem, v) =
          let (preds, succs, s) = g Map.! v
          in (if (not (canProp elem s) || (elem `Set.member` (propMap Map.! v)))
              then propMap
              else (let propMap' = Map.insertWith Set.union v (Set.singleton elem) propMap
                    in foldl (propTo canProp $ G g) propMap' [(elem, p) | p <- Set.elems preds]))

{-
  Finds the subgraph of the input graph that can be reached by following out-edges from the given
  vertex.
-}
reachableFrom :: (Ord v) => v -> Graph v s -> Graph v s
reachableFrom v (G g) = 
  let seen = dfsTo (G g) v Set.empty
  in G $ Map.fromSet (getInducedNode (G g) seen) seen
  where dfsTo :: (Ord v) => Graph v s -> v -> Set.Set v -> Set.Set v
        dfsTo (G g) v seen =
          if (Set.member v seen)
          then seen
          else (let (_, succs, _) = g Map.! v
                in Set.fold (dfsTo $ G g) (Set.insert v seen) succs)

        getInducedNode :: (Ord v) => Graph v s -> Set.Set v -> v -> (Set.Set v, Set.Set v, s)
        getInducedNode (G g) vs v =
          let (preds, succs, s) = g Map.! v
          in (Set.intersection preds vs, Set.intersection succs vs, s)


{-
  Solves the following problem: given a directed ACYCLIC graph, map a function over the nodes,
  also giving the function the results of mapping over its predecessors.
-}
dfsMapBackwardsWithPreds :: (Ord v) => (s -> [s'] -> s') -> Graph v s -> Graph v s'
dfsMapBackwardsWithPreds f graph =
  let resMap = dfsFoldBackwardsWithPreds (updateGraph f) Map.empty graph
  in addMapData (\x -> \_ -> x) resMap graph
  where updateGraph :: (Ord v) => (s -> [s'] -> s') -> Map.Map v s' -> Map.Map v s -> v -> s
                               -> Map.Map v s'
        updateGraph f resMap predMap v state =
          Map.insert v (f state $ map (resMap Map.!) $ Map.keys predMap) resMap

{-
  Solves the following problem: given a directed graph, fold through the nodes in the graph in
  child-first referse-DFS order (ensure that all in-neighbors are called before yourself,
  breaking cycles at an arbitrary point). For each vertex, the user function is also passed a
  map of predecessor states, so it can do neighborhood-aware processing.
  TODO: is this used anywhere?
-}
dfsFoldBackwardsWithPreds :: (Ord v) => (a -> Map.Map v s -> v -> s -> a) -> a -> Graph v s -> a
dfsFoldBackwardsWithPreds f acc (G g) =
  let (_, acc') = foldl (visit f $ G g) (Set.empty, acc) $ Map.keys g
  in acc'
  where visit :: (a -> Map.Map v s -> v -> s -> a) -> Graph v s -> (Set.Set v, a) -> v
              -> (Set.Set v, a)
        visit f (G g) (seen, acc) v =
          if (v `Set.member` seen)
          then (seen, acc)
          else (let (preds, _, state) = g Map.! v
                    (seen', acc') = Set.foldl (visit f $ G g) (Set.insert v seen, acc) preds
                in (seen', f acc' (Map.fromSet ((G g) !) preds) v state))
