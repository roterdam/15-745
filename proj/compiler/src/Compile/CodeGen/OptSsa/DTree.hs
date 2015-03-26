{-
  Handles the Dominance Tree datatype, as well as algorithms for it.
-}

module Compile.CodeGen.OptSsa.DTree where

import Compile.Types.S3Asm
import Compile.Types.Common (TmpIdent, BlockIdent)
import Compile.CodeGen.OptSsa.Common
import qualified Compile.Graph.Graph as Graph

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe

{-
  The dominance tree datatype: is a blockMap with additional structure.
-}
data DTree = Node (BlockIdent, Block) [DTree]


{-
  Turns the given blockMap into a dominance tree by doing the following:
  1. Get a map from blocks to lists of their predecessors, removing back-edges for simplicity.
  2. For each block, compute the LCA of its parents; this is its dominating ancestor.
  3. Flip the results of (2) around to get a map from each block to the blocks it's the
     dominating ancestor for.
  4. Use (3) to build the tree.
-}
fromBMap :: BlockMap -> DTree
fromBMap bMap =
  let predMap = removeBackEdges $ getPredecessorMap bMap
      bs = Map.keys predMap
      pairs = [(getDominatingAncestor predMap b, b) | b <- Map.keys predMap]
      pairs' = mapMaybe (\(a, b) -> if isJust a then Just (fromJust a, [b]) else Nothing) pairs
      successorMap = Map.fromListWith (++) pairs'
  in buildTree bMap successorMap
  where removeBackEdges :: Map.Map BlockIdent [BlockIdent] -> Map.Map BlockIdent [BlockIdent]
        removeBackEdges = Map.mapWithKey (\b -> filter (<= b))

        getDominatingAncestor :: Map.Map BlockIdent [BlockIdent] -> BlockIdent
                              -> Maybe BlockIdent
        getDominatingAncestor predMap b =
          case (predMap Map.! b) of
            [] -> Nothing
            [b'] -> Just b'
            bs -> Just $ Set.findMax $ foldl1 Set.intersection $ map (getAncestors predMap) bs
        
        getAncestors :: Map.Map BlockIdent [BlockIdent] -> BlockIdent -> Set.Set BlockIdent
        getAncestors predMap b = gARec predMap Set.empty b
        gARec :: Map.Map BlockIdent [BlockIdent] -> Set.Set BlockIdent -> BlockIdent
              -> Set.Set BlockIdent
        gARec predMap seen b
          | b `Set.member` seen = seen
          | otherwise = foldl (gARec predMap) (Set.insert b seen) (predMap Map.! b)

        buildTree :: BlockMap -> Map.Map BlockIdent [BlockIdent] -> DTree
        buildTree bMap succMap =
          let (minB, _) = Map.findMin bMap
          in bTRec bMap succMap minB
        bTRec :: BlockMap -> Map.Map BlockIdent [BlockIdent] -> BlockIdent -> DTree
        bTRec bMap succMap b =
          Node (b, bMap Map.! b) $ map (bTRec bMap succMap) $ Map.findWithDefault [] b succMap


{-
  Turns the given dominance tree back into a block map.
-}
toBMap :: DTree -> BlockMap
toBMap (Node (b, block) children) = Map.insert b block $ Map.unions $ map toBMap children

{-
  Takes the edges of the dominance tree and puts them in a map.
-}
toDEdgeMap :: DTree -> Map.Map BlockIdent [BlockIdent]
toDEdgeMap (Node (b, _) children) =
  Map.union (Map.singleton b $ map getBlockNum children) $ Map.unions $ map toDEdgeMap children
  where getBlockNum :: DTree -> BlockIdent
        getBlockNum (Node (b, _) _) = b

{-
  Given a tree and an initial state:
  1. Updates the root and state, noting if the root changed in the update.
  2. Passes the updated state down to the children, getting back a list of updated children,
     along with which (if any) changed.
  3. Rebuilds the root using the new children, and returns true if the root or any child
     changed.
-}
mapAccumTopDown :: (acc -> BlockIdent -> Block -> ((acc, Bool), Block)) -> acc -> DTree
                -> (DTree, Bool)
mapAccumTopDown update acc (Node (b, block) children) =
  let ((acc', rootChanged), block') = update acc b block
      pairs = map (mapAccumTopDown update acc') children
      (children', childrenChanged) = unzip pairs
  in (Node (b, block') children', rootChanged || or childrenChanged)
