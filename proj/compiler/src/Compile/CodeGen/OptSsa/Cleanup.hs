{-
  Handles code-trimming optimizations that are made possible by constant/copy propogation.
  Optimizations include:
  --> unreachable block elimination
  --> redundant check elimination
-}

module Compile.CodeGen.OptSsa.Cleanup (cleanup) where

import Compile.Types.S3Asm
import Compile.CodeGen.OptSsa.Common
import Compile.Types.Common (TmpIdent, BlockIdent)
import qualified Compile.CodeGen.OptSsa.Cfg as Cfg
import qualified Compile.Graph.Graph as Graph
import qualified Compile.Graph.GraphAlgs as GraphAlgs

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List (mapAccumL)

data Check = CNonZero     Src
           | CNonNeg      Src
           | CEqual       Src Src
           | CDivOverflow Src Src
           | CShiftMax    Src
           | CArrEnd      Src Src deriving (Ord, Eq)
type CheckSet = Set.Set Check
type CheckMap = Map.Map BlockIdent CheckSet
type CheckGraph = Graph.Graph BlockIdent CheckSet

{-
  Applies the above optimizations to the given blockMap.
-}
cleanup :: BlockMap -> BlockMap
cleanup bMap = 
  let (bMap', _) = Cfg.removeUnreachableBlocks bMap
      bMap'' = removeRedundantChecks bMap'
  in bMap''

{-
  Removes redundant check statements from the program, where a check is redundant if one or
  more checks that encompass it fully are guaranteed to have been run already. For now, the
  only rule for determining encompassing is "c1 encompasses c2 if c1 == c2", but more rules
  could be added in the future if needed.
  The way the set of prior checks is determined is by looping through the edges in reverse-DFS
  order, intersecting the sets for all predecessors of a node before processing that node. Note
  that back-edges are removed for simplicity; this is valid because the set of checks that have
  been seen at the end of a loop is a superset of those that were seen going in, so they would
  be intersected out anyway.
-}
removeRedundantChecks :: BlockMap -> BlockMap
removeRedundantChecks bMap =
  Map.intersectionWith simplifyBlock bMap $ getCheckMap bMap
  where getCheckMap :: BlockMap -> CheckMap
        getCheckMap bMap =
          let cg = removeBackEdges $ bgFromBMap getChecks bMap
              cg' = GraphAlgs.dfsMapBackwardsWithPreds getResultingCheckSet cg
              cg'' = Graph.mapWithPredsAndVertex (\_ -> \_ -> intersectCheckSets . Map.elems) cg'
          in Graph.toMap cg''

        {-
          Finds the set of Check statements for the given block.
        -}
        getChecks :: Block -> CheckSet
        getChecks (BBlock _ ss _) = Set.fromList $ mapMaybe getCheck ss

        removeBackEdges :: CheckGraph -> CheckGraph
        removeBackEdges = Graph.filterEdges (<=)

        {-
          Takes in a check set and list of parent check sets, intersects the parent sets, and
          unions it with your set.
        -}
        getResultingCheckSet :: CheckSet -> [CheckSet] -> CheckSet
        getResultingCheckSet checkSet parentCheckSets =
          unionCheckSets [checkSet, intersectCheckSets parentCheckSets]

        {-
          Given a list of check sets, finds a set of the checks in any of them :(
        -}
        unionCheckSets :: [CheckSet] -> CheckSet
        unionCheckSets = Set.unions

        {-
          Given a list of check sets, finds a set of the checks in all of them.
        -}
        intersectCheckSets :: [CheckSet] -> CheckSet
        intersectCheckSets [] = Set.empty
        intersectCheckSets cs = foldl1 Set.intersection cs

        {-
          Given a block and set of checks guaranteed to be executed by the start of the block,
          attempts to remove redundant checks from the block.
        -}
        simplifyBlock :: Block -> CheckSet -> Block
        simplifyBlock (BBlock params ss j) checks = 
          let (checks', ssOpts) = mapAccumL transSIns checks ss
          in BBlock params (catMaybes ssOpts) j
          where transSIns :: CheckSet -> SIns -> (CheckSet, Maybe SIns)
                transSIns checks s =
                  case (getCheck s) of
                    Nothing -> (checks, Just s)
                    Just c -> (Set.insert c checks,
                               if c `Set.member` checks then Nothing else Just s)

        {-
          Tries to build a Check from the given instruction, returning a Maybe of the result.
        -}
        getCheck :: SIns -> Maybe Check
        getCheck (CheckNonZero _ s _) = Just $ CNonZero s
        getCheck (CheckNonNeg _ s _) = Just $ CNonNeg s
        getCheck (CheckEqual _ s1 s2 _) = Just $ CEqual s1 s2
        getCheck (CheckDivOverflow s1 s2) = Just $ CDivOverflow s1 s2
        getCheck (CheckShiftMax s) = Just $ CShiftMax s
        getCheck (CheckArrEnd s1 s2) = Just $ CArrEnd s1 s2
        getCheck _ = Nothing
