{-
  Handles all control flow graph optimizations (empty block elimination, block merging, dead
  block elimination, etc).
-}

module Compile.CodeGen.OptSsa.Cfg (removeUnreachableBlocks, simplifyCfg) where

import Compile.Types.Common (TmpIdent, BlockIdent)
import Compile.Types.S3Asm
import qualified Compile.Graph.Graph as Graph
import qualified Compile.Graph.GraphAlgs as GraphAlgs
import Compile.CodeGen.OptSsa.Common (ValueMap, BlockGraph)
import qualified Compile.CodeGen.OptSsa.Common as SCommon

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (mapAccumL)

import Debug.Trace

{-
  Removes all blocks from the given map that are unreachable from the source node, and returns
  whether or not it removed any blocks.
-}
removeUnreachableBlocks :: BlockMap -> (BlockMap, Bool)
removeUnreachableBlocks bMap =
  let (firstBlock, _) = Map.findMin bMap
      bMap' = Graph.toMap $ GraphAlgs.reachableFrom firstBlock $ SCommon.bgFromBMap id bMap
  in (bMap', Map.size bMap /= Map.size bMap')


{-
  Simplifies the implicit control graph of the given block map, by removing unreachable nodes
  and compressing according to the following rules:

  skipping:  if block b has no ss or parameters and ends in "jmp b'(...)", all predecessors of
             b can jump directly to b' instead.
  unforking: if b ends in cjmp _ _ _ l1 l2, and l1 exactly equals l2, then b is changed to end
             in "jmp l1" instead.
  return lifting: if b has no ss and ends in a return, try to set the predecessors of b to
                  return instead.
  merging: if b ends in "Jmp b'" and nothing else goes to b', merge b' into b.

  Relations between optimizations:
  --> skipping causes unforking and merging
  --> unforking causes skipping, return lifting, and merging
  --> return lifting causes more return lifting
  --> merging doesn't cause anything

  Current order of operations:
  1. Do skipping and unforking together.
  2. Do return lifting. (TODO: separate this out from step 1)
  3. Do merging.

  ASSUMES:
    there are no multiedges in the starting cfg, where a multiedge is created by a conditional
    jump with both branches going to the same value.
-}
simplifyCfg :: BlockMap -> BlockMap
simplifyCfg bMap =
  if (Map.size bMap == 0)
  then bMap
  else (let bg = SCommon.bgFromBMap id bMap
            bg' = doSkip bg
            bg'' = doMerge bg'
        in Graph.toMap bg'')



{-
  Performs combined node skipping and unforking on the block graph. Note that unforking creates
  new opportunities for skipping, but not for successors of the skipped node, so we can just loop
  through the blocks in bottom-to-top order and try to skip each one.
-}
doSkip :: BlockGraph Block -> BlockGraph Block
doSkip bg = foldr trySkip bg $ Graph.verts bg

{-
  Given a map m and an empty block b in m, tries to set b's predecessors to skip over b.
  The pair (predB, b) succeeds if:
    --> b has no straight-line statements
    --> (predJ, j) is one of the following cases:
        --> (jmp, ret) (see TODO below)
        --> (jmp, jmp)
        --> (cjmp, jmp)
  If every predecessor of b successfully skips b, b is removed from the map.
-}
trySkip :: BlockIdent -> BlockGraph Block -> BlockGraph Block
trySkip b bg =
  if (b `Graph.notMember` bg)
  then bg
  else (let (preds, _, BBlock _ ss _) = bg `Graph.getWithEdges` b
        in (if (Set.size preds == 0) || (not (null ss))
            then bg
            else (let (bg', info) = mapAccumL (trySkipEdge b) bg $ Set.toList preds
                      (opSucceeded, todos) = unzip info
                  in (if all id opSucceeded
                      then (let todos' = Set.toList $ Set.fromList $ concat todos
                            in foldr trySkip (Graph.deleteVertex b bg') todos')
                      else bg'))))
  where {-
          Given, b, m, and predB, where b is known to have no ss, tries to skip directly to b's
          successors from predB. Returns the updated map, and whether or not the skip was
          successful.

          TODO: right now, this also copies rets up the graph, where applicable. This is out of
                place here and causes extra recursing to be done in the unfork case, so it
                should be moved to its own pass.

          NOTE: Doing this optimization can create a case for unforking, which can in turn
                create a case for skipping again. Normally, this function would just call
                trySkip in the relevant cases below, but this causes problems because b might
                be dead, and trySkip is written assuming there are no dead blocks in the CFG.
                To avoid this problem, we instead return a list of the nodes to re-examine,
                which are then re-examined in our parent.
        -}
        trySkipEdge :: BlockIdent -> BlockGraph Block -> BlockIdent
                    -> (BlockGraph Block, (Bool, [BlockIdent]))
        trySkipEdge b bg predB =
          let BBlock params [] j = bg Graph.! b
              BBlock predParams predSs predJ = bg Graph.! predB
          in (case (predJ, params, j) of
                (Jmp predL, _, Ret sz s) ->
                  let Label _ predArgs = predL
                      rMap = Map.fromList $ zip (map fst params) (map fst predArgs)
                      predJ' = Ret sz $ transSrc rMap s
                      predBlock' = BBlock predParams predSs predJ'
                  in (Graph.update predB predBlock' $ Graph.deleteEdge predB b bg, (True, []))
                (Jmp predL, [], Jmp l) ->
                  let Label succB _ = l
                  in (if b == succB
                      then (bg, (False, []))
                      else (let succBlock = bg Graph.! succB
                                predJ' = Jmp l
                                predBlock' = BBlock predParams predSs predJ'
                            in (Graph.addEdge predB succB $ Graph.deleteEdge predB b $
                                Graph.update predB predBlock' bg, (True, []))))
                (CmpJmp sz predC predSrc1 predSrc2 predL1 predL2, [], Jmp l) ->
                  let Label succB _ = l
                  in (if b == succB
                      then (bg, (False, []))
                      else (case (labelMatchesBlock predL1 b, labelMatchesBlock predL2 b) of
                              (True, True) ->
                                error "simplifyCFG: unfork should have been caught sooner."
                              (True, False) ->
                                let predJ' = CmpJmp sz predC predSrc1 predSrc2 l predL2
                                    predBlock' = BBlock predParams predSs predJ'
                                    bg' = Graph.addEdge predB succB $
                                          Graph.deleteEdge predB b $
                                          Graph.update predB predBlock' bg
                                in (if labelMatchesBlock predL2 succB
                                    then (let (bg'', todos) = tryUnfork bg' predB
                                          in (bg'', (True, todos)))
                                    else (bg', (True, [])))
                              (False, True) ->
                                let predJ' = CmpJmp sz predC predSrc1 predSrc2 predL1 l
                                    predBlock' = BBlock predParams predSs predJ'
                                    bg' = Graph.addEdge predB succB $
                                          Graph.deleteEdge predB b $
                                          Graph.update predB predBlock' bg
                                in (if labelMatchesBlock predL1 succB
                                    then (let (bg'', todos) = tryUnfork bg' predB
                                          in (bg'', (True, todos)))
                                    else (bg', (True, [])))
                              (False, False) -> error "simplifyCFG: edges out of whack"))
                (_, _, _) -> (bg, (False, [])))

        transSrc :: ValueMap -> Src -> Src
        transSrc _ src@(Imm _) = src
        transSrc rMap src@(STmp t) = Map.findWithDefault src t rMap
        transSrc _ src@(FnPtr _) = src

        labelMatchesBlock :: Label -> BlockIdent -> Bool
        labelMatchesBlock (Label b _) b' = b == b'

        {-
          Performs the following optimization:
            "CJmp cmp s1 s2 l1 l2" can be replaced with "Jmp l1" if l1 and l2 are exactly the
            same (including arguments passed).
          This optimization can create more opportunities for skipping, but it leads to more
            tricky code if it recursively calls trySkip itself, so instead it returns a list of
            the nodes to reexamine.
        -}
        tryUnfork :: BlockGraph Block -> BlockIdent
                  -> (BlockGraph Block, [BlockIdent])
        tryUnfork bg b =
          let BBlock params ss j = bg Graph.! b
          in (case j of
                (CmpJmp _ _ _ _ (Label succB1 args1) (Label succB2 args2)) ->
                  if (succB1 /= succB2 || args1 /= args2)
                  then (bg, [])
                  else (let j' = Jmp $ Label succB1 args1
                            block' = BBlock params ss j'
                        in (Graph.update b block' bg, [succB1, b]))
                _ -> (bg, []))


{-
  Performs node merging on the block graph.
-}
doMerge :: BlockGraph Block -> BlockGraph Block
doMerge bg = foldl tryMerge bg $ Graph.verts bg

{-
  Given a map m and block b in the map, tries to merge b into its predecessor in m.
  Succeeds if:
    1. b has exactly one predecessor (predB).
    2. predB has exactly one successor (b).
    3. predB isn't a silly CmpJmp (a silly cmpjmp has both targets the same block)
-}
tryMerge :: BlockGraph Block -> BlockIdent -> BlockGraph Block
tryMerge bg b =
  let (preds, _, _) = Graph.getWithEdges bg b
  in (if (Set.size preds /= 1)
      then bg
      else (let [predB] = Set.toList preds
                BBlock _ _ predJ = bg Graph.! predB
            in (case (predJ) of
                  (Ret _ _) -> error "simplifyCFG: edges out of whack in tryMerge"
                  (Jmp _) -> Graph.contractEdge mergeBlocks predB b bg
                  (CmpJmp _ _ _ _ _ _) -> bg)))
  where mergeBlocks :: Block -> Block -> Block
        mergeBlocks (BBlock params ss _) (BBlock [] ss' j') =
          BBlock params (ss ++ ss') j'
        mergeBlocks _ _ = error "simplifyCFG: edges out of whack in mergeBlocks"
