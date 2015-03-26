{-
  Handles SSA minimization.
-}

module Compile.CodeGen.OptSsa.Minimize (minimize) where

import Compile.Types.S3Asm
import qualified Compile.Graph.Graph as Graph
import Compile.Types.Common (TmpIdent, BlockIdent, Size)
import Compile.CodeGen.OptSsa.Common (ValueMap, BlockGraph)
import qualified Compile.CodeGen.OptSsa.Common as SCommon

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (transpose)
import Data.Maybe

type ArgMap = Map.Map TmpIdent [Src]
type Info1 = ([TmpIdent], ArgMap)
type Graph1 = BlockGraph Info1
type PredArgMap = Map.Map (BlockIdent, TmpIdent) [Src]
type PassMap = Map.Map TmpIdent (Set.Set (BlockIdent, TmpIdent))

{-
  Minimizes the parameter lists of the given blockMap, by lifting it into a directed graph and
  traversing that graph. Returns the modified program, and true iff it made a change to it.

  Data Structures:
    rename map (rMap): maps parameters that have been minimized out to the Srcs they're
                       replaced with. Can be used to check if a parameter has been removed
                       already.
    pred arg map (pArgMap): maps (block, param) to a list of the arguments passed for that
                            param when jumping to that block.
    param pass map (passMap): maps parameters to the places where they're passed as arguments.
                              Used to determine what needs to be updated when a parameter is
                              minimized out.

  Algorithm:
    UPDATE(b, p):
      if p has already been optimized out of b, return
      if p cannot be updated in b, return
      if p can be replaced by s in b:
        add p -> s to rMap
        passes = passMap[p]
        delete p from passMap
        if (s is STmp t and t in passMap):
          remove (b, p) from passMap[t]
          union passes into passMap[t]
        recurse on every pair in passes
    toplevel:
      build rMap, pArgMap, passMap
      call UPDATE(b, p) for every p for every b in the bMap
-}
minimize :: BlockMap -> (BlockMap, Bool)
minimize bMap =
  let bg = SCommon.bgFromBMap buildInfo bMap
      rMap = Map.empty
      pArgMap = getPredArgMap bg
      passMap = getPassMap bg
      pairs = [(b, p) | (b, BBlock ps _ _) <- Map.assocs bMap, (p, _) <- ps]
      (rMap', _) = foldl (update pArgMap) (rMap, passMap) pairs
      rMap'' = flattenRMap rMap'
      bg' = Graph.addMapData (\block -> (\_ -> (block, getParamstoKeep rMap'' block))) bMap bg
  in (Graph.toMap $ Graph.mapWithSuccs (transBlock rMap'') bg', not $ Map.null rMap'')
  where buildInfo :: Block -> Info1
        buildInfo (BBlock params _ j) = (map fst params, argMapFromJIns j)

        argMapFromJIns :: JIns -> ArgMap
        argMapFromJIns (Ret _ _) = Map.empty
        argMapFromJIns (Jmp (Label b args)) = Map.singleton b (map fst args)
        argMapFromJIns (CmpJmp _ _ _ _ (Label b1 a1) (Label b2 a2)) =
          Map.fromList [(b1, map fst a1), (b2, map fst a2)]
        argMapFromJIns (Raise _) = Map.empty

        {-
          Gets the pred arg map ((b, p) -> [a]) by mapping over nodes, each given info for their
          predecessors.
        -}
        getPredArgMap :: Graph1 -> PredArgMap
        getPredArgMap bg =
          Graph.fold Map.union Map.empty $ Graph.mapWithPredsAndVertex getPredArgMapComponent bg

        getPredArgMapComponent :: BlockIdent -> Info1 -> Map.Map BlockIdent Info1
                               -> PredArgMap
        getPredArgMapComponent b (params, aMap) predInfoMap =
          let argLists = map (\(_, predAMap) -> predAMap Map.! b) $ Map.elems predInfoMap
              paramVals = transpose argLists
              defaultParamVals = replicate (length params) []
              paramVals' = if (null paramVals) then defaultParamVals else paramVals
          in Map.fromList [((b, p), args) | (p, args) <- zip params paramVals']

        {-
          Gets the pass map (argTmp -> Set (b, p)) by first finding a map of (t -> Set (b, p))
          for all temps t, then reshaping the domain to only cover parameters, with default
          value for some parameter that's never passed being the empty set.
        -}
        getPassMap :: Graph1 -> PassMap
        getPassMap bg =
          let bg' = Graph.mapWithSuccs getPassMapComponent bg
              passMap = Graph.fold (Map.unionWith Set.union) Map.empty bg'
              defaultPassMap = Map.fromSet (\_ -> Set.empty) $ getAllParams bg
          in Map.union (Map.intersection passMap defaultPassMap) defaultPassMap

        getPassMapComponent :: Info1 -> Map.Map BlockIdent Info1 -> PassMap
        getPassMapComponent (_, aMap) succInfoMap =
          let succInfo = Map.assocs $ Map.map fst succInfoMap
              zippedInfo = map (\(b, params) -> (b, zip (aMap Map.! b) params)) succInfo
              allPairs = [(b, a, p) | (b, pairs) <- zippedInfo, (a, p) <- pairs]
          in Map.fromListWith Set.union $ mapMaybe transPair allPairs

        transPair :: (BlockIdent, Src, TmpIdent)
                  -> Maybe (TmpIdent, Set.Set (BlockIdent, TmpIdent))
        transPair (_, Imm _, _) = Nothing
        transPair (b, STmp argTmp, param) = Just (argTmp, Set.singleton (b, param))
        transPair (_, FnPtr _, _) = Nothing

        getAllParams :: Graph1 -> Set.Set TmpIdent
        getAllParams bg =
          Graph.fold Set.union Set.empty $ Graph.mapStates (Set.fromList . fst) bg


        {-
          Does the recursive updating (i.e. the UPDATE part of the above notes).
        -}
        update :: PredArgMap -> (ValueMap, PassMap) -> (BlockIdent, TmpIdent)
               -> (ValueMap, PassMap)
        update pArgMap (rMap, passMap) (b, p) =
          if p `Map.member` rMap
          then (rMap, passMap)
          else (let args = map (transSrc rMap) $ pArgMap Map.! (b, p)
                    argSet = Set.delete (STmp p) $ Set.fromList args
                in (if Set.size argSet /= 1
                    then (rMap, passMap)
                    else (let [s] = Set.toList argSet -- argSet is known to be a singleton here
                              rMap' = Map.insert p s rMap
                              (Just passSet, passMap') = Map.updateLookupWithKey
                                                                (\_ -> \_ -> Nothing) p passMap
                              passMap'' =
                                (case s of
                                  (Imm _) -> passMap'
                                  (STmp t) ->
                                    Map.adjust (Set.union passSet.Set.delete (b, p)) t passMap'
                                  (FnPtr _) -> passMap')
                          in Set.foldl (update pArgMap) (rMap', passMap'') passSet)))


        {-
          Given a map tmp -> src, follows the chains of srcs as far as possible to produce the
          "final" mapping for each tmp (so {3 -> 5, 5 -> 7} would map to {3 -> 7, 5 -> 7}).
          NOTE: this flatten is theoretically slow worst-case, and could probably be improved
                if it's ever needed.
        -}
        flattenRMap :: ValueMap -> ValueMap
        flattenRMap rMap = Map.map (lookupIn rMap) rMap

        lookupIn :: ValueMap -> Src -> Src
        lookupIn _ src@(Imm _) = src
        lookupIn rMap src@(STmp t) =
          case (Map.lookup t rMap) of
            Nothing -> src
            Just s -> lookupIn rMap s
        lookupIn _ src@(FnPtr _) = src

        {-
          Given a rename map and a block, makes a list of booleans indicating which parameters
          of the block to keep.
        -}
        getParamstoKeep :: ValueMap -> Block -> [Bool]
        getParamstoKeep rMap (BBlock params _ _) =
          map (\(p, _) -> p `Map.notMember` rMap) params


        {-
          Given the rename map, a block, info about which parameters to keep, and similar info
          about each of the block's successors, updates the block to its simplified format.
        -}
        transBlock :: ValueMap -> (Block, [Bool])
                   -> Map.Map BlockIdent (Block, [Bool]) -> Block
        transBlock rMap (BBlock params ss j, toKeep) succInfoMap =
          let keepMap = Map.map snd succInfoMap
              params' = [(p, sz) | ((p, sz), b) <- zip params toKeep, b]
          in BBlock params' (map (transSIns rMap) ss) $ transJIns rMap keepMap j

        transSIns :: ValueMap -> SIns -> SIns
        transSIns rMap (Binop srcSz dstSz op l r dst) =
          Binop srcSz dstSz op (transSrc rMap l) (transSrc rMap r) dst
        transSIns rMap (Set sz src dst) =
          Set sz (transSrc rMap src) dst
        transSIns rMap (Load sz mem dst) =
          Load sz (transMem rMap mem) dst
        transSIns rMap (Store sz src mem) =
          Store sz (transSrc rMap src) (transMem rMap mem)
        transSIns rMap (Memcpy bytes src) =
          Memcpy bytes (transSrc rMap src)
        transSIns rMap (Call sz name args dst) =
          Call sz name (map (transArg rMap) args) dst
        transSIns rMap (CallPtr sz fnPtrSrc args dst) =
          CallPtr sz (transSrc rMap fnPtrSrc) (map (transArg rMap) args) dst
        transSIns rMap (CheckNonZero sz src err) =
          CheckNonZero sz (transSrc rMap src) err
        transSIns rMap (CheckNonNeg sz src err) =
          CheckNonNeg sz (transSrc rMap src) err
        transSIns rMap (CheckEqual sz src1 src2 err) =
          CheckEqual sz (transSrc rMap src1) (transSrc rMap src2) err
        transSIns rMap (CheckDivOverflow l r) =
          CheckDivOverflow (transSrc rMap l) (transSrc rMap r)
        transSIns rMap (CheckShiftMax src) =
          CheckShiftMax (transSrc rMap src)
        transSIns rMap (CheckArrEnd addrSrc idxSrc) =
          CheckArrEnd (transSrc rMap addrSrc) (transSrc rMap idxSrc)


        transJIns :: ValueMap -> Map.Map BlockIdent [Bool] -> JIns -> JIns
        transJIns rMap _ (Ret sz src) = Ret sz (transSrc rMap src)
        transJIns rMap keepMap (Jmp l) = Jmp (transLabel rMap keepMap l)
        transJIns rMap keepMap (CmpJmp sz cmp src1 src2 l1 l2) =
          let (src1', src2') = (transSrc rMap src1, transSrc rMap src2)
              (l1', l2') = (transLabel rMap keepMap l1, transLabel rMap keepMap l2)
          in CmpJmp sz cmp src1' src2' l1' l2'
        transJIns _ _ j@(Raise _) = j

        transLabel :: ValueMap -> Map.Map BlockIdent [Bool] -> Label -> Label
        transLabel rMap keepMap (Label b args) =
          (Label b [transArg rMap a | (a, boo) <- zip args (keepMap Map.! b), boo])

        transArg :: ValueMap -> (Src, Size) -> (Src, Size)
        transArg rMap (src, sz) = (transSrc rMap src, sz)

        transSrc :: ValueMap -> Src -> Src
        transSrc _ src@(Imm x) = src
        transSrc rMap src@(STmp t) = Map.findWithDefault src t rMap
        transSrc _ src@(FnPtr fnName) = src

        transMem :: ValueMap -> Mem -> Mem
        transMem rMap (ArrMem elemSz addrSrc indSrc o) =
          ArrMem elemSz (transSrc rMap addrSrc) (transSrc rMap indSrc) o
        transMem rMap (PtrMem addrSrc o) =
          PtrMem (transSrc rMap addrSrc) o

-- NOTE: the below code is my original, bottom-up implementation of minimization that didn't
--       work fully on loops. It does (I think) less work than the above for the rest of the
--       nodes though, so I'm keeping it around just in case we decide to use it later for some
--       reason.
{-
type ArgListMap = Map.Map BlockIdent [Src]
type Info1 = ([SIns], JIns, [TmpIdent], ArgListMap)
type MInfo = ([Bool], ArgListMap)
type Info2 = ([SIns], JIns, [TmpIdent], [Bool], ArgListMap)
type MIMap = Map.Map BlockIdent MInfo
type Graph1 = Graph.Graph BlockIdent Info1
type Graph2 = Graph.Graph BlockIdent Info2

{-
  Attempts to minimize the generated SSA parameter lists. Currently it doesn't handle loops
  fully, but in future it can be extended to full minimization. The current algorithm is roughly
  as follows:
    1. Make a global rename map, and per-node map from successor to the arguments passed to it.
    2. For each block, DFS backwards to ensure that its predecessors are resolved, then note
       which of its parameters can be removed and add their updated values to the global rename
       map. If your predecessor has been seen by the DFS but hasn't been resolved yet (i.e.
       you're in a cycle), assume for safety that all its args change to unfarmiliar values.
    3. After the fact, remove all parameters that you marked for removal.

  FUTURE MODIFICATIONS:
    1. Remove alMap from the iMap, and compute it on the fly as needed.
    2. Add minimization through loops.

  FUTURE EXTENSIONS:
  1. Minimize all blocks, not accounting for cycles.
  2. Add a pass at the end to account for cycles.
-}
minimize :: BlockMap -> BlockMap
minimize bMap =
  let bg = Graph.fromMapWith buildState successorsOf bMap
      (pMap, iMap) = Graph.dfsFoldBackwardsWithPreds minimizeNode (Map.empty, Map.empty) bg
      bg' = Graph.addMapData addMinimizationData iMap bg
  in Graph.toMapWith (buildBlock pMap) $ Graph.mapWithSuccs processDeletions bg'
  where {-
          Builds the state used in the block graph (see above for details).
        -}
        buildState :: Block -> Info1
        buildState (BBlock params ss j) = (ss, j, params, alMapFromJIns j)

        alMapFromJIns :: JIns -> ArgListMap
        alMapFromJIns (Ret _) = Map.empty
        alMapFromJIns (Jmp (Label b args)) = Map.singleton b args
        alMapFromJIns (CmpJmp _ _ _ (Label b1 a1) (Label b2 a2)) =
          Map.fromList [(b1, a1), (b2, a2)]

        {-
          Minimizes the param set for a node, given that its parents are all fully minimized.
          Minimization rules:
            1. If a node has NO predecessors, leave it as-is (it's probably the root node).
            2. If a node has 1+ predecessors, and they all pass in the same value v for some
               parameter p:
                a. Set p's entry in the toPreserve list to be false.
                b. Add (p -> v) to the global propogation map.
                c. Use the propogation map to update your argListMap.
          At the end, the argListMaps, toPreserve lists, and propogation map can be used to
          actually do the minimization.
        -}
        minimizeNode :: (PropogateMap, MIMap) -> Map.Map BlockIdent Info1 -> BlockIdent -> Info1
                     -> (PropogateMap, MIMap)
        minimizeNode (pMap, iMap) predInfoMap b (ss, j, params, argMap) =
          if (Map.size predInfoMap == 0)
          then (pMap, Map.insert b (replicate (length params) True, argMap) iMap)
          else if any (`Map.notMember` iMap) $ Map.keys predInfoMap
          then (pMap, Map.insert b (replicate (length params) True, argMap) iMap)
          else (let argLists = map (getArgListFor b iMap) $ Map.keys predInfoMap
                    paramVals = map Set.fromList $ transpose argLists
                    toPreserve = map (\s -> Set.size s > 1) paramVals
                    pMap' = foldl processParam pMap $ zip params paramVals
                    argMap' = Map.map (map (transSrc pMap')) argMap
                in (pMap', Map.insert b (toPreserve, argMap') iMap))

        {-
          Gets the list of args passed to the first block from the second block by consulting the
          given info map, assumed to contain the required arg lists.
        -}
        getArgListFor :: BlockIdent -> MIMap -> BlockIdent -> [Src]
        getArgListFor b iMap b' =
          let (_, alMap) = iMap Map.! b'
          in alMap Map.! b

        processParam :: PropogateMap -> (TmpIdent, Set.Set Src) -> PropogateMap
        processParam pMap (param, argVals) =
          if Set.size argVals > 1
          then pMap
          else (let value = Set.findMin argVals -- argVals ia a singleton here
                    value' = (case value of
                                (Imm x) -> (Imm x)
                                (STmp t) -> Map.findWithDefault (STmp t) t pMap)
                in Map.insert param value' pMap)

        {-
          Unifies initial info and what was computed by propogation.
        -}
        addMinimizationData :: MInfo -> Info1 -> Info2
        addMinimizationData (toPreserve, alMap) (ss, j, params, _) =
          (ss, j, params, toPreserve, alMap)

        {-
          Updates the succesor label lists for each node by pulling the new parameter lists from
          the successor nodes and using them for filtering.
        -}
        processDeletions :: Info2 -> Map.Map BlockIdent Info2 -> Info1
        processDeletions (ss, j, params, toPreserve, argListMap) sMap =
          let params' = filterBy params toPreserve
              argListMap' = Map.mapWithKey (filterArgList sMap) argListMap
          in (ss, j, params', argListMap')

        filterArgList :: Map.Map BlockIdent Info2 -> BlockIdent -> [Src] -> [Src]
        filterArgList m b args =
          let (_, _, _, toPreserve, _) = m Map.! b
          in filterBy args toPreserve

        filterBy :: [a] -> [Bool] -> [a]
        filterBy xs bs = [x | (x, b) <- zip xs bs, b]

        {-
          Reconstructs a single node of the block graph, using the given propogation map to make
          all necessary updates (but NOT do further constant/copy propogation beyond that).
          NOTE: this code is extremely similar to the propogation code above and in Propogate,
                and should really be unified in some way.
        -}
        buildBlock :: PropogateMap -> Info1 -> Block
        buildBlock pMap (ss, j, params, argListMap) =
          BBlock params (map (transSIns pMap) ss) (transJIns pMap argListMap j)

        transSIns :: PropogateMap -> SIns -> SIns
        transSIns pMap (Binop op l r dst) =
          Binop op (transSrc pMap l) (transSrc pMap r) dst
        transSIns pMap (Set src dst) = Set (transSrc pMap src) dst
        transSIns pMap (Call name srcs dst) =
          Call name (map (transSrc pMap) srcs) dst
        transSIns pMap (Assert src) = Assert (transSrc pMap src)

        transJIns :: PropogateMap -> ArgListMap -> JIns -> JIns
        transJIns pMap _ (Ret src) = Ret $ transSrc pMap src
        transJIns _ alMap (Jmp l) = Jmp $ transLabel alMap l
        transJIns pMap alMap (CmpJmp cmp src1 src2 l1 l2) =
          let (src1', src2') = (transSrc pMap src1, transSrc pMap src2)
              (l1', l2') = (transLabel alMap l1, transLabel alMap l2)
          in CmpJmp cmp src1' src2' l1' l2'

        transLabel :: ArgListMap -> Label -> Label
        transLabel alMap (Label b _) = Label b $ alMap Map.! b

        transSrc :: PropogateMap -> Src -> Src
        transSrc _ (Imm x) = Imm x
        transSrc pMap (STmp t) = Map.findWithDefault (STmp t) t pMap

-}
