{-
  Handles a number of optimizations, all of which are of the form: "starting at the top of the
  program, loop down with a state, use that state to simplify each instruction you see, and use
  the simplified instruction in turn to update your state." Optimizations include:
  --> constant propogation
  --> constant folding
  --> copy propogation
  --> common subexpression elimination
  --> redundant read elimination (like CSE, but with somewhat fancier safety checks)
  --> arithmetic optimizations (x + 0 -> x, x * 1 -> x, etc)
-}

module Compile.CodeGen.OptSsa.Propogate (propogate) where

import Compile.Types.S3Asm
import Compile.Types.Common (TmpIdent, BlockIdent, Size(..), Error(..), boolToInt32)
import qualified Compile.Types.Common as Common
import qualified Compile.Graph.Graph as Graph
import qualified Compile.CodeGen.OptSsa.DTree as DTree
import Compile.CodeGen.OptSsa.Common

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Data.Either as Either
import Data.List (mapAccumL, break, partition)
import Data.Maybe
import Data.Int (Int32)
import Data.Bits (xor, shiftL, shiftR, (.|.), (.&.))

type CSEMap = Map.Map (Binop, Src, Src) TmpIdent
type RREMap = Map.Map Mem TmpIdent

{-
  The data passed around during propogation.
-}
type PropState = (ValueMap, CSEMap, RREMap)

type InterveningBlocksMap = Map.Map BlockIdent [BlockIdent]
type MemInterferenceMap = Map.Map BlockIdent (Maybe [Mem])


emptyPropState :: PropState
emptyPropState = (Map.empty, Map.empty, Map.empty)

{-
  Does the above optimizations on the given block map.
  NOTE: most of these optimizations require propogating information downward from earlier in
        the program, and some care must be taken to ensure that the information remains valid.
        The way this handles it is by building the dominance tree (i.e. the tree where each
        block's parent is the LCA of their parents in the normal graph) and mapping over it,
        since all the propogation info valid for use at the start of a block must be live at
        the end of the most recent block that dominates it. This isn't enough for redundant
        read elimination, which must also verify that none of the block between this one and
        its dominating ancestor wrote to the memory address in question; it handles the problem
        by conservatively assuming that none of the intervening memory writes or function calls
        will be propogated away, and relying on the external optimization scheduler to run it
        to saturation.
-}
propogate :: BlockMap -> (BlockMap, Bool)
propogate bMap =
  let dt = DTree.fromBMap bMap
      ibMap = getInterveningBlocksMap bMap dt
      iMap = getInterferenceMap bMap
      (dt', didSomething) = DTree.mapAccumTopDown (processBlock ibMap iMap) emptyPropState dt
  in (DTree.toBMap dt', didSomething)

{-
  From a dominance tree and blockMap, computes for each b the set of blocks between b and its
  dominant ancestor, exclusive.
-}
getInterveningBlocksMap :: BlockMap -> DTree.DTree -> InterveningBlocksMap
getInterveningBlocksMap bMap dt =
  let pairs = [(succ, b) | (b, succs) <- Map.toList $ DTree.toDEdgeMap dt, succ <- succs]
      predMap = getPredecessorMap bMap
      m = Map.fromList [(b, getInterveningBlocks predMap b ansc) | (b, ansc) <- pairs]
      defaultM = Map.map (\_ -> []) bMap
  in Map.union m defaultM
  where getInterveningBlocks :: Map.Map BlockIdent [BlockIdent] -> BlockIdent -> BlockIdent
                             -> [BlockIdent]
        getInterveningBlocks predMap b ansc =
          Set.toList $ foldl (gIBRec predMap b ansc) Set.empty $ predMap Map.! b
        gIBRec :: Map.Map BlockIdent [BlockIdent] -> BlockIdent -> BlockIdent
               -> Set.Set BlockIdent ->BlockIdent -> Set.Set BlockIdent
        gIBRec predMap b ansc seen b'
          | b' == b || b' == ansc || b' `Set.member` seen = seen
          | otherwise = foldl (gIBRec predMap b ansc) (Set.insert b' seen) (predMap Map.! b')

{-
  Scans each block in the given block map to determine which memory locations they potentially
  overwrite:
    Nothing -> it called a function with a non-constant argument, so it overwrites everything
    Just [ms] -> the following memory locations were written to.
-}
getInterferenceMap :: BlockMap -> MemInterferenceMap
getInterferenceMap = Map.map getBlockInterference
  where getBlockInterference :: Block -> Maybe [Mem]
        getBlockInterference (BBlock _ ss _) =
          let cOpts = catMaybes $ map getInsInterference ss
          in (if any isNothing cOpts
              then Nothing
              else Just $ map fromJust cOpts)

        getInsInterference :: SIns -> Maybe (Maybe Mem)
        getInsInterference (Binop _ _ _ _ _ _) = Nothing
        getInsInterference (Set _ _ _) = Nothing
        getInsInterference (Load _ _ _) = Nothing
        getInsInterference (Store _ _ mem) = Just $ Just mem
        getInsInterference (Memcpy _ _) = Nothing
        getInsInterference (Call _ _ args _)
          | (any (isTmp . fst) args) = Just Nothing
          | otherwise = Nothing
        getInsInterference (CallPtr _ _ args _)
          | (any (isTmp . fst) args) = Just Nothing
          | otherwise = Nothing
        getInsInterference (CheckNonZero _ _ _) = Nothing
        getInsInterference (CheckNonNeg _ _ _) = Nothing
        getInsInterference (CheckEqual _ _ _ _) = Nothing
        getInsInterference (CheckDivOverflow _ _) = Nothing
        getInsInterference (CheckShiftMax _) = Nothing
        getInsInterference (CheckArrEnd _ _) = Nothing

{-
  Given (b, block), and the state at the end of the least dominating ancestor of b, tries to
  simplify block. Returns the new state, new block, and whether or not any meaningful
  simplfication occured.
-}
processBlock :: InterveningBlocksMap -> MemInterferenceMap -> PropState -> BlockIdent -> Block
             -> ((PropState, Bool), Block)
processBlock ibMap iMap state b (BBlock params ss j) =
  let (state', insOpts) = mapAccumL processSIns (filterState ibMap iMap b state) ss
      (ss', rest) = break isRight $ catMaybes insOpts
      (j', outEdgesChanged) =
        case rest of
          [] -> transJIns state' j
          ((Right j') : _) -> (j', isNonterminal j)
      block' = BBlock params (Either.lefts ss') j'
  in ((state', outEdgesChanged || not (stateEmpty state')), block')
  where {-
          Uses the given information (the list of intervening blocks and potential memory
          writes from each one) to filter the state.
        -}
        filterState :: InterveningBlocksMap -> MemInterferenceMap -> BlockIdent -> PropState
                    -> PropState
        filterState ibMap iMap b (pMap, cseMap, rreMap) =
          case (partition isJust $ map (iMap Map.!) $ ibMap Map.! b) of
            (_, x:_) -> (pMap, cseMap, Map.empty)
            (cOpts, []) ->
              let writeSet = Set.fromList $ map getCanonicalMem $ concat $ catMaybes cOpts
              in (pMap, cseMap, Map.filterWithKey (\mem -> \_ -> noAliasIn mem writeSet) rreMap)
          where noAliasIn :: Mem -> Set.Set Mem -> Bool
                noAliasIn mem memSet = (getCanonicalMem mem) `Set.notMember` memSet

                getCanonicalMem :: Mem -> Mem
                getCanonicalMem (PtrMem addrSrc o) = PtrMem (getCanonicalSrc addrSrc) o
                getCanonicalMem (ArrMem esz arrSrc idxSrc o) =
                  ArrMem esz (getCanonicalSrc arrSrc) (getCanonicalSrc idxSrc) o

                getCanonicalSrc (Imm x) = Imm x
                getCanonicalSrc (STmp _) = STmp (-1)
                getCanonicalSrc (FnPtr fName) = FnPtr fName

        {-
          Applies propogation to try to simplify the given instructions. Returns the new state,
          as well as:
            Nothing, if the instruction can be removed
            Just (Left s'), if the instruction should be replaced by s'
            Just (Right j'), if the block should be truncated here, and the new jIns be j'
        -}
        processSIns :: PropState -> SIns -> (PropState, Maybe (Either SIns JIns))
        processSIns state s =
          let s' = simplifyIns state $ updateInsArgs state s
              state' = updateState state s'
              sOpt = tryRemoveIns s'
          in (state', sOpt)

        {-
          Given a propState and jIns, returns the updated JIns, as well as a flag indicating
          whether or not the type of jump changed.
        -}
        transJIns :: PropState -> JIns -> (JIns, Bool)
        transJIns state (Ret sz src) = (Ret sz (transSrc state src), False)
        transJIns state (Jmp l) = (Jmp $ transLabel state l, False)
        transJIns state (CmpJmp sz cmp sl sr lt lf) =
          let (sl', sr') = (transSrc state sl, transSrc state sr)
              (lt', lf') = (transLabel state lt, transLabel state lf)
          in case (tryFoldCmp cmp sl' sr') of
            Just (Imm x) -> (Jmp (if Common.int32ToBool x then lt' else lf'), True)
            Nothing -> (CmpJmp sz cmp sl' sr' lt' lf', False)
        transJIns state j@(Raise _) = (j, False)

        {-
          Tries to fold a comparison into a single value.
        -}
        tryFoldCmp :: Common.Cmp -> Src -> Src -> Maybe Src
        tryFoldCmp cmp (Imm x) (Imm y) =
          Just $ Imm $ Common.boolToInt32 $ Common.applyCmp cmp x y
        tryFoldCmp Common.E (FnPtr fnName1) (FnPtr fnName2) =
          Just $ Imm $ Common.boolToInt32 $ fnName1 == fnName2
        tryFoldCmp Common.NE (FnPtr fnName1) (FnPtr fnName2) =
          Just $ Imm $ Common.boolToInt32 $ fnName1 /= fnName2
        tryFoldCmp cmp (FnPtr _) (Imm _) =
          Just $ Imm $ if (cmp == Common.E) then 0 else 1
        tryFoldCmp cmp (Imm _) (FnPtr _) =
          Just $ Imm $ if (cmp == Common.E) then 0 else 1
        tryFoldCmp _ _ _ = Nothing

        {-
          Translates the given label to its post-propogation form.
        -}
        transLabel :: PropState -> Label -> Label
        transLabel state (Label b args) = Label b $ map (transArg state) args

        {-
          Returns whether or not the given JIns is a non-terminal instruction.
        -}
        isNonterminal :: JIns -> Bool
        isNonterminal (Ret _ _) = False
        isNonterminal (Jmp _) = True
        isNonterminal (CmpJmp _ _ _ _ _ _) = True
        isNonterminal (Raise _) = False

        isRight :: Either a b -> Bool
        isRight (Right _) = True
        isRight (Left _) = False
        
        stateEmpty :: PropState -> Bool
        stateEmpty (pMap, _, _) = Map.null pMap


{-
  Does constant and copy propogation together by looping through the blocks in ascending order,
  building a map from temps to the values they hold and using it to simplify later instructions.
  The requirement in all cases is that only information agreed upon by ALL parents of a block can
  be used within that block, so a map from block number to the translation state at the END of
  that block is constructed in the process; each block looks up its parents (ignoring back-edges,
  see below), combines their information, then proceeds as normal.

  Note that we can ignore back-edges (i.e. parents below you) in the above algorithm:
  --> no variables written to down there exist up here, so constant/copy propogation is fine
  --> any arithmetic/memory expression that exists in both places might as well be handled here
      and simplified away down there instead, so CSE/RRE are fine too.
-}
{-
propogate :: BlockMap -> (BlockMap, Bool)
propogate bMap =
  let bg = bgFromBMap id bMap
      initialState = (Map.empty, False)
      ((sMap, cfgChanged), bg') = Graph.mapAccumWithPredSetAndKey processBlock initialState bg
  in (Graph.toMap bg', cfgChanged || not (allStatesEmpty sMap))
  where allStatesEmpty :: PropStateMap -> Bool
        allStatesEmpty stateMap = Map.fold (&&) True $ Map.map stateEmpty stateMap
        stateEmpty :: PropState -> Bool
        stateEmpty (pMap, _, _) = Map.null pMap
-}
{-
  Does the above propogation/folding for a single block, by first intersecting the PropStates of
  its parents to get its own PropState, using that state to optimize the given block, and
  recording it at the end.

  NOTE: this would be more straightforward, but it's possible for a straight-line instruction to
        turn into a jump instruction, (and so short-circuit the entire block). We handle this by
        propogating over the whole thing anyway (blocks aren't usually too too long, so this
        should be fine), then looking through after the fact to find the first instruction that
        would've definitely caused a crash.
-}{-
processBlock :: (PropStateMap, Bool) -> BlockIdent -> Block -> Set.Set BlockIdent
             -> ((PropStateMap, Bool), Block)
processBlock (stateMap, cfgChanged) b (BBlock params ss j) predSet =
  let state = getStartingState stateMap predSet
      (state', insOpts) = mapAccumL processSIns state ss
      (ss', rest) = break isRight $ catMaybes insOpts
      (j', outEdgesChanged) =
        case rest of
          [] -> transJIns state' j
          ((Right j') : _) -> (j', isNonterminal j)
      block' = BBlock params (Either.lefts ss') j'
  in ((Map.insert b state' stateMap, cfgChanged || outEdgesChanged), block')
  where getStartingState :: PropStateMap -> Set.Set BlockIdent -> PropState
        getStartingState stateMap preds =
          let predList = Set.toList preds
              propStates = map (\b -> Map.findWithDefault emptyPropState b stateMap) $ predList
          in (case propStates of
                [] -> emptyPropState
                _ -> foldl1 mergePropStates propStates)

        mergePropStates :: PropState -> PropState -> PropState
        mergePropStates (pMap, cseMap, rreMap) (pMap', cseMap', rreMap') =
          (Map.union pMap pMap', exactIntersection cseMap cseMap',
           exactIntersection rreMap rreMap')

        {-
          Computes the intersection of the two maps, where both the keys AND values have to be
          equal for it to be included in the output.
        -}
        exactIntersection :: (Ord k, Ord v) => Map.Map k v -> Map.Map k v -> Map.Map k v
        exactIntersection m1 m2 =
          let (s1, s2) = (Set.fromAscList $ Map.assocs m1, Set.fromAscList $ Map.assocs m2)
          in  Map.fromAscList $ Set.toList $ Set.intersection s1 s2
-}


{-
  Uses the propogation information to update the arguments to the given instruction.
-}
updateInsArgs :: PropState -> SIns -> SIns
updateInsArgs state (Binop srcSz dstSz op l r dst) =
  Binop srcSz dstSz op (transSrc state l) (transSrc state r) dst
updateInsArgs state (Set sz src dst) =
  Set sz (transSrc state src) dst
updateInsArgs state (Load sz mem dst) =
  Load sz (transMem state mem) dst
updateInsArgs state (Store sz src mem) =
  Store sz (transSrc state src) (transMem state mem)
updateInsArgs state (Memcpy bytes src) =
  Memcpy bytes (transSrc state src)
updateInsArgs state (Call sz fn args dst) =
  Call sz fn (map (transArg state) args) dst
updateInsArgs state (CallPtr sz fnPtrSrc args dst) =
  CallPtr sz (transSrc state fnPtrSrc) (map (transArg state) args) dst
updateInsArgs state (CheckNonZero sz src err) =
  CheckNonZero sz (transSrc state src) err
updateInsArgs state (CheckNonNeg sz src err) =
  CheckNonNeg sz (transSrc state src) err
updateInsArgs state (CheckEqual sz src1 src2 err) =
  CheckEqual sz (transSrc state src1) (transSrc state src2) err
updateInsArgs state (CheckDivOverflow l r) =
  CheckDivOverflow (transSrc state l) (transSrc state r)
updateInsArgs state (CheckShiftMax src) =
  CheckShiftMax (transSrc state src)
updateInsArgs state (CheckArrEnd s1 s2) =
  CheckArrEnd (transSrc state s1) (transSrc state s2)


{-
  Simplifies the given instruction as much as possible, without doing anything that would
  require a state update or a nonlocal change (so "t <- 5" is unmodified, as is
  "CheckNonNull 0"). Currenty does:
  --> constant folding
  --> arithmetic simplification (x + 0 => x, etc)
  --> common subexpression elimination
  --> CallPtr(*foo)(...) -> foo(...)
-}
simplifyIns :: PropState -> SIns -> SIns
simplifyIns state s@(Binop srcSz dstSz op l r dst) =
  case (tryFold op l r, tryArithSimp op l r, tryCSE state op l r) of
          (Just src', _, _) -> Set dstSz src' dst
          (Nothing, Just src', _) -> Set dstSz src' dst
          (Nothing, Nothing, Just src') -> Set dstSz src' dst
          (Nothing, Nothing, Nothing) -> s
simplifyIns state s@(Load sz mem dst) =
  case (tryRRE state mem) of
    (Just src') -> Set sz src' dst
    Nothing -> s
simplifyIns state (CallPtr sz (FnPtr name) args dst) = Call sz name args dst
simplifyIns _ s = s


{-
  Uses the given instruction to update the propogation state.
-}
updateState :: PropState -> SIns -> PropState
updateState (pMap, cseMap, rreMap) (Binop _ _ op l r (DTmp tDst)) =
  let (l', r') = getCanonicalOperandOrder op l r
  in (pMap, Map.insert (op, l', r') tDst cseMap, rreMap)
updateState (pMap, cseMap, rreMap) (Set _ src (DTmp tDst)) =
  (Map.insert tDst src pMap, cseMap, rreMap)
updateState (pMap, cseMap, rreMap) (Load _ mem (DTmp tDst)) =
  (pMap, cseMap, Map.insert mem tDst rreMap)
updateState (pMap, cseMap, rreMap) (Store _ src mem) =
  (pMap, cseMap, Map.filterWithKey (keepMem mem src) rreMap)
  where keepMem mem (Imm _) mem' tDst = areNotAliases mem mem'
        keepMem mem (STmp tSrc) mem' tDst = areNotAliases mem mem' || tSrc == tDst
        areNotAliases (PtrMem addrSrc1 o1) (PtrMem addrSrc2 o2) =
          srcsDifferent addrSrc1 addrSrc2 || o1 /= o2
        areNotAliases (ArrMem esz1 arrSrc1 iSrc1 o1) (ArrMem esz2 arrSrc2 iSrc2 o2) =
          esz1 /= esz2 || srcsDifferent arrSrc1 arrSrc2 || srcsDifferent iSrc1 iSrc2 || o1 /= o2
        areNotAliases _ _ = True
        srcsDifferent (Imm a) (Imm b) = a /= b
        srcsDifferent _ _ = False
updateState state (Memcpy _ src) = state
updateState state@(pMap, cseMap, rreMap) (Call _ _ args _)
  | any (isTmp . fst) args = (pMap, cseMap, Map.empty)
  | otherwise = state
updateState state@(pMap, cseMap, rreMap) (CallPtr _ _ args _)
  | any (isTmp . fst) args = (pMap, cseMap, Map.empty)
  | otherwise = state
updateState state (CheckNonZero _ _ _) = state
updateState state (CheckNonNeg _ _ _) = state
updateState state (CheckEqual _ _ _ _) = state
updateState state (CheckDivOverflow _ _) = state
updateState state (CheckShiftMax _) = state
updateState state (CheckArrEnd _ _) = state


{-
  Takes a fully simplified instruction and sees whether or not it can be removed.
  Rules:
  --> Set src dst can be removed, since it's handled by constant/copy propogation.
  --> Assert/Check* that's known to fail can be turned into an error.
  --> Assert/Check* that's known to pass can be removed.
-}
tryRemoveIns :: SIns -> Maybe (Either SIns JIns)
tryRemoveIns s@(Binop _ _ _ _ _ _) = Just $ Left s
tryRemoveIns s@(Set _ _ _) = Nothing
tryRemoveIns s@(Load _ _ _) = Just $ Left s
tryRemoveIns s@(Store _ _ _) = Just $ Left s
tryRemoveIns s@(Memcpy _ _) = Just $ Left s
tryRemoveIns s@(Call _ _ _ _) = Just $ Left s
tryRemoveIns s@(CallPtr _ _ _ _) = Just $ Left s
tryRemoveIns s@(CheckNonZero _ (Imm 0) err) = Just $ Right $ Raise err
tryRemoveIns s@(CheckNonZero _ (Imm _) _) = Nothing
tryRemoveIns s@(CheckNonZero _ _ _) = Just $ Left s
tryRemoveIns s@(CheckNonNeg _ (Imm x) err)
  | x < 0 = Just $ Right $ Raise err
  | otherwise = Nothing
tryRemoveIns s@(CheckNonNeg _ _ _) = Just $ Left s
tryRemoveIns s@(CheckEqual _ (Imm x) (Imm y) err)
  | x == y = Nothing
  | otherwise = Just $ Right $ Raise err
tryRemoveIns s@(CheckEqual _ (STmp t1) (STmp t2) _)
  | t1 == t2 = Nothing
  | otherwise = Just $ Left s
tryRemoveIns s@(CheckEqual _ (FnPtr fnName1) (FnPtr fnName2) err)
  | fnName1 == fnName2 = Nothing
  | otherwise = Just $ Right $ Raise err
tryRemoveIns s@(CheckEqual _ _ _ _) = Just $ Left s
tryRemoveIns s@(CheckDivOverflow (Imm x) (Imm (-1)))
  | x == minInt = Just $ Right $ Raise ArithError
  | otherwise = Nothing
tryRemoveIns s@(CheckDivOverflow _ (Imm (-1))) = Just $ Left s
tryRemoveIns s@(CheckDivOverflow _ (Imm _)) = Nothing
tryRemoveIns s@(CheckDivOverflow (Imm x) _)
  | x == minInt = Just $ Left s
  | otherwise = Nothing
tryRemoveIns s@(CheckDivOverflow _ _) = Just $ Left s
tryRemoveIns s@(CheckShiftMax (Imm x))
  | x >= 32 = Just $ Right $ Raise ArithError
  | otherwise = Nothing
tryRemoveIns s@(CheckShiftMax _) = Just $ Left s
tryRemoveIns s@(CheckArrEnd _ _) = Just $ Left s


{-
  Given a binop and two sources, tries to combine them into a single source.
-}
tryFold :: Binop -> Src -> Src -> Maybe Src
tryFold (ArithOp aOp) (Imm x) (Imm y) =
  case aOp of
    Common.AAdd -> Just $ Imm (x + y)
    Common.ASub -> Just $ Imm (x - y)
    Common.AMul -> Just $ Imm (x * y)
    Common.AAnd -> Just $ Imm (x .&. y)
    Common.AOr -> Just $ Imm (x .|. y)
    Common.AXor -> Just $ Imm  (x `xor` y)
    Common.ADiv -> if (y /= 0 && (x /= minInt || y /= -1))
      then Just $ Imm (x `quot` y)
      else Nothing
    Common.AMod -> if (y /= 0 && (x /= minInt || y /= -1))
      then Just $ Imm (x `rem` y)
      else Nothing
    Common.AShL -> if (0 <= y && y < 32)
      then Just $ Imm  (x `shiftL` (fromIntegral y))
      else Nothing
    Common.AShR -> if (0 <= y && y < 32)
      then Just $ Imm (x `shiftR` (fromIntegral y))
      else Nothing
tryFold (CmpOp (Common.CmpOp c)) (Imm x) (Imm y) =
  Just $ Imm $ Common.boolToInt32 $ Common.applyCmp c x y
tryFold (CmpOp (Common.CmpOp c)) (STmp t1) (STmp t2)
  | t1 == t2 =
    case c of
      Common.L -> Just $ Imm 0
      Common.LE -> Just $ Imm 1
      Common.E -> Just $ Imm 1
      Common.NE -> Just $ Imm 0
      Common.GE -> Just $ Imm 1
      Common.G -> Just $ Imm 0
  | otherwise = Nothing
tryFold (CmpOp (Common.CmpOp Common.E)) (FnPtr fnName1) (FnPtr fnName2) =
  Just $ Imm $ Common.boolToInt32 $ fnName1 == fnName2
tryFold (CmpOp (Common.CmpOp Common.NE)) (FnPtr fnName1) (FnPtr fnName2) =
  Just $ Imm $ Common.boolToInt32 $ fnName1 /= fnName2
tryFold (CmpOp (Common.CmpOp c)) (FnPtr _) (Imm _) =
  Just $ Imm $ if (c == Common.E) then 0 else 1
tryFold (CmpOp (Common.CmpOp c)) (Imm _) (FnPtr _) =
  Just $ Imm $ if (c == Common.E) then 0 else 1
tryFold _ _ _ = Nothing


{-
  Given a binop and two sources, tries to apply arithmetic identites to turn it into one
  source.
  Examples:
    x + 0  =>  x
    x - 0  =>  x
    x * 1  =>  x
    x / 1  =>  x
    ...
-}
tryArithSimp :: Binop -> Src -> Src -> Maybe Src
tryArithSimp (ArithOp Common.AAdd) (Imm 0) r = Just r
tryArithSimp (ArithOp Common.AAdd) l (Imm 0) = Just l
tryArithSimp (ArithOp Common.ASub) l (Imm 0) = Just l
tryArithSimp (ArithOp Common.ASub) (STmp lT) (STmp rT) =
  (if lT == rT then Just (Imm 0) else Nothing)
tryArithSimp (ArithOp Common.AMul) (Imm 0) r = Just (Imm 0)
tryArithSimp (ArithOp Common.AMul) l (Imm 0) = Just (Imm 0)
tryArithSimp (ArithOp Common.AMul) (Imm 1) r = Just r
tryArithSimp (ArithOp Common.AMul) l (Imm 1) = Just l
tryArithSimp (ArithOp Common.ADiv) l (Imm 1) = Just l
tryArithSimp (ArithOp Common.ADiv) (Imm 0) r = Just (Imm 0)
tryArithSimp (ArithOp Common.ADiv) (STmp lT) (STmp rT) =
  (if lT == rT then Just (Imm 1) else Nothing)
tryArithSimp (ArithOp Common.AMod) l (Imm 1) = Just (Imm 0)
tryArithSimp (ArithOp Common.AMod) (Imm 0) r = Just (Imm 0)
tryArithSimp (ArithOp Common.AMod) (STmp lT) (STmp rT) =
  (if lT == rT then Just (Imm 0) else Nothing)
tryArithSimp (ArithOp Common.AAnd) (Imm 0) r = Just (Imm 0)
tryArithSimp (ArithOp Common.AAnd) l (Imm 0) = Just (Imm 0)
tryArithSimp (ArithOp Common.AAnd) (Imm (-1)) r = Just r
tryArithSimp (ArithOp Common.AAnd) l (Imm (-1)) = Just l
tryArithSimp (ArithOp Common.AAnd) (STmp lT) (STmp rT) =
  (if lT == rT then Just (STmp lT) else Nothing)
tryArithSimp (ArithOp Common.AOr) (Imm 0) r = Just r
tryArithSimp (ArithOp Common.AOr) l (Imm 0) = Just l
tryArithSimp (ArithOp Common.AOr) l (Imm (-1)) = Just (Imm (-1))
tryArithSimp (ArithOp Common.AOr) (Imm (-1)) r = Just (Imm (-1))
tryArithSimp (ArithOp Common.AOr) (STmp lT) (STmp rT) =
  (if lT == rT then Just (STmp lT) else Nothing)
tryArithSimp (ArithOp Common.AXor) l (Imm 0) = Just l
tryArithSimp (ArithOp Common.AXor) (Imm 0) r = Just r
tryArithSimp (ArithOp Common.AXor) (STmp lT) (STmp rT) =
  (if lT == rT then Just (Imm 0) else Nothing)
tryArithSimp (ArithOp Common.AShL) l (Imm 0) = Just l
tryArithSimp (ArithOp Common.AShL) (Imm 0) r = Just (Imm 0)
tryArithSimp (ArithOp Common.AShR) l (Imm 0) = Just l
tryArithSimp (ArithOp Common.AShR) (Imm 0) r = Just (Imm 0)
tryArithSimp _ _ _ = Nothing


{-
  Tries to apply common subexpression elimination to the given binop.
-}
tryCSE :: PropState -> Binop -> Src -> Src -> Maybe Src
tryCSE (_, cseMap, _) op l r =
  let (l', r') = getCanonicalOperandOrder op l r
  in (case (Map.lookup (op, l', r') cseMap) of
        Nothing -> Nothing
        Just t -> Just $ STmp t)


{-
  In the case where multiple binops can have equivalent results (i.e. x + 1 vs 1 + x), chooses
  a canonical ordering for storage/lookup in the cseMap so they all will be treated as
  equivalent.
-}
getCanonicalOperandOrder :: Binop -> Src -> Src -> (Src, Src)
getCanonicalOperandOrder op l r
  | isCommutative op = sortSrcs l r
  | otherwise = (l, r)
  where isCommutative :: Binop -> Bool
        isCommutative (ArithOp Common.AAdd) = True
        isCommutative (ArithOp Common.ASub) = False
        isCommutative (ArithOp Common.AMul) = True
        isCommutative (ArithOp Common.ADiv) = False
        isCommutative (ArithOp Common.AMod) = False
        isCommutative (ArithOp Common.AAnd) = True
        isCommutative (ArithOp Common.AOr)  = True
        isCommutative (ArithOp Common.AXor) = True
        isCommutative (ArithOp Common.AShL) = False
        isCommutative (ArithOp Common.AShR) = False
        isCommutative (CmpOp (Common.CmpOp Common.L))  = False
        isCommutative (CmpOp (Common.CmpOp Common.LE)) = False
        isCommutative (CmpOp (Common.CmpOp Common.E))  = True
        isCommutative (CmpOp (Common.CmpOp Common.NE)) = True
        isCommutative (CmpOp (Common.CmpOp Common.GE)) = False
        isCommutative (CmpOp (Common.CmpOp Common.G))  = False

        sortSrcs :: Src -> Src -> (Src, Src)
        sortSrcs l r
          | l <= r = (l, r)
          | otherwise = (r, l)


{-
  Tries to apply redundant read elimination (like CSE, but for memory) to the given memory read.
-}
tryRRE :: PropState -> Mem -> Maybe Src
tryRRE (_, _, rreMap) mem =
  case (Map.lookup mem rreMap) of
    Nothing -> Nothing
    (Just t) -> Just $ STmp t


{-
  Updates a function argument, using the given value map.
-}
transArg :: PropState -> (Src, Size) -> (Src, Size)
transArg state (src, sz) = (transSrc state src, sz)

{-
  Updates a src, using the given value map.
-}
transSrc :: PropState -> Src -> Src
transSrc _ src@(Imm _) = src
transSrc (pMap, _, _) src@(STmp t) = Map.findWithDefault src t pMap
transSrc _ src@(FnPtr _) = src

{-
  Updates a memory reference, using the given value map.
-}
transMem :: PropState -> Mem -> Mem
transMem state (ArrMem esz addrSrc idxSrc o) =
  ArrMem esz (transSrc state addrSrc) (transSrc state idxSrc) o
transMem state (PtrMem addrSrc o) = PtrMem (transSrc state addrSrc) o

isTmp :: Src -> Bool
isTmp (STmp _) = True
isTmp _ = False

minInt :: Int32
minInt = minBound
