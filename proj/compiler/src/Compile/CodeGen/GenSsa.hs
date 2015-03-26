{-
  Converts blocked 3-argument assembly into SSA form.
-}

module Compile.CodeGen.GenSsa where

import Data.List (mapAccumL)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Compile.Types.Common (TmpIdent, BlockIdent, Size(Quad, Long))
import qualified Compile.Types.B3Asm as B3Asm
import qualified Compile.Types.S3Asm as S3Asm
import qualified Compile.Graph.Graph as Graph
import qualified Compile.Graph.GraphAlgs as GraphAlgs
import qualified Job

type TransState = (TmpIdent, TransMap)
type TransMap = Map.Map TmpIdent TmpIdent

type ParamMap = Map.Map BlockIdent [(TmpIdent, Size)]

type DefSet = Set.Set TmpIdent
type LiveMap = Map.Map TmpIdent Size 
type BlockInfo = (DefSet, LiveMap, LiveMap)
type BlockGraph = Graph.Graph BlockIdent BlockInfo

{-
  Converts 3-argument assembly to SSA form, by building a map from temp names (i.e. numbers) in
  the input to their current names and folding over the list of assembly to do the rename.
  ASSUMES: no uninitialized variables at this point.

  NOTE: the first block in each function is being treated a bit strangely, since it's
        parameterized with the function call arguments, If you want to optimize things, beware
        of this case! Function parameters are less flexible than normal temps (I think).
        Current algorithm is:
        --> Get the list of parameters each block expects.
        --> Force the first block's paramter list to be exactly the function's parameters
        --> Translate every block using the straight-line algorithm
        --> Assert that the first block's translated parameters are the function parameters
 -}
threeToSsa :: Job.Job -> B3Asm.B3Asm -> S3Asm.S3Asm
threeToSsa _ (B3Asm.Prog fns) = S3Asm.Prog $ map transFn fns
  where transFn :: B3Asm.Fn -> S3Asm.Fn
        transFn (B3Asm.Fn name fnParams bMap) =
          let paramMap = getBlockParams bMap
              paramMap' = Map.updateMin (\_ -> Just fnParams) paramMap
              pbMap = snd $ Map.mapAccumWithKey (transBlock paramMap') (0, Map.empty) bMap
              (0, S3Asm.BBlock blockParams _ _) = Map.findMin pbMap
          in  (if fnParams == blockParams
               then S3Asm.Fn name fnParams pbMap
               else error ("GenSSA: for " ++ show name ++ ", expected " ++ show fnParams ++
                           " but got " ++ show blockParams))

{-
  Gets the list of paramters that need to be passed into each block, by saying that a parameter
  needs to be passed into a block if it's live at the start of the block. This is a valid but
  non-minimal, approximation.

  The algorithm used to determine the set of variables live at the start of each block is:
  --> Find the variables that are used before being defined in each block
  --> Find the variables that are defined in each block
  --> Saturate the rule `a variable is live at the start of a block if it's used before being
      defined in that block, or is live at the start of a successor block and isn't defined in
      the current block.`
  This is done by lifting the block map into a directed graph, and propogating using a DFS-like
  scheme (per-variable, not per-block).
-}
getBlockParams :: B3Asm.BlockMap -> ParamMap
getBlockParams bMap =
  let bg = propogateLiveness $ Graph.fromMapWith livenessFromBlock getSuccessors bMap
  in Graph.toMapWith extractLiveness bg
  where getSuccessors :: B3Asm.Block -> [BlockIdent]
        getSuccessors (B3Asm.BBlock ss (B3Asm.Ret _ _)) = []
        getSuccessors (B3Asm.BBlock ss (B3Asm.Jmp l)) = [l]
        getSuccessors (B3Asm.BBlock ss (B3Asm.CmpJmp _ _ _ _ l1 l2)) = [l1, l2]
        getSuccessors (B3Asm.BBlock ss (B3Asm.Raise _)) = []
        extractLiveness :: BlockInfo -> [(TmpIdent, Size)]
        extractLiveness (_, lMap, pMap) = Map.assocs $ Map.union lMap pMap

{-
  Given a block of straight-line assembly terminated by a jump/ret, computes the following:
  1. A set of all the variables defined in the current block.
  2. A set of all the variables used before their definition in the current block (i.e. all the
     variables known to be live at the start of the block).
-}
livenessFromBlock :: B3Asm.Block -> BlockInfo
livenessFromBlock (B3Asm.BBlock ss j) =
  let (dSet, lMap) = foldr processSIns (Set.empty, getLiveFromJ j) ss
  in (dSet, lMap, Map.empty)
  where getLiveFromJ :: B3Asm.JIns -> LiveMap
        getLiveFromJ (B3Asm.Ret sz (B3Asm.STmp t)) = Map.singleton t sz
        getLiveFromJ (B3Asm.Jmp _) = Map.empty
        getLiveFromJ (B3Asm.CmpJmp sz _ (B3Asm.STmp t) (B3Asm.STmp s) _ _) =
          Map.fromList [(t, sz), (s, sz)]
        getLiveFromJ (B3Asm.CmpJmp sz _ (B3Asm.STmp t) _ _ _) = Map.singleton t sz
        getLiveFromJ (B3Asm.CmpJmp sz _ _ (B3Asm.STmp t) _ _) = Map.singleton t sz
        getLiveFromJ (B3Asm.Raise _) = Map.empty

        {-
          The first tuple represents the state just below the given instruction, the return
          triple represents the state just above.
        -}
        processSIns :: B3Asm.SIns -> (DefSet, LiveMap) -> (DefSet, LiveMap)
        processSIns (B3Asm.Binop srcSz dstSz _ l r (B3Asm.DTmp tDst)) (dSet, lMap) =
          (Set.insert tDst dSet, addIfTmp srcSz r $ addIfTmp srcSz l $ Map.delete tDst lMap)
        processSIns (B3Asm.Set sz src (B3Asm.DTmp tDst)) (dSet, lMap) =
          (Set.insert tDst dSet, addIfTmp sz src $ Map.delete tDst lMap)
        processSIns (B3Asm.Load sz mem (B3Asm.DTmp tDst)) (dSet, lMap) =
          (Set.insert tDst dSet, addLivenessFromMem mem $ Map.delete tDst lMap)
        processSIns (B3Asm.Store sz src mem) (dSet, lMap) =
          (dSet, addLivenessFromMem mem $ addIfTmp sz src lMap)
        processSIns (B3Asm.Memcpy bytes src) (dSet, lMap) =
          (dSet, addIfTmp Quad src lMap)
        processSIns (B3Asm.Call sz _ args (B3Asm.DTmp tDst)) (dSet, lMap) =
          (Set.insert tDst dSet, foldr (uncurry $ flip addIfTmp) (Map.delete tDst lMap) args)
        processSIns (B3Asm.CallPtr sz fPtrSrc args (B3Asm.DTmp tDst)) (dSet, lMap) =
          let lMap' = foldr (uncurry $ flip addIfTmp) (Map.delete tDst lMap) args
          in (Set.insert tDst dSet, addIfTmp Long fPtrSrc lMap')
        processSIns (B3Asm.CheckNonZero sz src _) (dSet, lMap) =
          (dSet, addIfTmp sz src lMap)
        processSIns (B3Asm.CheckNonNeg sz src _) (dSet, lMap) =
          (dSet, addIfTmp sz src lMap)
        processSIns (B3Asm.CheckEqual sz src1 src2 _) (dSet, lMap) =
          (dSet, addIfTmp sz src1 $ addIfTmp sz src2 lMap)
        processSIns (B3Asm.CheckDivOverflow l r) (dSet, lMap) =
          (dSet, addIfTmp Long l $ addIfTmp Long r lMap)
        processSIns (B3Asm.CheckShiftMax src) (dSet, lMap) =
          (dSet, addIfTmp Long src lMap)
        processSIns (B3Asm.CheckArrEnd addrSrc indSrc) (dSet, lMap) =
          (dSet, addIfTmp Quad addrSrc $ addIfTmp Long indSrc lMap)

        {-
          Given a src and a set, if the src is a temp it adds it to the set, otherwise it does
          nothing.
        -}
        addIfTmp :: Size -> B3Asm.Src -> LiveMap -> LiveMap
        addIfTmp sz (B3Asm.STmp t) = Map.insert t sz
        addIfTmp _ _ = id

        {-
          Given a mem and a set, adds all temps rendered live by the memory access to the set.
        -}
        addLivenessFromMem :: B3Asm.Mem -> LiveMap -> LiveMap
        addLivenessFromMem (B3Asm.ArrMem esz addrSrc idxSrc o) lMap =
          addIfTmp Quad addrSrc $ addIfTmp Long idxSrc lMap
        addLivenessFromMem (B3Asm.PtrMem addrSrc o) lMap = addIfTmp Quad addrSrc lMap


{-
  Given the set of temps known to be live at the start of each block, and the set of temps
  defined within that block, propogates liveness as much as possible to find the full set of
  temps live at the start of each block. For later convenience separates out those newly
  propogated temps from the ones that were live at the top of the block before propogation.
-}
propogateLiveness :: BlockGraph -> BlockGraph
propogateLiveness bg =
  let pairSetMap = GraphAlgs.propogateAllBackwards getLiveVars canPropVar bg
      pairMapMap = Map.map (Map.fromList . Set.toList) pairSetMap
  in Graph.addMapData updatePropMap pairMapMap bg
  where getLiveVars :: BlockInfo -> [(TmpIdent, Size)]
        getLiveVars (_, lMap, _) = Map.assocs lMap
        canPropVar :: (TmpIdent, Size) -> BlockInfo -> Bool
        canPropVar (t, sz) (dSet, lMap, _) = t `Set.notMember` dSet && t `Map.notMember` lMap
        updatePropMap :: LiveMap -> BlockInfo -> BlockInfo
        updatePropMap pMap (dSet, lMap, _) = (dSet, lMap, pMap)


{-
  Takes in a map from original temp names to their current versions (represented as a whole new
  temp, not as (temp, v#)), a set of parameters for each block to take in, and a current block,
  and uses this information to translate the block into SSA form.
-}
transBlock :: ParamMap -> TransState -> BlockIdent -> B3Asm.Block -> (TransState, S3Asm.Block)
transBlock paramMap state b (B3Asm.BBlock ss j) =
  let (state', args) = transParameters state (paramMap Map.! b)
      (state'', ss') = mapAccumL transSIns state' ss
      (state''', j') = transJIns state'' paramMap j
  in (state''', S3Asm.BBlock args ss' j')
  where -- Translates the given list of input parameters, updating the state if necessary
        transParameters :: TransState -> [(TmpIdent, Size)] -> (TransState, [(TmpIdent, Size)])
        transParameters state ts = mapAccumL transParameter state ts
        transParameter :: TransState -> (TmpIdent, Size) -> (TransState, (TmpIdent, Size))
        transParameter (nextT, tMap) (t, sz) =
          ((nextT + 1, Map.insert t nextT tMap), (nextT, sz))
        {-
          Translates a straight-line instruction and returns the instruction and updated state.
        -}
        transSIns :: TransState -> B3Asm.SIns -> (TransState, S3Asm.SIns)
        transSIns state (B3Asm.Binop srcSz dstSz op l r dst) =
          let (l', r') = (transSrc state l, transSrc state r)
              (dst', state') = transDst state dst
              op' = transOp op
          in (state', S3Asm.Binop srcSz dstSz op' l' r' dst')
        transSIns state (B3Asm.Set sz src dst) =
          let src' = transSrc state src
              (dst', state') = transDst state dst
          in (state', S3Asm.Set sz src' dst')
        transSIns state (B3Asm.Load sz mem dst) =
          let mem' = transMem state mem
              (dst', state') = transDst state dst
          in (state', S3Asm.Load sz mem' dst')
        transSIns state (B3Asm.Store sz src mem) =
          let src' = transSrc state src
              mem' = transMem state mem
          in (state, S3Asm.Store sz src' mem')
        transSIns state (B3Asm.Memcpy bytes src) =
          (state, S3Asm.Memcpy bytes (transSrc state src))
        transSIns state (B3Asm.Call sz name args dst) =
          let args' = transArguments state args
              (dst', state') = transDst state dst
          in (state', S3Asm.Call sz name args' dst')
        transSIns state (B3Asm.CallPtr sz fPtrSrc args dst) =
          let args' = transArguments state args
              (dst', state') = transDst state dst
          in (state', S3Asm.CallPtr sz (transSrc state fPtrSrc) args' dst')
        transSIns state (B3Asm.CheckNonZero sz src err) =
          (state, S3Asm.CheckNonZero sz (transSrc state src) err)
        transSIns state (B3Asm.CheckNonNeg sz src err) =
          (state, S3Asm.CheckNonNeg sz (transSrc state src) err)
        transSIns state (B3Asm.CheckEqual sz src1 src2 err) =
          (state, S3Asm.CheckEqual sz (transSrc state src1) (transSrc state src2) err)
        transSIns state (B3Asm.CheckDivOverflow l r) =
          (state, S3Asm.CheckDivOverflow (transSrc state l) (transSrc state r))
        transSIns state (B3Asm.CheckShiftMax src) =
          (state, S3Asm.CheckShiftMax (transSrc state src))
        transSIns state (B3Asm.CheckArrEnd addrSrc indSrc) =
          (state, S3Asm.CheckArrEnd (transSrc state addrSrc) (transSrc state indSrc))
        {-
          Translates a jump instruction and returns the instruction and modified state.
        -}
        transJIns :: TransState -> ParamMap -> B3Asm.JIns -> (TransState, S3Asm.JIns)
        transJIns state _ (B3Asm.Ret sz src) = (state, S3Asm.Ret sz (transSrc state src))
        transJIns state paramMap (B3Asm.Jmp b) =
          (state, S3Asm.Jmp $ transLabel paramMap state b)
        transJIns state paramMap (B3Asm.CmpJmp sz cmp l r b1 b2) =
          let (b1', b2') = (transLabel paramMap state b1, transLabel paramMap state b2)
          in (state, S3Asm.CmpJmp sz cmp (transSrc state l) (transSrc state r) b1' b2')
        transJIns state paramMap (B3Asm.Raise err) = (state, S3Asm.Raise err)
        {-
          Translates a label (in B3Asm just a BlockIdent) into a label with param list.
        -}
        transLabel :: ParamMap -> TransState -> BlockIdent -> S3Asm.Label
        transLabel paramMap (_, tMap) b =
          S3Asm.Label b [(S3Asm.STmp $ tMap Map.! p, sz) | (p, sz) <- paramMap Map.! b]
        {-
          Translates a list of arguments (tmps) to their current versions.
        -}
        transArguments :: TransState -> [(B3Asm.Src, Size)] -> [(S3Asm.Src, Size)]
        transArguments state ts = map (\(src, sz) -> (transSrc state src, sz)) ts
        {-
          Translates a src to S3Asm, updating the possible tmp name using the rename map.
        -}
        transSrc :: TransState -> B3Asm.Src -> S3Asm.Src
        transSrc _ (B3Asm.Imm x) = S3Asm.Imm x
        transSrc (_, tMap) (B3Asm.STmp t) = S3Asm.STmp $ tMap Map.! t
        transSrc _ (B3Asm.FnPtr fnName) = S3Asm.FnPtr fnName
        {-
          Translates a memory access (either read or write; it doesn't matter) to S3Asm.
        -}
        transMem :: TransState -> B3Asm.Mem -> S3Asm.Mem
        transMem state (B3Asm.ArrMem elemSz addrSrc indSrc o) =
          S3Asm.ArrMem elemSz (transSrc state addrSrc) (transSrc state indSrc) o
        transMem state (B3Asm.PtrMem addrSrc o) =
          S3Asm.PtrMem (transSrc state addrSrc) o
        {-
          Translates a dest to S3Asm, updating the tmp map as needed.
        -}
        transDst :: TransState -> B3Asm.Dst -> (S3Asm.Dst, TransState)
        transDst (nextT, tMap) (B3Asm.DTmp t) =
          (S3Asm.DTmp nextT, (nextT + 1, Map.insert t nextT tMap))
        {-
          Translates a binary operator to S3Asm.
        -}
        transOp :: B3Asm.Binop -> S3Asm.Binop
        transOp (B3Asm.ArithOp arithop) = S3Asm.ArithOp arithop
        transOp (B3Asm.CmpOp cmpop) = S3Asm.CmpOp cmpop
