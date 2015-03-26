{-
  Transforms from 3-argument to 2-argument abstract assembly.
-}

module Compile.CodeGen.GenTwo where

import Compile.Types.Common (TmpIdent, BlockIdent, Size)
import qualified Compile.Types.Common as Common
import qualified Compile.Types.S3Asm as S3Asm
import qualified Compile.Types.B2Asm as B2Asm
import qualified Job

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List

type S3BlockMap = S3Asm.BlockMap
type B2BlockMap = B2Asm.BlockMap
type ArgTmpMap = Map.Map BlockIdent [TmpIdent]

{-
  Generates two-argument assembly from 3-argument SSA assembly by doing the following:
  --> de-parameterizing all functions and labels (every call/parameterized jmp is replaced by a
      series of writes to setup the arguments, followed by an unparameterized jmp)
  --> turn "return t" into "res0 = t; ret"
  --> add a block to the beginning of each function which copies args in from reserved temps
  --> expand cmpjmps into compares, followed by jcc
-}
ssaToTwo :: Job.Job -> S3Asm.S3Asm -> B2Asm.B2Asm
ssaToTwo _ (S3Asm.Prog fns) = B2Asm.Prog $ map transFn fns
  where transFn :: S3Asm.Fn -> B2Asm.Fn
        transFn (S3Asm.Fn name params bMap) =
          let bMap' = expandCJmps bMap
              aMap = getArgTemps bMap'
              bMap'' = Map.intersectionWith (transBlock bMap aMap) bMap' aMap
              bMap''' = addPreludeBlock bMap'' aMap params
          in B2Asm.Fn name bMap'''

        {-
          For each block, computes special temps unused anywhere else in the function to serve
          as argument temps for that block.
        -}
        getArgTemps :: S3BlockMap -> ArgTmpMap
        getArgTemps bMap =
          let (_, tMap) = Map.mapAccum allocateTempsForBlock (getStartingT bMap) bMap
          in tMap

        {-
          Returns the smallest temp number bigger than any temp number used in the program.
        -}
        getStartingT :: S3BlockMap -> TmpIdent
        getStartingT bMap = (Map.fold updateMaxWithBlock (-1) bMap) + 1

        updateMaxWithBlock :: S3Asm.Block -> TmpIdent -> TmpIdent
        updateMaxWithBlock (S3Asm.BBlock [] ss _) maxTmp = foldl updateMaxWithSIns maxTmp ss
        updateMaxWithBlock (S3Asm.BBlock params ss _) maxTmp =
          max (maximum $ map fst params) $ foldl updateMaxWithSIns maxTmp ss

        updateMaxWithSIns :: TmpIdent -> S3Asm.SIns -> TmpIdent
        updateMaxWithSIns maxTmp (S3Asm.Binop _ _ _ _ _ (S3Asm.DTmp tDst)) = max maxTmp tDst
        updateMaxWithSIns maxTmp (S3Asm.Set _ _ (S3Asm.DTmp tDst)) = max maxTmp tDst
        updateMaxWithSIns maxTmp (S3Asm.Load _ _ (S3Asm.DTmp tDst)) = max maxTmp tDst
        updateMaxWithSIns maxTmp (S3Asm.Store _ _ _) = maxTmp
        updateMaxWithSIns maxTmp (S3Asm.Memcpy _ _) = maxTmp
        updateMaxWithSIns maxTmp (S3Asm.Call _ _ _ (S3Asm.DTmp tDst)) = max maxTmp tDst
        updateMaxWithSIns maxTmp (S3Asm.CallPtr _ _ _ (S3Asm.DTmp tDst)) = max maxTmp tDst
        updateMaxWithSIns maxTmp (S3Asm.CheckNonZero _ _ _) = maxTmp
        updateMaxWithSIns maxTmp (S3Asm.CheckNonNeg _ _ _) = maxTmp
        updateMaxWithSIns maxTmp (S3Asm.CheckEqual _ _ _ _) = maxTmp
        updateMaxWithSIns maxTmp (S3Asm.CheckDivOverflow _ _) = maxTmp
        updateMaxWithSIns maxTmp (S3Asm.CheckShiftMax _) = maxTmp
        updateMaxWithSIns maxTmp (S3Asm.CheckArrEnd _ _) = maxTmp


        allocateTempsForBlock :: TmpIdent -> S3Asm.Block -> (TmpIdent, [TmpIdent])
        allocateTempsForBlock t (S3Asm.BBlock params _ _) =
          mapAccumL (\t -> (\(p, _) -> (t + 1, t))) t params

        {-
          Translates the given block into B2Asm, by doing the following:
            1. Adding block-arg setup instructions at the start of the block.
            2. Translating all the normal straight-line instructions into 2asm versions.
            3. Adding fn-arg processing around call instructions.
            4. Adding block-arg setup instructions before all the jumps.
        -}
        transBlock :: S3BlockMap -> ArgTmpMap -> S3Asm.Block -> [TmpIdent] -> B2Asm.Block
        transBlock bMap aMap (S3Asm.BBlock params ss j) argTmps =
          let ss' = getCopySs argTmps params ++ concatMap transSIns ss
              (js, j') = transJIns aMap j
          in B2Asm.BBlock (ss' ++ js) j'

        getCopySs :: [TmpIdent] -> [(TmpIdent, Size)] -> [B2Asm.SIns]
        getCopySs = zipWith (\s -> (\(d, sz) -> B2Asm.Set sz (B2Asm.STmp s) (B2Asm.DTmp d)))

        -- NOTE: the Common.ArithOp case relies on the fact that the input is in SSA
        transSIns :: S3Asm.SIns -> [B2Asm.SIns]
        transSIns (S3Asm.Binop srcSz dstSz op l r dst) =
          let (l', r', dst') = (transSrc l, transSrc r, transDst dst)
          in case (transOp op) of
              (Left o) -> [B2Asm.Set srcSz l' dst',
                           B2Asm.Asop srcSz o r' dst' ]
              (Right c) ->
                let true = B2Asm.Imm 1
                    false = B2Asm.Imm 0
                    (cmpIns, cmpCode) = transCmpCode srcSz c l' r'
                in  [B2Asm.Set dstSz false dst', cmpIns, B2Asm.CSet dstSz cmpCode true dst']
        transSIns (S3Asm.Set sz src dst) =
          [B2Asm.Set sz (transSrc src) (transDst dst)]
        transSIns (S3Asm.Load sz mem dst) =
          [B2Asm.Load sz (transMem mem) (transDst dst)]
        transSIns (S3Asm.Store sz src mem) =
          [B2Asm.Store sz (transSrc src) (transMem mem)]
        transSIns (S3Asm.Memcpy bytes src) =
          [B2Asm.Memcpy bytes (transSrc src)]
        transSIns (S3Asm.Call sz name args dst) =
          transArgs args ++ [B2Asm.Call name, B2Asm.FromReserved sz B2Asm.res0 (transDst dst)]
        transSIns (S3Asm.CallPtr sz fnPtrSrc args dst) =
          transArgs args ++ [B2Asm.CallPtr (transSrc fnPtrSrc),
                             B2Asm.FromReserved sz B2Asm.res0 (transDst dst)]
        transSIns (S3Asm.CheckNonZero sz src err) =
          [B2Asm.CheckNonZero sz (transSrc src) err]
        transSIns (S3Asm.CheckNonNeg sz src err) =
          [B2Asm.CheckNonNeg sz (transSrc src) err]
        transSIns (S3Asm.CheckEqual sz src1 src2 err) =
          [B2Asm.CheckEqual sz (transSrc src1) (transSrc src2) err]
        transSIns (S3Asm.CheckDivOverflow l r) =
          [B2Asm.CheckDivOverflow (transSrc l) (transSrc r)]
        transSIns (S3Asm.CheckShiftMax src) =
          [B2Asm.CheckShiftMax (transSrc src)]
        transSIns (S3Asm.CheckArrEnd addrSrc indSrc) =
          [B2Asm.CheckArrEnd (transSrc addrSrc) (transSrc indSrc)]

        transJIns :: ArgTmpMap -> S3Asm.JIns -> ([B2Asm.SIns], B2Asm.JIns)
        transJIns _ (S3Asm.Ret sz src) =
          ([B2Asm.ToReserved sz (transSrc src) B2Asm.res0], B2Asm.Ret)
        transJIns aMap (S3Asm.Jmp l) =
          let (ls, l') = transLabel aMap l
          in (ls, B2Asm.Jmp l')
        transJIns aMap (S3Asm.CmpJmp sz cmp l r (S3Asm.Label b1 []) (S3Asm.Label b2 [])) =
          let (cmpIns, cmp') = transCmpCode sz cmp (transSrc l) (transSrc r)
          in ([cmpIns], B2Asm.Jmpcc cmp' b1 b2)
        transJIns aMap (S3Asm.CmpJmp _ _ _ _ _ _) =
          error "in unssa: found CmpJmp with parameterized labels (should have no arguments)"
        transJIns _ (S3Asm.Raise err) = ([], B2Asm.Raise err)

        transLabel :: ArgTmpMap -> S3Asm.Label -> ([B2Asm.SIns], BlockIdent)
        transLabel aMap (S3Asm.Label b args) =
          let aTmps = aMap Map.! b
          in (zipWith copyArg args aTmps, b)
          where copyArg :: (S3Asm.Src, Size) -> TmpIdent -> B2Asm.SIns
                copyArg (src, sz) t = B2Asm.Set sz (transSrc src) $ B2Asm.DTmp t

        transArgs :: [(S3Asm.Src, Size)] -> [B2Asm.SIns]
        transArgs = snd . mapAccumL transArg 1
          where transArg :: Integer -> (S3Asm.Src, Size) -> (Integer, B2Asm.SIns)
                transArg n (src, sz) =
                  (n + 1, B2Asm.ToReserved sz (transSrc src) (B2Asm.argI n))

        transCmpCode :: Size -> Common.Cmp -> B2Asm.Src -> B2Asm.Src
                     -> (B2Asm.SIns, Common.Cmp)
        transCmpCode sz c l r =
          let msg1 = "Encountered two immediates in comparison. " ++
                     "Should not happen after constant folding"
              msg2 = "Encountered two function pointers in comparison. " ++
                     "Should not happen after constant folding"
              msg3 = "Encountered a function pointer and NULL in comparison. " ++
                     "Should not happen after constant folding"
              cmpIns = (if Common.isEqCmp c then B2Asm.Test else B2Asm.Cmp)
          in  case (l, r) of
            (B2Asm.Imm _  , B2Asm.Imm _  ) -> error msg1
            (_            , B2Asm.STmp rT) -> (cmpIns sz l (B2Asm.DTmp rT), Common.flipCmp c)
            (B2Asm.STmp lT, _            ) -> (cmpIns sz r (B2Asm.DTmp lT), c)
            (B2Asm.FnPtr name1, B2Asm.FnPtr name2) -> error msg2
            (B2Asm.FnPtr _, B2Asm.Imm _) -> error msg3
            (B2Asm.Imm _, B2Asm.FnPtr _) -> error msg3

        transSrc :: S3Asm.Src -> B2Asm.Src
        transSrc (S3Asm.STmp t) = B2Asm.STmp t
        transSrc (S3Asm.Imm x) = B2Asm.Imm x
        transSrc (S3Asm.FnPtr fnName) = B2Asm.FnPtr fnName

        transDst :: S3Asm.Dst -> B2Asm.Dst
        transDst (S3Asm.DTmp t) = B2Asm.DTmp t

        transOp :: S3Asm.Binop -> Either Common.ArithOp Common.Cmp
        transOp (S3Asm.ArithOp arithop) = Left arithop
        transOp (S3Asm.CmpOp (Common.CmpOp cmp)) = Right cmp

        transMem :: S3Asm.Mem -> B2Asm.Mem
        transMem (S3Asm.ArrMem elemSz addrSrc indSrc o) =
          B2Asm.ArrMem elemSz (transSrc addrSrc) (transSrc indSrc) o
        transMem (S3Asm.PtrMem addrSrc o) = B2Asm.PtrMem (transSrc addrSrc) o

        {-
          Adds a block 0 (incrementing all other block labels), whose only purpose is to load
          function parameters in from arg1, ..., argN and jump to block 1. Note that because
          each block has a set of arg storage temps associated with it (like reserved temps for
          function arguments), the args are loaded directly into those temps.
        -}
        addPreludeBlock :: B2BlockMap -> ArgTmpMap -> [(TmpIdent, Size)] -> B2BlockMap
        addPreludeBlock bMap aMap fnParams =
          let (oldFirst, _) = Map.findMin bMap
              bMap' = Map.mapKeysMonotonic (+1) $ Map.map incLabels bMap
              newFirst = oldFirst + 1
              argTmpsWithSizes = zip (aMap Map.! oldFirst) (map snd fnParams)
              ss = zipWith makeLoadIns [1..] argTmpsWithSizes
          in Map.insert oldFirst (B2Asm.BBlock ss $ B2Asm.Jmp newFirst) bMap'

        incLabels :: B2Asm.Block -> B2Asm.Block
        incLabels block@(B2Asm.BBlock ss (B2Asm.Ret)) = block
        incLabels (B2Asm.BBlock ss (B2Asm.Jmp l)) = B2Asm.BBlock ss $ B2Asm.Jmp (l + 1)
        incLabels (B2Asm.BBlock ss (B2Asm.Jmpcc cmp l1 l2)) =
          B2Asm.BBlock ss (B2Asm.Jmpcc cmp (l1 + 1) (l2 + 1))
        incLabels block@(B2Asm.BBlock ss (B2Asm.Raise _)) = block

        makeLoadIns :: TmpIdent -> (TmpIdent, Size) -> B2Asm.SIns
        makeLoadIns r (d, sz) = B2Asm.FromReserved sz (B2Asm.RTmp r) (B2Asm.DTmp d)

{-
  Expands conditional jumps that take parameters into their own blocks, do avoid having to set
  up arguments for both branches in both cases. Rules are:
    cjmp A([]) B([]) = cjmp A([]) B([])
    cjmp A(ps) B([]) = cjmp A'([]) B([]); A': jmp A'(ps)
    cjmp A([]) B(ps) = cjmp A([]) B'(ps); B': jmp B'(ps)
    cjmp A(ps) B(ps') = cjmp A'([]) B'([]); A': jmp A(ps); B': jmp B(ps')

  Algorithm:
  --> Map (b, block) -> [(b, block)], where new block numbers don't have to be in order.
  --> Zip with [0..] to generate (oldName -> newName) map.
  --> Update all the blocks to account for this.
-}
expandCJmps :: S3BlockMap -> S3BlockMap
expandCJmps bMap =
  if Map.null bMap
  then bMap
  else (let (maxB, _) = Map.findMax bMap
            (_, bLists) = mapAccumL expandBlock (maxB + 1) $ Map.assocs bMap
            pairs = concat bLists
            renameMap = Map.fromList [(b, b') | ((b, _), b') <- zip pairs [0..]]
        in Map.fromList $ map (updatePair renameMap) pairs)
  where expandBlock :: BlockIdent -> (BlockIdent, S3Asm.Block)
                    -> (BlockIdent, [(BlockIdent, S3Asm.Block)])
        expandBlock nextB (b, S3Asm.BBlock ps ss (S3Asm.CmpJmp sz c s1 s2 l1 l2)) =
          case (l1, l2) of
            (S3Asm.Label b1 [], S3Asm.Label b2 []) ->
              (nextB, [(b, S3Asm.BBlock ps ss (S3Asm.CmpJmp sz c s1 s2 l1 l2))])
            (S3Asm.Label b1 _, S3Asm.Label b2 []) ->
              let (b1', block1') = (nextB, S3Asm.BBlock [] [] $ S3Asm.Jmp l1)
                  l1' = S3Asm.Label b1' []
                  block' = S3Asm.BBlock ps ss (S3Asm.CmpJmp sz c s1 s2 l1' l2)
              in (nextB + 1, [(b, block'), (b1', block1')])
            (S3Asm.Label b1 [], S3Asm.Label b2 _) ->
              let (b2', block2') = (nextB, S3Asm.BBlock [] [] $ S3Asm.Jmp l2)
                  l2' = S3Asm.Label b2' []
                  block' = S3Asm.BBlock ps ss (S3Asm.CmpJmp sz c s1 s2 l1 l2')
              in (nextB + 1, [(b, block'), (b2', block2')])
            (S3Asm.Label b1 _, S3Asm.Label b2 _) ->
              let (b1', block1') = (nextB, S3Asm.BBlock [] [] $ S3Asm.Jmp l1)
                  (b2', block2') = (nextB + 1, S3Asm.BBlock [] [] $ S3Asm.Jmp l2)
                  (l1', l2') = (S3Asm.Label b1' [], S3Asm.Label b2' [])
                  block' = S3Asm.BBlock ps ss (S3Asm.CmpJmp sz c s1 s2 l1' l2')
              in (nextB + 2, [(b, block'), (b1', block1'), (b2', block2')])
        expandBlock nextB (b, block) = (nextB, [(b, block)])


        updatePair :: Map.Map BlockIdent BlockIdent -> (BlockIdent, S3Asm.Block)
                   -> (BlockIdent, S3Asm.Block)
        updatePair rMap (b, S3Asm.BBlock ps ss j) =
          let j' = (case j of
                      (S3Asm.Ret sz s) -> S3Asm.Ret sz s
                      (S3Asm.Jmp l) -> S3Asm.Jmp $ transLabel rMap l
                      (S3Asm.CmpJmp sz cmp s1 s2 l1 l2) ->
                        S3Asm.CmpJmp sz cmp s1 s2 (transLabel rMap l1) (transLabel rMap l2)
                      j@(S3Asm.Raise _) -> j)
          in (rMap Map.! b, S3Asm.BBlock ps ss j')


        transLabel :: Map.Map BlockIdent BlockIdent -> S3Asm.Label -> S3Asm.Label
        transLabel rMap (S3Asm.Label b args) = S3Asm.Label (rMap Map.! b) args
