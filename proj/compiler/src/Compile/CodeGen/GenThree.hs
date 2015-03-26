{-
  Converts an IRT into our 3-argument abstract assembly (see Types/B3Asm.hs)
-}

module Compile.CodeGen.GenThree where

import Compile.Types.Common (TmpIdent, BlockIdent, FnName, Size(..))
import qualified Compile.Types.Common as Common
import qualified Compile.Types.IRT as IRT
import qualified Compile.Types.B3Asm as B3Asm
import qualified Job

import Data.List (mapAccumL)
import qualified Data.Map as Map
import Data.Maybe
import Data.Either

type BlockMap = B3Asm.BlockMap

type SizeMap = Map.Map TmpIdent Size

type TransState = (BlockMap, BlockIdent, TmpIdent, SizeMap)

{-
  Converts an AST to B3Asm format (a collection of basic blocks).
-}
irtToThree :: Job.Job -> IRT.IRT -> B3Asm.B3Asm
irtToThree _ (IRT.Prog fns) = B3Asm.Prog $ map transFn fns
  where transFn :: IRT.Fn -> B3Asm.Fn
        transFn (IRT.Fn name params stmts) =
          let nextT = (case params of
                          [] -> 0
                          _ -> 1 + (maximum $ map fst params))
              sMap = Map.fromList params
              ((bMap, _, _, _), Nothing) = transList (Map.empty, 0, nextT, sMap) stmts
          in (B3Asm.Fn name params bMap)


{-
  Translates a list of statements (guaranteed no Decls) into a list of basic blocks:
  --> If the list is empty, it returns the state with nextB incremented, and Maybe([])
  --> If the list is nonempty, it parses as many complete blocks as it can:
      --> If there are no dangling blocks, it returns the modified state and Nothing
      --> If there is a dangling block (ss), it returns the rest of the state and Just(ss)

  NOTE: when transList returns a dangling block, it must increment nextB to reserve a block
        number for it.

  NOTE: we can safely assume that there's no dead code lying around after a return, because
        astToIRT would've removed it.
-}
transList :: TransState -> [IRT.Stmt] -> (TransState, Maybe [B3Asm.SIns])
transList (bMap, nextB, nextT, sMap) [] = ((bMap, nextB + 1, nextT, sMap), Just [])
transList state stmts =
  let (state', dangleOpt, restOpt) = transBlock state stmts
  in (case (dangleOpt, restOpt) of
        (Just ss, Just rest) -> error "found dangling statement not at end of list"
        (Nothing, Just rest) -> transList state' rest
        (dOpt, Nothing) -> (state', dOpt))


{-
  Parses a bblock from the front of the statement list, translates it, and returns the new
  state along with the remainder of the list. Extra complications come from the fact that the
  last block might not end (it could fall naturally into the parent scope), and that if
  statements sometimes need to fall through to a common following block, even if there's no
  more code in this scope:
  --> If the input is empty, it returns (state, Maybe [], Nothing)
  --> If the input is just a dangling block (ss), it returns (state, Maybe ss, Nothing)
  --> If the input a full block, then parse it into B and insert it into the map:
      --> If control flow stops at B, return (state', Nothing, Nothing)
      --> Otherwise, return (state', Nothing, Maybe rest)
-}
transBlock :: TransState -> [IRT.Stmt] -> (TransState, Maybe [B3Asm.SIns], Maybe [IRT.Stmt])
transBlock (bMap, nextB, nextT, sMap) stmts =
  let (pss, jStmtOpt, rest) = extractBlock stmts
      ((nextT', sMap'), pss') = transStraightLineStmts (nextT, sMap) pss
  in (case jStmtOpt of
        Nothing -> ((bMap, nextB + 1, nextT', sMap'), Just pss', Nothing)
        (Just (IRT.Return e)) ->
          let (ess, sz) = transExp e nextT' (nextT' + 1, sMap')
              pss'' = pss' ++ ess
              bMap' = addBlock nextB pss'' (B3Asm.Ret sz (B3Asm.STmp nextT')) bMap
          in ((bMap', nextB + 1, nextT', sMap'), Nothing, Nothing)
        (Just (IRT.Raise err)) ->
          let bMap' = addBlock nextB pss' (B3Asm.Raise err) bMap
          in ((bMap', nextB + 1, nextT', sMap'), Nothing, Nothing)
        (Just (IRT.While css (IRT.Term (IRT.NumLit _ 1)) bss)) ->
          let (pEnd, cStart) = (nextB, nextB + 1)
              ((bMap', nextB', nextT'', sMap''), Just css') =
                transList (bMap, cStart, nextT', sMap') css
              (cEnd, bStart) = (nextB' - 1, nextB')
              ((bMap'', nextB'', nextT''', _), bOpt) =
                transList (bMap', bStart, nextT'', sMap'') bss
              bEnd = nextB'' - 1
              bMap''' = addBlock pEnd pss' (B3Asm.Jmp cStart) bMap''
              bMap'''' = addBlock cEnd css' (B3Asm.Jmp bStart) bMap'''
              bMap''''' = optAdd bEnd bOpt (B3Asm.Jmp cStart) bMap''''
            in ((bMap''''', nextB'', nextT''', sMap''), Nothing, Nothing)
        (Just (IRT.While css ce bss)) ->
          let (pEnd, cStart) = (nextB, nextB + 1)
              ((bMap', nextB', nextT'', sMap''), Just css') =
                transList (bMap, cStart, nextT', sMap') css
              (cEnd, bStart) = (nextB' - 1, nextB')
              ((bMap'', nextB'', nextT''', _), bOpt) =
                transList (bMap', bStart, nextT'', sMap'') bss
              (bEnd, end) = (nextB'' - 1, nextB'')
              (css'', j) = transCond (nextT'', sMap'') ce bStart end
              css''' = css' ++ css''
              bMap''' = addBlock pEnd pss' (B3Asm.Jmp cStart) bMap''
              bMap'''' = addBlock cEnd css''' j bMap'''
              bMap''''' = optAdd bEnd bOpt (B3Asm.Jmp cStart) bMap''''
            in ((bMap''''', end, nextT''', sMap''), Nothing, Just rest)
        (Just (IRT.If ce tss fss)) ->
          let (pEnd, tStart) = (nextB, nextB + 1)
              ((bMap', nextB', nextT'', _), tOpt) =
                transList (bMap, tStart, nextT', sMap') tss
              (tEnd, fStart) = (nextB' - 1, nextB')
              ((bMap'', nextB'', nextT''', _), fOpt) =
                transList (bMap', fStart, nextT'', sMap') fss
              (fEnd, end) = (nextB'' - 1, nextB'')
              (css, j) = transCond (nextT', sMap') ce tStart fStart
              pss'' = pss' ++ css
              bMap''' = addBlock pEnd pss'' j bMap''
              bMap'''' = optAdd tEnd tOpt (B3Asm.Jmp end) bMap'''
              bMap''''' = optAdd fEnd fOpt (B3Asm.Jmp end) bMap''''
              restOpt = if ((isJust tOpt) || (isJust fOpt)) then Just rest else Nothing
          in ((bMap''''', end, nextT''', sMap'), Nothing, restOpt))
  where extractBlock [] = ([], Nothing, [])
        extractBlock (s : rest) = (if (isTerminal s)
                                   then ([], Just s, rest)
                                   else (let (head, end, tail) = extractBlock rest
                                         in (s : head, end, tail)))

        isTerminal (IRT.If _ _ _) = True
        isTerminal (IRT.While _ _ _) = True
        isTerminal (IRT.Return _) = True
        isTerminal (IRT.Raise _) = True
        isTerminal _ = False

        addBlock :: BlockIdent -> [B3Asm.SIns] -> B3Asm.JIns -> BlockMap -> BlockMap
        addBlock b ss j bMap = Map.insert b (B3Asm.BBlock ss j) bMap

        optAdd :: BlockIdent -> Maybe [B3Asm.SIns] -> B3Asm.JIns -> BlockMap -> BlockMap
        optAdd _ Nothing _ bMap = bMap
        optAdd b (Just ss) j bMap = Map.insert b (B3Asm.BBlock ss j) bMap

        transCond :: (TmpIdent, SizeMap) -> IRT.Exp -> BlockIdent -> BlockIdent ->
                     ([B3Asm.SIns], B3Asm.JIns)
        transCond (nextT, sMap) (IRT.Binop op el er) b1 b2 =
          let (tl, tr) = (nextT, nextT + 1)
              B3Asm.CmpOp (Common.CmpOp cmp) = transOp op
              (lss, sz) = transExp el tl (tl + 1, sMap)
              (rss, _) = transExp er tr (tr + 1, sMap)
          in (lss ++ rss, B3Asm.CmpJmp sz cmp (B3Asm.STmp tl) (B3Asm.STmp tr) b1 b2)
        transCond (nextT, sMap) e b1 b2 =
          let t = nextT
              (ss, sz) = transExp e t (nextT + 1, sMap)
          in (ss, B3Asm.CmpJmp sz Common.NE (B3Asm.STmp t) (B3Asm.Imm 0) b1 b2)

{-
  Translates the given straight-line statements.
-}
transStraightLineStmts :: (TmpIdent, SizeMap) -> [IRT.Stmt]
                       -> ((TmpIdent, SizeMap), [B3Asm.SIns])
transStraightLineStmts state stmts =
  let (state', stmtLists) = mapAccumL transSLS state stmts
  in (state', concat stmtLists)
  where transSLS :: (TmpIdent, SizeMap) -> IRT.Stmt -> ((TmpIdent, SizeMap), [B3Asm.SIns])
        transSLS (nextT, sMap) (IRT.Assn (IRT.LTmp t) e) =
          let (ss, sz) = transExp e t (nextT, sMap)
          in ((max (t + 1) nextT, Map.insert t sz sMap), ss)
        transSLS (nextT, sMap) (IRT.Assn (IRT.LMem mem) e) =
          let (ss, sz) = transExp e nextT (nextT + 1, sMap)
          in ((nextT + 1, sMap), ss ++ [B3Asm.Store sz (B3Asm.STmp nextT) (transMem mem)])
        transSLS state (IRT.Memcpy t bytes) =
          (state, [B3Asm.Memcpy bytes (B3Asm.STmp t)])
        transSLS (nextT, sMap) (IRT.MemRead sz t mem) =
          let sMap' = Map.insert t sz sMap
          in ((max (t + 1) nextT, sMap'), [B3Asm.Load sz (transMem mem) (B3Asm.DTmp t)])
        transSLS state (IRT.If e [] []) =
          (state, [])
        transSLS state (IRT.CheckNonZero term err) =
          (state, [B3Asm.CheckNonZero (sizeFromTerm state term) (transTerm term) err])
        transSLS state (IRT.CheckNonNeg term err) =
          (state, [B3Asm.CheckNonNeg (sizeFromTerm state term) (transTerm term) err])
        transSLS state (IRT.CheckEqual term1 term2 err) =
          (state, [B3Asm.CheckEqual (sizeFromTerm state term1)
                                    (transTerm term1) (transTerm term2) err])
        transSLS state (IRT.CheckDivOverflow term1 term2) =
          (state, [B3Asm.CheckDivOverflow (transTerm term1) (transTerm term2)])
        transSLS state (IRT.CheckShiftMax term) =
          (state, [B3Asm.CheckShiftMax (transTerm term)])
        transSLS state (IRT.CheckArrEnd addrTerm idxTerm) =
          (state, [B3Asm.CheckArrEnd (transTerm addrTerm) (transTerm idxTerm)])
        transSLS (nextT, sMap) (IRT.Call sz t fn argExps) =
          let (_, (argSs, argPairs)) = transArgs (nextT, sMap) argExps
              nextT' = max (t + 1) nextT
              sMap' = Map.insert t sz sMap
          in ((nextT', sMap'), concat argSs ++ [B3Asm.Call sz fn argPairs (B3Asm.DTmp t)])
        transSLS (nextT, sMap) (IRT.CallPtr sz t fPtrTerm argExps) =
          let (_, (argSs, argPairs)) = transArgs (nextT, sMap) argExps
              nextT' = max (t + 1) nextT
              sMap' = Map.insert t sz sMap
              callIns = B3Asm.CallPtr sz (transTerm fPtrTerm) argPairs (B3Asm.DTmp t)
          in ((nextT', sMap'), concat argSs ++ [callIns])
        {-
          Given a list of expressions and a free temp, produces:
            1. The next free temp.
            2. A list of lists of setup statements for the expressions.
            3. A list of (size, src) pairs for the expressions.
        -}
        transArgs :: (TmpIdent, SizeMap) -> [IRT.Exp]
                  -> (TmpIdent, ([[B3Asm.SIns]], [(B3Asm.Src, Size)]))
        transArgs (nextT, sMap) argExps =
          let (nextT', pairs) = mapAccumL (transArg sMap) nextT argExps
              (argTmps, argSs, argSizes) = unzip3 [(t', ss, sz') | (t', (ss, sz')) <- pairs]
              argPairs = [(B3Asm.STmp t', sz') | (t', sz') <- zip argTmps argSizes]
          in (nextT', (argSs, argPairs))
          where transArg :: SizeMap -> TmpIdent -> IRT.Exp
                         -> (TmpIdent, (TmpIdent, ([B3Asm.SIns], Size)))
                transArg sMap t e = (t + 1, (t, transExp e t (t + 1, sMap)))

{-
  Granslates the given IRT memory expression to a 3-asm one.
-}
transMem :: IRT.Mem -> B3Asm.Mem
transMem (IRT.ArrMem elemSz addrTerm indTerm o) =
  B3Asm.ArrMem elemSz (transTerm addrTerm) (transTerm indTerm) o
transMem (IRT.PtrMem addrTerm o) = B3Asm.PtrMem (transTerm addrTerm) o

{-
  Generates code that computes the value of an expression e and stores it in t, spilling to
  nextT, nextT + 1, ..., as needed and using the size map to do size lookups for other temps.
-}
transExp :: IRT.Exp -> TmpIdent -> (TmpIdent, SizeMap) -> ([B3Asm.SIns], Size)
transExp (IRT.Term term) t state =
  let sz = sizeFromTerm state term
  in ([B3Asm.Set sz (transTerm term) (B3Asm.DTmp t)], sz)
transExp (IRT.Binop op eL eR) t (nextT, sMap) =
  let (ssL, szL) = transExp eL nextT (nextT + 1, sMap)
      (ssR, _) = transExp eR (nextT + 1) (nextT + 2, sMap)
      (srcL, srcR) = (B3Asm.STmp nextT, B3Asm.STmp (nextT + 1))
      (srcSz, dstSz) = if (isCmpOp op) then (szL, Byte) else (szL, szL)
  in (ssL ++ ssR ++ [B3Asm.Binop srcSz dstSz (transOp op) srcL srcR $ B3Asm.DTmp t], dstSz)
  where isCmpOp :: IRT.Binop -> Bool
        isCmpOp (IRT.ArithOp _) = False
        isCmpOp (IRT.CmpOp _) = True

sizeFromTerm :: (TmpIdent, SizeMap) -> IRT.Term -> Size
sizeFromTerm _ (IRT.NumLit sz _) = sz
sizeFromTerm (_, sMap) (IRT.Tmp t) = sMap Map.! t
sizeFromTerm _ (IRT.FnPtr _) = Common.Quad

transTerm :: IRT.Term -> B3Asm.Src
transTerm (IRT.NumLit _ n) = B3Asm.Imm n
transTerm (IRT.Tmp t) = B3Asm.STmp t
transTerm (IRT.FnPtr fName) = B3Asm.FnPtr fName

-- Translates an IRT operator into a B3Asm operator
transOp :: IRT.Binop -> B3Asm.Binop
transOp (IRT.ArithOp aOp) = B3Asm.ArithOp aOp
transOp (IRT.CmpOp cOp) = B3Asm.CmpOp cOp
