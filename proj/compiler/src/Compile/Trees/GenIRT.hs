{-
  Converts an PCT into intermediate tree representation.
-}

module Compile.Trees.GenIRT where

import Compile.Types.Common (TmpIdent, Size(..), Error(..))
import qualified Compile.Types.Common as Common
import qualified Compile.Types.PCT as PCT
import qualified Compile.Types.IRT as IRT
import qualified Compile.Trees.Conc as Conc
import qualified Job

import Data.Int (Int32)
import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Data.DList as DList

-- Variable idents to tmps, as well as their sizes
type TmpMap = Map.Map String (TmpIdent, Size)

{-
  Converts a PCT to an IR tree by doing the following:
    - Maps all variables to tmp names.
    - Removes all decls.
    - Expands all expressions into (leadingStmts, expr), adding Check statements before
      expressions that would otherwise be effectful.
    - Translates BoolLits/CharLits to NumLits (true -> 1, false -> 0).
    - Translates NULL to 0.
-}
pctToIRT :: Job.Job -> PCT.PCT -> IRT.IRT
pctToIRT _ (PCT.Prog fns) = IRT.Prog (map transFn fns ++ [tagFn, untagFn, compareTagsFn])
  where {-
          Translates a function from PCT to IRT form.
        -}
        transFn :: PCT.Fn -> IRT.Fn
        transFn (PCT.Fn fName params stmts) =
          let paramPairs = [(p, (t, sz)) | ((p, sz), t) <- zip params [0..]]
              tMap = Map.fromList paramPairs
          in IRT.Fn fName (map snd paramPairs) ((transStmts tMap) $ elabStmts stmts)

        {-
          Does some elaboration on the given statements, turning the unary operators into
          binary operators, and the logical operators into Cond.
          NOTE: the output format is the same type as the input (still a PCT), but with the
                implicit invariant that it has no unops or cond. This is expected (and used) in
                transExp below.
        -}
        elabStmts :: [PCT.Stmt] -> [PCT.Stmt]
        elabStmts = map elabStmt

        elabStmt :: PCT.Stmt -> PCT.Stmt
        elabStmt (PCT.Assn op lv e) = PCT.Assn op (elabLValue lv) (elabExp e)
        elabStmt (PCT.If e ss1 ss2) = PCT.If (elabExp e) (elabStmts ss1) (elabStmts ss2)
        elabStmt (PCT.While e ss) = PCT.While (elabExp e) (elabStmts ss)
        elabStmt (PCT.Return e) = PCT.Return (elabExp e)
        elabStmt (PCT.Decl v sz ss) = PCT.Decl v sz (elabStmts ss)
        elabStmt (PCT.Assert e) = PCT.Assert (elabExp e)
        elabStmt (PCT.Exp e) = PCT.Exp (elabExp e)

        elabLValue :: PCT.LValue -> PCT.LValue
        elabLValue (PCT.LIdent v) = PCT.LIdent v
        elabLValue (PCT.LStar lv oOpt) = PCT.LStar (elabLValue lv) oOpt
        elabLValue (PCT.LIndex esz lv idxE o) = PCT.LIndex esz (elabLValue lv) (elabExp idxE) o

        elabExp :: PCT.Exp -> PCT.Exp
        elabExp e@(PCT.NumLit _) = e
        elabExp e@(PCT.BoolLit _) = e
        elabExp e@(PCT.CharLit _) = e
        elabExp e@(PCT.StringLit _) = e
        elabExp e@(PCT.Null) = e
        elabExp e@(PCT.Ident _) = e
        elabExp (PCT.Binop (PCT.LogOp Common.LAnd) e1 e2) =
          PCT.Cond (elabExp e1) (elabExp e2) (PCT.BoolLit False)
        elabExp (PCT.Binop (PCT.LogOp Common.LOr) e1 e2) =
          PCT.Cond (elabExp e1) (PCT.BoolLit True) (elabExp e2)
        elabExp (PCT.Binop op e1 e2) =
          PCT.Binop op (elabExp e1) (elabExp e2)
        elabExp (PCT.Unop Common.Bang e) =
          PCT.Binop (PCT.CmpOp (Common.CmpOp Common.E)) (elabExp e) (PCT.BoolLit False)
        elabExp (PCT.Unop Common.Inv e) =
          PCT.Binop (PCT.ArithOp Common.AXor) (elabExp e) (PCT.NumLit (-1))
        elabExp (PCT.Unop Common.Neg e) =
          PCT.Binop (PCT.ArithOp Common.ASub) (PCT.NumLit 0) (elabExp e)
        elabExp (PCT.Cond e1 e2 e3) =
          PCT.Cond (elabExp e1) (elabExp e2) (elabExp e3)
        elabExp (PCT.Tag e tag) = PCT.Tag (elabExp e) tag
        elabExp (PCT.Untag e tag) = PCT.Untag (elabExp e) tag
        elabExp (PCT.CompareTagged cOp e1 e2) = PCT.CompareTagged cOp (elabExp e1) (elabExp e2)
        elabExp e@(PCT.Amp _) = e
        elabExp (PCT.Call sz fName args) =
          PCT.Call sz fName (map elabExp args)
        elabExp (PCT.CallPtr sz fnPtrE args) =
          PCT.CallPtr sz (elabExp fnPtrE) (map elabExp args)
        elabExp e@(PCT.Alloc _) = e
        elabExp (PCT.AllocArray esz szE) =
          PCT.AllocArray esz (elabExp szE)
        elabExp (PCT.Index sz esz arrE idxE o) =
          PCT.Index sz esz (elabExp arrE) (elabExp idxE) o
        elabExp (PCT.Star sz addrE o) =
          PCT.Star sz (elabExp addrE) o

{-
  Translates the given list of statements into IRT form.
-}
transStmts :: TmpMap -> [PCT.Stmt] -> [IRT.Stmt]
transStmts tMap stmts = concatMap (transStmt tMap) stmts

{-
  Translates a single statement from PCT to IRT form, given a translation state.
  Evaluation semantics:
    lv = e    ==>  (lv -> address, e -> e', check lv', lv' = e')
    lv op= e  ==>  (lv -> address, e -> e', check lv' lv' -> e'', lv' = e'' op e')
    other operations that just read e  ==>  (e -> e', do the stmt)
-}
transStmt :: TmpMap -> PCT.Stmt -> [IRT.Stmt]
transStmt tMap (PCT.Assn Common.Set lv e) =
  let (preLV, lv', checks, nextT') = transLValue tMap (fromIntegral $ Map.size tMap) lv
      (preE, e', _, _) = transExp tMap nextT' e
      code = [IRT.Assn lv' e']
  in preLV ++ preE ++ checks ++ code
transStmt tMap (PCT.Assn (Common.SetOp op) lv e) =
  let (preLV, lv', checks, nextT') = transLValue tMap (fromIntegral $ Map.size tMap) lv
      (preE, e', sz, nextT'') = transExp tMap nextT' e
      (lvStoreT, nextT''') = (nextT'', nextT'' + 1)
      (preLVStore, lvStore) = (checks, storeLValue lvStoreT sz lv')
      (preOp, opE, _) = transBinop nextT''' (PCT.ArithOp op) (IRT.Term $ IRT.Tmp lvStoreT) e'
      code = [IRT.Assn lv' opE]
  in preLV ++ preE ++ preLVStore ++ [lvStore] ++ preOp ++ code
transStmt tMap (PCT.If e ss1 ss2) =
  let (preE, e', _, _) = transExp tMap (toInteger $ Map.size tMap) e
      ss1' = transStmts tMap ss1
      ss2' = transStmts tMap ss2
  in preE ++ [IRT.If e' ss1' ss2']
transStmt tMap (PCT.While e ss) =
  let (preE, e', _, _) = transExp tMap (toInteger $ Map.size tMap) e
      ss' = transStmts tMap ss
  in [IRT.While preE e' ss']
transStmt tMap (PCT.Return e) =
  let (preE, e', sz, _) = transExp tMap (toInteger $ Map.size tMap) e
  in preE ++ [IRT.Return e']
transStmt tMap (PCT.Decl v sz ss) =
  let t = toInteger $ Map.size tMap
      tMap' = Map.insert v (t, sz) tMap
  in (IRT.Assn (IRT.LTmp t) (IRT.Term $ IRT.NumLit sz 0)) : (transStmts tMap' ss)
transStmt tMap (PCT.Assert e) =
  let (preE, e', _, nextT') = transExp tMap (toInteger $ Map.size tMap) e
  in preE ++ [IRT.Assn (IRT.LTmp nextT') e', IRT.CheckNonZero (IRT.Tmp nextT') AssertError]
transStmt tMap (PCT.Exp e) =
  let t = toInteger $ Map.size tMap
      (preE, _, _, _) = transExp tMap t e
  in preE


{-
  Translates an PCT lValue into an IRT lValue (i.e. an address), preceeding statements to set
  up that lValue, and any checks that should be performed before that lvalue is read/written
  to. Note that it also takes in a temp nextT and returns a temp nextT': all temps used in the
  return statements/lvalue are guaranteed to either be in the tMap or the range
  [nextT, nextT').
  Check semantics:
    LIdent                ==>   ([], no checks)
    LStar lv' Nothing     ==>   (ss for lv', no further checks)
    LStar lv' (Just o)    ==>   (ss for lv', null-check on lv')
    LIndex sz lv' idx o   ==>   (ss for lv', ss for idx, null-check lv', idx-check lv', idx)
-}
transLValue :: TmpMap -> TmpIdent -> PCT.LValue
            -> ([IRT.Stmt], IRT.LValue, [IRT.Stmt], TmpIdent)
transLValue tMap nextT lv =
  let (preLV, lv', checks, nextT') = tLVRec tMap nextT lv
  in (DList.toList preLV, lv', checks, nextT')
  where tLVRec :: TmpMap -> TmpIdent -> PCT.LValue
               -> (DList.DList IRT.Stmt, IRT.LValue, [IRT.Stmt], TmpIdent)
        tLVRec tMap nextT (PCT.LIdent ident) =
          (DList.empty, IRT.LTmp (fst $ tMap Map.! ident), [], nextT)
        tLVRec tMap nextT (PCT.LStar lv oOpt) =
          let (setup, lv', checks, nextT') = tLVRec tMap nextT lv
              (addrT, nextT'') = (nextT', nextT' + 1)
              assnStmts = checks ++ [storeLValue addrT Quad lv']
              checkNullStmt = IRT.CheckNonZero (IRT.Tmp addrT) MemError
              (eagerChecks, checks', o) =
                (case oOpt of
                  Nothing -> ([], [checkNullStmt], 0)
                  Just o -> ([checkNullStmt], [], o))
              setup' = setup `DList.append` (DList.fromList assnStmts)
                             `DList.append` (DList.fromList eagerChecks)
              lv'' = IRT.LMem $ IRT.PtrMem (IRT.Tmp addrT) o
          in (setup', lv'', checks', nextT'')
        tLVRec tMap nextT (PCT.LIndex esz lv idxE o) =
          let (arrSetup, lv', checks, nextT') = tLVRec tMap nextT lv
              (arrT, nextT'') = (nextT', nextT' + 1)
              (idxSetup, idxE', _, nextT''') = transExp tMap nextT'' idxE
              (idxT, nextT'''') = (nextT''', nextT''' + 1)
              assnArrStmts = checks ++ [storeLValue arrT Quad lv']
              assnIdxStmt = IRT.Assn (IRT.LTmp idxT) idxE'
              eagerChecks = [IRT.CheckNonZero (IRT.Tmp arrT) MemError,
                             IRT.CheckNonNeg (IRT.Tmp idxT) MemError,
                             IRT.CheckArrEnd (IRT.Tmp arrT) (IRT.Tmp idxT)]
              setup = arrSetup `DList.append` (DList.fromList assnArrStmts)
                               `DList.append` (DList.fromList idxSetup)
                               `DList.snoc` assnIdxStmt
                               `DList.append` (DList.fromList eagerChecks)
              lv'' = IRT.LMem $ IRT.ArrMem esz (IRT.Tmp arrT) (IRT.Tmp idxT) o
          in (setup, lv'', [], nextT'''')


{-
  Given tMap, nextT, and an expression, turns that expression into a sequence of statements,
  followed by an expression that's guaranteed to not cause an exception. Also returns a next'
  such that all temps used in the return statements or expression are either from the tmpMap or
  the interval [nextT, nextT').
-}
transExp :: TmpMap -> TmpIdent -> PCT.Exp -> ([IRT.Stmt], IRT.Exp, Size, TmpIdent)
transExp tMap nextT e =
  let (preE, e', sz, nextT') = tERec tMap nextT e
  in (DList.toList preE, e', sz, nextT')

{-
  The recursive expression translater. Note that special effort is made to fold cond away here,
  because it generates icky control flow nodes otherwise and this is basically the only place
  where the folding can occur.
-}
tERec :: TmpMap -> TmpIdent -> PCT.Exp -> (DList.DList IRT.Stmt, IRT.Exp, Size, TmpIdent)
tERec _ nextT (PCT.NumLit n) =
  (DList.empty, IRT.Term $ IRT.NumLit Long n, Long, nextT)
tERec _ nextT (PCT.BoolLit b) =
  (DList.empty, IRT.Term $ IRT.NumLit Byte $ Common.boolToInt32 b, Byte, nextT)
tERec _ nextT (PCT.CharLit c) =
  (DList.empty, IRT.Term $ IRT.NumLit Byte $ fromIntegral c, Byte, nextT)
tERec _ nextT (PCT.StringLit s) =
  let (code, nextT') = (stringLitCodeFor s nextT, nextT + 1)
  in (DList.fromList code, IRT.Term $ IRT.Tmp nextT, Quad, nextT')
  where stringLitCodeFor s t =
          let s' :: [Word8]
              s' = (map (fromIntegral . ord) s) ++ [0]
          in [IRT.Call Quad t callocName
                [IRT.Term $ IRT.NumLit Long 1,
                 IRT.Term $ IRT.NumLit Long (fromIntegral $ length s')],
              IRT.CheckNonZero (IRT.Tmp t) MemError,
              IRT.Memcpy t s']
tERec _ nextT (PCT.Null) =
  (DList.empty, IRT.Term $ IRT.NumLit Quad 0, Quad, nextT)
tERec tMap nextT (PCT.Ident ident) =
  let (t, sz) = tMap Map.! ident
  in (DList.empty, IRT.Term $ IRT.Tmp $ t, sz, nextT)
tERec tMap nextT (PCT.Binop op eL eR) =
  let (preL, eL', szL, nextT') = tERec tMap nextT eL
      (preR, eR', _, nextT'') = tERec tMap nextT' eR
      (preOp, eOp, nextT''') = transBinop nextT'' op eL' eR'
      preE = preL `DList.append` preR `DList.append` (DList.fromList preOp)
      sz = (case op of
              (PCT.CmpOp _) -> Byte
              _ -> szL)
  in (preE, eOp, sz, nextT''')
tERec _ _ (PCT.Unop _ _) = error "GenIRT.tERec: unexpected Unop"
tERec tMap nextT (PCT.Cond e1 e2 e3) =
  let (preE1, e1', _, nextT') = tERec tMap nextT e1
  in (case (e1', e2 == e3) of
        (_, True) ->
          let (preE2, e2', sz, nextT'') = tERec tMap nextT' e2
          in (preE1 `DList.append` preE2, e2', sz, nextT'')
        (IRT.Term (IRT.NumLit _ 0), _) ->
          let (preE3, e3', sz, nextT'') = tERec tMap nextT' e3
          in (preE1 `DList.append` preE3, e3', sz, nextT'')
        (IRT.Term (IRT.NumLit _ _), _) ->
          let (preE2, e2', sz, nextT'') = tERec tMap nextT' e2
          in (preE1 `DList.append` preE2, e2', sz, nextT'')
        _ ->
          let (preE2, e2', sz2, nextT'') = tERec tMap nextT' e2
              (preE3, e3', _, nextT''') = tERec tMap nextT'' e3
              (t, nextT'''') = (nextT''', nextT''' + 1)
              stmts2 = DList.toList $ preE2 `DList.snoc` (IRT.Assn (IRT.LTmp t) e2')
              stmts3 = DList.toList $ preE3 `DList.snoc` (IRT.Assn (IRT.LTmp t) e3')
              preE = preE1 `DList.snoc` (IRT.Assn (IRT.LTmp t) (IRT.Term $ IRT.NumLit sz2 0))
                           `DList.snoc` (IRT.If e1' stmts2 stmts3)
          in (preE, IRT.Term $ IRT.Tmp t, sz2, nextT''''))
tERec tMap nextT (PCT.Tag e tag) =
  let (preE, e', _, nextT') = tERec tMap nextT e
      (inPtrT, outPtrT, nextT'') = (nextT', nextT' + 1, nextT' + 2)
      pre = preE `DList.snoc` (IRT.Assn (IRT.LTmp inPtrT) e')
                 `DList.snoc` (IRT.Call Quad outPtrT tagFnName
                                [IRT.Term $ IRT.Tmp inPtrT, IRT.Term $ IRT.NumLit Long tag])
  in (pre, IRT.Term $ IRT.Tmp outPtrT, Quad, nextT'')
tERec tMap nextT (PCT.Untag e tag) =
  let (preE, e', _, nextT') = tERec tMap nextT e
      (inPtrT, outPtrT, nextT'') = (nextT', nextT' + 1, nextT' + 2)
      pre = preE `DList.snoc` (IRT.Assn (IRT.LTmp inPtrT) e')
                 `DList.snoc` (IRT.Call Quad outPtrT untagFnName
                                [IRT.Term $ IRT.Tmp inPtrT, IRT.Term $ IRT.NumLit Long tag])
  in (pre, IRT.Term $ IRT.Tmp outPtrT, Quad, nextT'')
tERec tMap nextT (PCT.CompareTagged cOp e1 e2) =
  let (preE1, e1', _, nextT') = tERec tMap nextT e1
      (preE2, e2', _, nextT'') = tERec tMap nextT' e2
      (t1, t2, t, nextT''') = (nextT'', nextT'' + 1, nextT'' + 2, nextT'' + 3)
      pre = preE1 `DList.snoc` (IRT.Assn (IRT.LTmp t1) e1') `DList.append`
            preE2 `DList.snoc` (IRT.Assn (IRT.LTmp t2) e2') `DList.snoc`
            (IRT.Call Byte t compareTagsFnName [IRT.Term $ IRT.Tmp t1, IRT.Term $ IRT.Tmp t2])
      pre' = case cOp of
               (Common.CmpOp Common.E) -> pre
               (Common.CmpOp Common.NE) ->
                  pre `DList.snoc` (IRT.Assn (IRT.LTmp t)
                                             (IRT.Binop (IRT.CmpOp $ Common.CmpOp Common.E)
                                                        (IRT.Term $ IRT.Tmp t)
                                                        (IRT.Term $ IRT.NumLit Byte 0)))
               _ -> error "GenIRT.tERec: compareTagged with non-equality operator" 
  in (pre', IRT.Term $ IRT.Tmp t, Byte, nextT''')
tERec tMap nextT (PCT.Amp fName) =
  (DList.empty, IRT.Term $ IRT.FnPtr fName, Quad, nextT)
tERec tMap nextT (PCT.Call sz fName args) =
  let (nextT', pairs) = List.mapAccumL (transArg tMap) nextT args
      (pres, args') = unzip pairs
      (t, nextT'') = (nextT', nextT' + 1)
      preE = (DList.concat pres) `DList.snoc` (IRT.Call sz t fName args')
  in (preE, IRT.Term $ IRT.Tmp t, sz, nextT'')
tERec tMap nextT (PCT.CallPtr sz fPtrE args) =
  let (preFPtrE, fPtrE', _, nextT') = tERec tMap nextT fPtrE
      (nextT'', pairs) = List.mapAccumL (transArg tMap) nextT' args
      (preArgs, args') = unzip pairs
      (fPtrT, t, nextT''') = (nextT'', nextT'' + 1, nextT'' + 2)
      preE = preFPtrE `DList.snoc` (IRT.Assn (IRT.LTmp fPtrT) fPtrE')
                      `DList.append` (DList.concat preArgs)
                      `DList.snoc` (IRT.CallPtr sz t (IRT.Tmp fPtrT) args')
  in (preE, IRT.Term $ IRT.Tmp t, sz, nextT''')
tERec _ nextT (PCT.Alloc sz) =
  if sz < 0
  then error "GenIRT.tERec: alloc size negative"
  else (let (code, nextT') = (allocCodeFor sz nextT, nextT + 1)
        in (DList.fromList code, IRT.Term $ IRT.Tmp nextT, Quad, nextT'))
  where allocCodeFor sz t =
          [IRT.Call Quad t callocName [IRT.Term $ IRT.NumLit Long 1,
                                       IRT.Term $ IRT.NumLit Long sz],
           IRT.CheckNonZero (IRT.Tmp t) MemError]
tERec tMap nextT (PCT.AllocArray esz lenE) =
  if esz < 0
  then error "GenIRT.tERec: element size negative"
  else (let (preLen, lenE', _, nextT') = tERec tMap nextT lenE
            (code, nextT'') = allocArrayCodeFor esz lenE' nextT'
            pre = preLen `DList.append` (DList.fromList code)
        in (pre, IRT.Term $ IRT.Tmp nextT', Quad, nextT''))
  where allocArrayCodeFor esz lenE t =
          ([IRT.Assn (IRT.LTmp (t + 1)) lenE,
            IRT.CheckNonNeg (IRT.Tmp (t + 1)) MemError,
            IRT.Call Quad t callocName [IRT.Term $ IRT.NumLit Long 1,
                                        IRT.Binop (IRT.ArithOp Common.AAdd)
                                                  (IRT.Binop (IRT.ArithOp Common.AMul)
                                                             (IRT.Term $ IRT.Tmp (t + 1))
                                                             (IRT.Term $ IRT.NumLit Long esz))
                                                  (IRT.Term $ IRT.NumLit Long 8)],
            IRT.CheckNonZero (IRT.Tmp t) MemError,
            IRT.Assn (IRT.LMem (IRT.ArrMem esz (IRT.Tmp t) (IRT.NumLit Long 0) (-8)))
                     (IRT.Term $ IRT.Tmp (t + 1)),
            IRT.Assn (IRT.LMem (IRT.ArrMem esz (IRT.Tmp t) (IRT.NumLit Long 0) (-4)))
                     (IRT.Term $ IRT.NumLit Long esz)],
           t + 2)
tERec tMap nextT (PCT.Star sz addrE o) =
  let (preAddr, addrE', _, nextT') = tERec tMap nextT addrE
      (t, t2, nextT'') = (nextT', nextT' + 1, nextT' + 2)
      preE = preAddr `DList.snoc` (IRT.Assn (IRT.LTmp t) addrE')
                     `DList.snoc` (IRT.CheckNonZero (IRT.Tmp t) MemError)
                     `DList.snoc` (IRT.MemRead sz t2 (IRT.PtrMem (IRT.Tmp t) o))
  in (preE, IRT.Term $ IRT.Tmp t2, sz, nextT'')
tERec tMap nextT (PCT.Index sz esz arrE idxE o) =
  let (preArr, arrE', _, nextT') = tERec tMap nextT arrE
      (arrT, nextT'') = (nextT', nextT' + 1)
      (preIdx, idxE', _, nextT''') = tERec tMap nextT'' idxE
      (idxT, nextT'''') = (nextT''', nextT''' + 1)
      (memT, nextT''''') = (nextT'''', nextT'''' + 1)
      preE = (preArr `DList.snoc` (IRT.Assn (IRT.LTmp arrT) arrE')) `DList.append`
             (preIdx `DList.snoc` (IRT.Assn (IRT.LTmp idxT) idxE')) `DList.snoc`
             (IRT.CheckNonZero (IRT.Tmp arrT) MemError) `DList.snoc`
             (IRT.CheckNonNeg (IRT.Tmp idxT) MemError) `DList.snoc`
             (IRT.CheckArrEnd (IRT.Tmp arrT) (IRT.Tmp idxT)) `DList.snoc`
             (IRT.MemRead sz memT (IRT.ArrMem esz (IRT.Tmp arrT) (IRT.Tmp idxT) o))
  in (preE, IRT.Term $ IRT.Tmp memT, sz, nextT''''')

{-
  Translates the given function argument into a temp, returning the next temp, setup
  statements, and resulting pure expression.
-}
transArg :: TmpMap -> TmpIdent -> PCT.Exp -> (TmpIdent, (DList.DList IRT.Stmt, IRT.Exp))
transArg tMap t e = let (preE, e', _, t') = tERec tMap t e in (t', (preE, e'))

{-
  Translates a binop into effectful statments and a non-effectful expression.
-}
transBinop :: TmpIdent -> PCT.Binop -> IRT.Exp -> IRT.Exp -> ([IRT.Stmt], IRT.Exp, TmpIdent)
transBinop nextT op l r =
  case transOp op of
    Left pOp -> ([], IRT.Binop pOp l r, nextT)
    Right eOp -> (let (tL, tR, nextT') = (nextT, nextT + 1, nextT + 2)
                      preE = (IRT.Assn (IRT.LTmp tL) l) :
                             (IRT.Assn (IRT.LTmp tR) r) :
                             (getCheck eOp tL tR)
                      e = IRT.Binop eOp (IRT.Term $ IRT.Tmp tL) (IRT.Term $ IRT.Tmp tR)
                  in (preE, e, nextT'))
  where getCheck :: IRT.Binop -> TmpIdent -> TmpIdent -> [IRT.Stmt]
        getCheck (IRT.ArithOp Common.ADiv) tL tR =
          [IRT.CheckNonZero (IRT.Tmp tR) ArithError,
           IRT.CheckDivOverflow (IRT.Tmp tL) (IRT.Tmp tR)]
        getCheck (IRT.ArithOp Common.AMod) tL tR =
          [IRT.CheckNonZero (IRT.Tmp tR) ArithError,
           IRT.CheckDivOverflow (IRT.Tmp tL) (IRT.Tmp tR)]
        getCheck (IRT.ArithOp Common.AShL) tL tR =
          [IRT.CheckNonNeg (IRT.Tmp tR) ArithError,
           IRT.CheckShiftMax (IRT.Tmp tR)]
        getCheck (IRT.ArithOp Common.AShR) tL tR =
          [IRT.CheckNonNeg (IRT.Tmp tR) ArithError,
           IRT.CheckShiftMax (IRT.Tmp tR)]

        transOp :: PCT.Binop -> Either IRT.Binop IRT.Binop
        transOp (PCT.ArithOp Common.AShL) = Right $ IRT.ArithOp Common.AShL
        transOp (PCT.ArithOp Common.AShR) = Right $ IRT.ArithOp Common.AShR
        transOp (PCT.ArithOp Common.ADiv) = Right $ IRT.ArithOp Common.ADiv
        transOp (PCT.ArithOp Common.AMod) = Right $ IRT.ArithOp Common.AMod
        transOp (PCT.ArithOp arithop)     = Left $ IRT.ArithOp arithop
        transOp (PCT.CmpOp cmpop)         = Left $ IRT.CmpOp cmpop
        transOp (PCT.LogOp _)             = error "GenIRT.transOp: unexpected LogOp"

{-
  Given a temp t and lvalue lv, generates code that writes the value stored at lv into t.
-}
storeLValue :: TmpIdent -> Size -> IRT.LValue -> IRT.Stmt
storeLValue t _ (IRT.LTmp t') = IRT.Assn (IRT.LTmp t) (IRT.Term $ IRT.Tmp t')
storeLValue t sz (IRT.LMem mem) = IRT.MemRead sz t mem

-- TODO: Move this out into somewhere common?
callocName = Common.wrapFnName "calloc"
tagFnName = Common.wrapFnName "tag"
untagFnName = Common.wrapFnName "untag"
compareTagsFnName = Common.wrapFnName "compareTags"

tagFn = IRT.Fn tagFnName [(0, Quad), (1, Long)]
          [IRT.If (IRT.Binop (IRT.CmpOp $ Common.CmpOp Common.E)
                             (IRT.Term $ IRT.Tmp 0) (IRT.Term $ IRT.NumLit Quad 0))
            [IRT.Return $ IRT.Term $ IRT.Tmp 0]
            [IRT.Call Quad 2 callocName [IRT.Term $ IRT.NumLit Long 1,
                                         IRT.Term $ IRT.NumLit Long 16],
             IRT.CheckNonZero (IRT.Tmp 2) MemError,
             IRT.Assn (IRT.LMem $ IRT.PtrMem (IRT.Tmp 2) 0) (IRT.Term $ IRT.Tmp 1),
             IRT.Assn (IRT.LMem $ IRT.PtrMem (IRT.Tmp 2) 8) (IRT.Term $ IRT.Tmp 0),
             IRT.Return $ IRT.Term $ IRT.Tmp 2]]

untagFn = IRT.Fn untagFnName [(0, Quad), (1, Long)]
            [IRT.If (IRT.Binop (IRT.CmpOp $ Common.CmpOp Common.E)
                               (IRT.Term $ IRT.Tmp 0) (IRT.Term $ IRT.NumLit Quad 0))
              [IRT.Return $ IRT.Term $ IRT.Tmp 0]
              [IRT.MemRead Long 2 (IRT.PtrMem (IRT.Tmp 0) 0),
               IRT.CheckEqual (IRT.Tmp 1) (IRT.Tmp 2) MemError,
               IRT.MemRead Quad 3 (IRT.PtrMem (IRT.Tmp 0) 8),
               IRT.Return $ IRT.Term $ IRT.Tmp 3]]

compareTagsFn = IRT.Fn compareTagsFnName [(0, Quad), (1, Quad)]
                  [IRT.If (IRT.Binop (IRT.CmpOp $ Common.CmpOp Common.E)
                                     (IRT.Term $ IRT.Tmp 0) (IRT.Term $ IRT.NumLit Quad 0))
                    [IRT.Return $ IRT.Binop (IRT.CmpOp $ Common.CmpOp Common.E)
                                            (IRT.Term $ IRT.Tmp 1)
                                            (IRT.Term $ IRT.NumLit Quad 0)]
                    [],
                   IRT.If (IRT.Binop (IRT.CmpOp $ Common.CmpOp Common.E)
                                     (IRT.Term $ IRT.Tmp 1) (IRT.Term $ IRT.NumLit Quad 0))
                    [IRT.Return $ IRT.Term $ IRT.NumLit Byte 0]
                    [],
                   IRT.MemRead Quad 2 (IRT.PtrMem (IRT.Tmp 0) 8),
                   IRT.MemRead Quad 3 (IRT.PtrMem (IRT.Tmp 1) 8),
                   IRT.Return $ IRT.Binop (IRT.CmpOp $ Common.CmpOp Common.E)
                                          (IRT.Term $ IRT.Tmp 2) (IRT.Term $ IRT.Tmp 3)]
