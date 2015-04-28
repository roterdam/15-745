{-
  For combining adjacent sequence functions to improve locality.
-}
module Compile.Trans.Combine (combine) where

import Compile.Types.AST
import Compile.Types.ASTUtils
import qualified Compile.Types.Common as Common
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (mapAccumL)

{-
  Combines consecutive sequence operations into a single sequence operation.
  Algorithm:
    tabulate, rangeSeq, map, filter, combine are PRODUCERS.
    map, filter, combine, reduce are CONSUMERS.
    all other expressions that have a sequence as an argument are CONSUMERS.

  STEP 1:
    For each statement that defines a sequence:
      if the statement after it uses it in a sequence operation exactly once AND
      nowhere else uses it (before redefinition) at all, inline it and remove
      the definition.
    --> For simplicity, just recurse through the rest of the program every time
        you see one of these. It's O(n^2), but it's simpler.

  STEP 2:
    Look at inlined sequences operations, and try to combine them.
    Not as sure how to do this one..
-}
combine :: AST -> AST
combine = combineSeqOps . (astUpdateStmtsR inlineStmt)

{-
  Given a statement and list of following statements, tries to inline this one
  into the first following statement (but only if it's a definition of a
  sequence variable), and returns the resulting list.
-}
inlineStmt :: Stmt -> [Stmt] -> [Stmt]
inlineStmt stmt1@(Assn Common.Set (LIdent v) e) (stmt2:rest)
  | isSequenceOperation e && readsOnce v stmt2 &&
      (directlyWritesTo v stmt2 || neverUses v rest) =
    (substituteInStmt e v stmt2):rest
  | otherwise = stmt1:stmt2:rest
inlineStmt stmt1@(Decl _ v (Just e) (stmt2:rest1)) rest2
  | isSequenceOperation e && readsOnce v stmt2 &&
      (directlyWritesTo v stmt2 || neverUses v rest1) =
    (substituteInStmt e v stmt2):rest1 ++ rest2
  | otherwise = stmt1:rest2
inlineStmt stmt rest = stmt:rest

{-
  Returns true if the given statement is an assignment to the given variable.
-}
directlyWritesTo :: Common.Ident -> Stmt -> Bool
directlyWritesTo v (Assn _ (LIdent v2) _) = v == v2
directlyWritesTo _ _ = False

{-
  Substitutes e for v in the given statement.
-}
substituteInStmt :: Exp -> Common.Ident -> Stmt -> Stmt
substituteInStmt e v (Assn op lv e') =
  Assn op (substituteInLV e v lv) (substituteInE e v e')
substituteInStmt e v (If e' stmts1 stmts2) =
  If (substituteInE e v e') stmts1 stmts2
substituteInStmt e v (While e' stmts) =
  While (substituteInE e v e') stmts
substituteInStmt e v (Return eOp) =
  Return (substituteInEOp e v eOp)
substituteInStmt e v (Decl t v' eOp stmts) =
  Decl t v' (substituteInEOp e v eOp) stmts
substituteInStmt e v (Assert e') =
  Assert (substituteInE e v e')
substituteInStmt e v (Exp e') =
  Exp (substituteInE e v e')

substituteInLV :: Exp -> Common.Ident -> LValue -> LValue
substituteInLV e v lv@(LIdent _) = lv
substituteInLV e v (LStar lv) = LStar (substituteInLV e v lv)
substituteInLV e v (LDot lv f) = LDot (substituteInLV e v lv) f
substituteInLV e v (LIndex lv e') =
  LIndex (substituteInLV e v lv) (substituteInE e v e')

substituteInE :: Exp -> Common.Ident -> Exp -> Exp
substituteInE e v e'@(IntLit _) = e'
substituteInE e v e'@(BoolLit _) = e'
substituteInE e v e'@(CharLit _) = e'
substituteInE e v e'@(StringLit _) = e'
substituteInE e v e'@(Ident v') = if (v == v') then e else e'
substituteInE e v (Binop op e1 e2) =
  Binop op (substituteInE e v e1) (substituteInE e v e2)
substituteInE e v (Unop op e') =
  Unop op (substituteInE e v e')
substituteInE e v (Cond e1 e2 e3) =
  Cond (substituteInE e v e1) (substituteInE e v e2) (substituteInE e v e3)
substituteInE e v (Call e' es) =
  Call (substituteInE e v e') $ map (substituteInE e v) es
substituteInE e v e'@(Alloc _) = e'
substituteInE e v (AllocArray t e') =
  AllocArray t (substituteInE e v e')
substituteInE e v (Index e1 e2) =
  Index (substituteInE e v e1) (substituteInE e v e2)
substituteInE e v (Star e') =
  Star (substituteInE e v e')
substituteInE e v (Dot e' f) =
  Dot (substituteInE e v e') f
substituteInE e v e'@(Amp _) = e'
substituteInE e v (Cast t e') =
  Cast t (substituteInE e v e')
substituteInE e v Null = Null
substituteInE e v (Tabulate e1 e2) =
  Tabulate (substituteInE e v e1) (substituteInE e v e2)
substituteInE e v (ListSeq es) =
  ListSeq (map (substituteInE e v) es)
substituteInE e v (RangeSeq e1 e2) =
  RangeSeq (substituteInE e v e1) (substituteInE e v e2)
substituteInE e v (Map e1 e2) =
  Map (substituteInE e v e1) (substituteInE e v e2)
substituteInE e v (Filter e1 e2) =
  Filter (substituteInE e v e1) (substituteInE e v e2)
substituteInE e v (Combine e1 e2 e3) =
  Combine (substituteInE e v e1) (substituteInE e v e2) (substituteInE e v e3)
substituteInE e v (Reduce e1 e2 e3) =
  Reduce (substituteInE e v e1) (substituteInE e v e2) (substituteInE e v e3)

substituteInEOp :: Exp -> Common.Ident -> Maybe Exp -> Maybe Exp
substituteInEOp e v Nothing = Nothing
substituteInEOp e v (Just e') = Just (substituteInE e v e')



{-
  Returns whether the exp is a sequence operation.
  NOTE: it's possible to have expressions that aren't sequence operations, but
        still produce sequences (examples would be function calls & ternary);
        these aren't inlined because non-sequence-operations wouldn't be able to
        be coalesced in the second pass anyway.
-}
isSequenceOperation :: Exp -> Bool
isSequenceOperation (Tabulate _ _) = True
isSequenceOperation (ListSeq _) = True
isSequenceOperation (RangeSeq _ _) = True
isSequenceOperation (Map _ _) = True
isSequenceOperation (Filter _ _) = True
isSequenceOperation (Combine _ _ _) = True
isSequenceOperation (Reduce _ _ _) = True
isSequenceOperation _ = False

{-
  Returns true if the given statement reads the given variable exactly once.
  NOTE: this gets a little weird for statements that have nested statements;
        we say a statement uses v once if it uses v exactly once in its local
        expression, and never in any of its nested statements.
  NOTE2: this gets additionally weird if the following line is actually a write
         to v. We say that this doesn't matter, since the write happens at the
         end of the computations on this line, and so inlining can still occur
         even if
-}
readsOnce :: Common.Ident -> Stmt -> Bool
readsOnce v (Assn _ lv e) = (countReadsLV v lv + countReadsE v e) == 1
readsOnce v (If e stmts1 stmts2) =
  (countReadsE v e == 1) && neverUses v stmts1 && neverUses v stmts2
readsOnce v (While e stmts) =
  (countReadsE v e == 1) && neverUses v stmts
readsOnce v (Return eOp) = countReadsEOp v eOp == 1
readsOnce v (Decl _ _ eOp stmts) = countReadsEOp v eOp == 1 && neverUses v stmts
readsOnce v (Assert e) = countReadsE v e == 1
readsOnce v (Exp e) = countReadsE v e == 1

countReadsLV :: Common.Ident -> LValue -> Integer
countReadsLV v (LIdent v2) = 0
countReadsLV v (LStar lv) = countReadsLV v lv
countReadsLV v (LDot lv _) = countReadsLV v lv
countReadsLV v (LIndex lv e) = countReadsLV v lv + countReadsE v e

countReadsE :: Common.Ident -> Exp -> Integer
countReadsE v (IntLit _) = 0
countReadsE v (BoolLit _) = 0
countReadsE v (CharLit _) = 0
countReadsE v (StringLit _) = 0
countReadsE v (Ident v2) = if (v == v2) then 1 else 0
countReadsE v (Binop _ e1 e2) = countReadsE v e1 + countReadsE v e2
countReadsE v (Unop _ e) = countReadsE v e
countReadsE v (Cond e1 e2 e3) =
  countReadsE v e1 + countReadsE v e2 + countReadsE v e3
countReadsE v (Call e es) = countReadsE v e + (sum $ map (countReadsE v) es)
countReadsE v (Alloc _) = 0
countReadsE v (AllocArray _ e) = countReadsE v e
countReadsE v (Index e1 e2) = countReadsE v e1 + countReadsE v e2
countReadsE v (Star e) = countReadsE v e
countReadsE v (Dot e _) = countReadsE v e
countReadsE v (Amp _) = 0
countReadsE v (Cast _ e) = countReadsE v e
countReadsE v (Null) = 0
countReadsE v (Tabulate e1 e2) = countReadsE v e1 + countReadsE v e2
countReadsE v (ListSeq es) = sum $ map (countReadsE v) es
countReadsE v (RangeSeq e1 e2) = countReadsE v e1 + countReadsE v e2
countReadsE v (Map e1 e2) = countReadsE v e1 + countReadsE v e2
countReadsE v (Filter e1 e2) = countReadsE v e1 + countReadsE v e2
countReadsE v (Combine e1 e2 e3) =
  countReadsE v e1 + countReadsE v e2 + countReadsE v e3
countReadsE v (Reduce e1 e2 e3) =
  countReadsE v e1 + countReadsE v e2 + countReadsE v e3

countReadsEOp :: Common.Ident -> Maybe Exp -> Integer
countReadsEOp _ Nothing = 0
countReadsEOp v (Just e) = countReadsE v e


{-
  Returns true if the given variable v isn't used before being redefined in the
  given list of statements.
-}
neverUses :: Common.Ident -> [Stmt] -> Bool
neverUses v stmts = not $ fst $ getUsage v stmts

{-
  Takes a variable and list of statements, and returns the usage status of the
  variable as two booleans: could it be read before being used, and is it
  definitely redefined.

  CASES:
    (lv = e):rest  -->  If e or lv reads v, (T, F).
                        If lv writes to v, Defined.
                        Otherwise, recurse on rest.
    (lv op= e):rest  -->  If e or lv reads v, Read.
                          If lv writes to v, Read (since it's an op=).
                          Otherwise, recurse on rest.
    (If e s1 s2):rest -->  If e reads v, Read.
                           If either s1 or s2 reads to v before def, Read.
                           If both s1 and s2 write to v, Defined.
                           Otherwise, recurse on rest.
    (While e ss):rest  -->  If e reads v, Read.
                            If
                            
 
-}
getUsage :: Common.Ident -> [Stmt] -> (Bool, Bool)
getUsage v [] = (False, False)
getUsage v (stmt:rest) =
  let (read, def) = getUsageLocal v stmt
  in if def
     then (read, def)
     else let (read', def') = getUsage v rest
          in (read || read', def')

getUsageLocal :: Common.Ident -> Stmt -> (Bool, Bool)
getUsageLocal v (Assn Common.Set lv e) =
  (readsE v e || readsLV v lv, writesLV v lv)
getUsageLocal v (Assn op lv e) =
  (readsE v e || readsLV v lv || writesLV v lv, writesLV v lv)
getUsageLocal v (If e stmts1 stmts2) =
  let (read1, def1) = getUsage v stmts1
      (read2, def2) = getUsage v stmts2
  in (readsE v e || read1 || read2, def1 && def2)
getUsageLocal v (While e stmts) =
  let (read, _) = getUsage v stmts
  in (readsE v e || read, False)
getUsageLocal v (Return eOp) =
  (readsEOp v eOp, False)
getUsageLocal v (Decl _ _ eOp stmts) =
  let (read, def) = getUsage v stmts
  in (readsEOp v eOp || read, def)
getUsageLocal v (Assert e) =
  (readsE v e, False)
getUsageLocal v (Exp e) =
  (readsE v e, False)



{-
  Returns whether the given variable is read in the given expression.
-}
readsE :: Common.Ident -> Exp -> Bool
readsE v e = not $ countReadsE v e == 0

{-
  Returns whether the given variable is read in the given lvalue.
-}
readsLV :: Common.Ident -> LValue -> Bool
readsLV v e = not $ countReadsLV v e == 0

{-
  Returns whether the given variable is read in the given optional expression.
-}
readsEOp :: Common.Ident -> Maybe Exp -> Bool
readsEOp _ Nothing = False
readsEOp v (Just e) = readsE v e

{-
  Returns whether the given lvalue is a write to v.
-}
writesLV :: Common.Ident -> LValue -> Bool
writesLV v (LIdent v2) = v == v2
writesLV v _ = False



{-
  ------------------------------------------------------------------------------
  STEP 2: combining nested sequence operations
  ------------------------------------------------------------------------------

  The goal is to turn things like map(f2, map(f1, s)) into map(f3, s), where f3
  is a new function that calls f1 and f2. This is split into two steps:
    In step 1, the whole program is looped through, and all candidates of
    combining are identified and replaced with the combined versions. At the
    same time, information about all the new compound functions that need to be
    created is collected and propogated up.
    In step 2, all the new functions that were assumed to exist in step 1 are
    created.
-}

data OpType = TabOp | MapOp | FltOp | CmbOp | RedOp deriving (Eq, Ord)

type FnInfo = Set.Set [(OpType, Common.Ident)]

combineSeqOps :: AST -> AST
combineSeqOps ast =
  let (fnInfo, Prog gDecls') =
        astMapAccumFnStmts combineLV combineE joinFnInfo Set.empty ast
  in Prog $ addNewFunctions fnInfo gDecls'


combineLV :: FnInfo -> LValue -> (FnInfo, LValue)
combineLV fnInfo lv@(LIdent _) = (fnInfo, lv)
combineLV fnInfo (LStar lv) = 
  let (fnInfo', lv') = combineLV fnInfo lv
  in (fnInfo', LStar lv')
combineLV fnInfo (LDot lv f) =
  let (fnInfo', lv') = combineLV fnInfo lv
  in (fnInfo', LDot lv' f)
combineLV fnInfo (LIndex lv e) =
  let (fnInfo', lv') = combineLV fnInfo lv
      (fnInfo'', e') = combineE fnInfo' e
  in (fnInfo'', LIndex lv' e')


combineE :: FnInfo -> Exp -> (FnInfo, Exp)
combineE fnInfo e@(IntLit _) = (fnInfo, e)
combineE fnInfo e@(BoolLit _) = (fnInfo, e)
combineE fnInfo e@(CharLit _) = (fnInfo, e)
combineE fnInfo e@(StringLit _) = (fnInfo, e)
combineE fnInfo e@(Ident _) = (fnInfo, e)
combineE fnInfo (Binop op e1 e2) = 
  let (fnInfo', e1') = combineE fnInfo e1
      (fnInfo'', e2') = combineE fnInfo' e2
  in (fnInfo'', Binop op e1' e2')
combineE fnInfo (Unop op e) =
  let (fnInfo', e') = combineE fnInfo e
  in (fnInfo', Unop op e')
combineE fnInfo (Cond e1 e2 e3) =
  let (fnInfo', e1') = combineE fnInfo e1
      (fnInfo'', e2') = combineE fnInfo' e2
      (fnInfo''', e3') = combineE fnInfo'' e3
  in (fnInfo''', Cond e1' e2' e3')
combineE fnInfo (Call e es) =
  let (fnInfo', e') = combineE fnInfo e
      (fnInfo'', es') = mapAccumL combineE fnInfo' es
  in (fnInfo'', Call e' es')
combineE fnInfo e@(Alloc _) = (fnInfo, e)
combineE fnInfo (AllocArray t e) =
  let (fnInfo', e') = combineE fnInfo e
  in (fnInfo', AllocArray t e')
combineE fnInfo (Index e1 e2) =
  let (fnInfo', e1') = combineE fnInfo e1
      (fnInfo'', e2') = combineE fnInfo' e2
  in (fnInfo'', Index e1' e2')
combineE fnInfo (Star e) =
  let (fnInfo', e') = combineE fnInfo e
  in (fnInfo', Star e')
combineE fnInfo (Dot e f) =
  let (fnInfo', e') = combineE fnInfo e
  in (fnInfo', Dot e' f)
combineE fnInfo e@(Amp _) = (fnInfo, e)
combineE fnInfo (Cast t e) =
  let (fnInfo', e') = combineE fnInfo e
  in (fnInfo', Cast t e')
combineE fnInfo e@(Null) = (fnInfo, e)
combineE fnInfo (Tabulate e1 e2) =
  let (fnInfo', e1') = combineE fnInfo e1
      (fnInfo'', e2') = combineE fnInfo' e2
  in (fnInfo'', Tabulate e1' e2')
combineE fnInfo (ListSeq es) =
  error "ListSeq unsupported"
combineE fnInfo (RangeSeq e1 e2) =
  error "RangeSeq should be elaborated out before the combining optimization"
  {-let (fnInfo', e1') = combineE fnInfo e1
      (fnInfo'', e2') = combineE fnInfo' e2
  in (fnInfo'', RangeSeq e1' e2')-}
combineE fnInfo (Map (Ident fName) e) =
  case (tryLocalCombine MapOp fName fnInfo e) of
    Nothing -> 
      let (fnInfo', e') = combineE fnInfo e
      in (fnInfo', Map (Ident fName) e')
    (Just (e', fnInfo')) -> combineE fnInfo' e'
combineE fnInfo (Filter (Ident fName) e) =
  case (tryLocalCombine FltOp fName fnInfo e) of
    Nothing -> 
      let (fnInfo', e') = combineE fnInfo e
      in (fnInfo', Filter (Ident fName) e')
    (Just (e', fnInfo')) -> combineE fnInfo' e'
combineE fnInfo (Combine (Ident fName) e1 e2) =
  let (fnInfo', e1') = combineE fnInfo e1
      (fnInfo'', e2') = combineE fnInfo' e2
  in (fnInfo'', Combine (Ident fName) e1' e2')
combineE fnInfo (Reduce (Ident fName) e1 e2) =
  let (fnInfo', e1') = combineE fnInfo e1
  in case (tryLocalCombine RedOp fName fnInfo e2) of
      Nothing -> 
        let (fnInfo'', e2') = combineE fnInfo e2
        in (fnInfo'', Reduce (Ident fName) e1' e2')
      (Just (e', fnInfo')) -> combineE fnInfo' e'


{-
  Cases we intend to support in V1:
    tab-map -> tab
    map-map -> map
    flt-flt -> flt
    cmb-map -> cmb
  Cases I wish I could support but won't for now :(
    tab-red -> *special*
    map-red -> red    <-- (using a hack)
    map-cmb -> cmb
    cmb-cmb -> cmb3? cmb*?
    cmb-red -> red3? red*?
-}
tryLocalCombine :: OpType -> Common.Ident -> FnInfo -> Exp
            -> Maybe (Exp, FnInfo)
tryLocalCombine MapOp mapF fnInfo (Tabulate (Ident tabF) e) =
  let newF = combinedFnName tabF mapF
      fnInfo' = Set.insert [(TabOp, tabF), (MapOp, mapF)] fnInfo
  in Just (Tabulate (Ident newF) e, fnInfo')
tryLocalCombine MapOp mapF2 fnInfo (Map (Ident mapF1) e) =
  let newF = combinedFnName mapF1 mapF2
      fnInfo' = Set.insert [(MapOp, mapF1), (MapOp, mapF2)] fnInfo
  in Just (Map (Ident newF) e, fnInfo')
tryLocalCombine FltOp fltF2 fnInfo (Filter (Ident fltF1) e) =
  let newF = combinedFnName fltF1 fltF2
      fnInfo' = Set.insert [(FltOp, fltF1), (FltOp, fltF2)] fnInfo
  in Just (Filter (Ident newF) e, fnInfo')
tryLocalCombine MapOp mapF fnInfo (Combine (Ident combF) e1 e2) =
  let newF = combinedFnName combF mapF
      fnInfo' = Set.insert [(CmbOp, combF), (MapOp, mapF)] fnInfo
  in Just (Combine (Ident newF) e1 e2, fnInfo')
tryLocalCombine _ _ _ _ = Nothing

combinedFnName :: Common.Ident -> Common.Ident -> Common.Ident
combinedFnName f1 f2 = f1 ++ "__then__" ++ f2

joinFnInfo :: FnInfo -> FnInfo -> FnInfo
joinFnInfo = Set.union

type SigMap = Map.Map Common.Ident (Common.Type, [Common.Param])

addNewFunctions :: FnInfo -> [GDecl] -> [GDecl]
addNewFunctions fnInfo gDecls =
  let infoList = Set.toList fnInfo
      sigs = addFnSigs infoList $ getFnSigs gDecls
      decls = map (getFnDecl sigs) infoList
      defns = map (getFnDefn sigs) infoList
  in decls ++ gDecls ++ defns

getFnSigs :: [GDecl] -> SigMap
getFnSigs = foldl addFnSig Map.empty
  where addFnSig :: SigMap -> GDecl -> SigMap
        addFnSig sigs (FDecl t fName params) =
          Map.insert fName (t, params) sigs
        addFnSig sigs (FDefn t fName params _) =
          Map.insert fName (t, params) sigs
        addFnSig sigs _ = sigs

addFnSigs :: [[(OpType, Common.Ident)]] -> SigMap -> SigMap
addFnSigs [] sigs = sigs
addFnSigs l sigs =
  let (sigs', l') = foldl tryAddSig (sigs, []) l
  in addFnSigs l' sigs'
  where tryAddSig :: (SigMap, [[(OpType, Common.Ident)]])
                  -> [(OpType, Common.Ident)]
                  -> (SigMap, [[(OpType, Common.Ident)]])
        tryAddSig (sigs, l) pair@[(_, f1), (_, f2)] =
          case (Map.lookup f1 sigs, Map.lookup f2 sigs) of
            (Just (_, params), Just (t, _)) ->
              (Map.insert (combinedFnName f1 f2) (t, params) sigs, l)
            _ -> (sigs, pair:l)

getFnDecl :: SigMap -> [(OpType, Common.Ident)] -> GDecl
getFnDecl sigs [(op1, f1), (op2, f2)] =
  let fName = combinedFnName f1 f2
      (t, params) = sigs Map.! fName
  in FDecl t fName params

getFnDefn :: SigMap -> [(OpType, Common.Ident)] -> GDecl
getFnDefn sigs [(op1, f1), (op2, f2)] =
  let fName = combinedFnName f1 f2
      (t, params) = sigs Map.! fName
  in FDefn t fName params $ getFnStmts params op1 f1 op2 f2

getFnStmts :: [Common.Param] -> OpType -> Common.Ident -> OpType -> Common.Ident
           -> [Stmt]
getFnStmts [Common.Param _ v] TabOp tabF MapOp mapF =
  [Return $ Just $ Call (Ident mapF) [Call (Ident tabF) [Ident v]]]
getFnStmts [Common.Param _ v] MapOp mapF1 MapOp mapF2 =
  [Return $ Just $ Call (Ident mapF2) [Call (Ident mapF1) [Ident v]]]
getFnStmts [Common.Param _ v] FltOp fltF1 FltOp fltF2 =
  [Return $ Just $ Binop (LogOp $ Common.LAnd) (Call (Ident fltF1) [Ident v])
                                               (Call (Ident fltF2) [Ident v])]
getFnStmts [Common.Param _ v1, Common.Param _ v2] CmbOp cmbF MapOp mapF =
  [Return $ Just $ Call (Ident mapF) [Call (Ident cmbF) [Ident v1, Ident v2]]]
