{-
  Converts ASTs into CTrees. Relevant changes are:
  --> types are translated:
      --> arrays are turned into pointers.
      --> strings are turned into Char *s.
  --> alloc and alloc_array are turned into calloc.
  --> The distinction between external and local declarations is removed.
  TO ADD:
  --> sequences are mapped to pointers to structs with arrays and size.
  --> tabulate, map, reduce, filter, and combine are mapped to special functions
      that are added to the program as GDecls.
  --> sequence literals are mapped to a custom block of code that inlines the
      creation of the sequence.
-}

module Compile.Trans.GenCTree where

import Compile.Trans.ElabSeq
import qualified Compile.Trans.Conc as Conc
import qualified Compile.Types.Common as Common
import qualified Compile.Types.AST as AST
import qualified Compile.Types.CTree as CTree
import qualified Job as Job
import Compile.Trans.CheckState (GlobalState(..))

import qualified Data.Map as Map

assertFnName :: String
assertFnName = "assert"
callocFnName :: String
callocFnName = "calloc"

{-
  A subset of the GlobalState used in typechecking.
-}
type SigMap = Map.Map Common.Ident (Conc.Conc, [Conc.Conc])
type TypedefMap = Map.Map Common.Ident Conc.Conc
type GlobalInfo = (SigMap, TypedefMap)
emptyGI :: GlobalInfo
emptyGI = (Map.empty, Map.empty)

astToCTree :: AST.AST -> CTree.CTree
astToCTree (AST.Prog gDecls) =
  let gi = foldl addInfoFromGDecl emptyGI gDecls
      seqInfo = getSeqInfo gi gDecls
  in  CTree.Prog $ concat [stdIncludes,
                           getSeqDecls seqInfo,
                           map transGDecl gDecls,
                           getSeqDefns seqInfo]
  where stdIncludes :: [CTree.GDecl]
        stdIncludes = [CTree.Include "<stdlib.h>", CTree.Include "<stdbool.h>",
                       CTree.Include "<assert.h>"]

        addInfoFromGDecl :: GlobalInfo -> AST.GDecl -> GlobalInfo
        addInfoFromGDecl gi@(sigs, typedefs) (AST.Typedef t tName) =
          (sigs, Map.insert tName (getConcrete gi t) typedefs)
        addInfoFromGDecl gi (AST.Sigdef _ _ _) = gi
        addInfoFromGDecl gi@(sigs, typedefs) (AST.FDecl t fName params) =
          let resC = getConcrete gi t
              paramsC = map (\(Common.Param t _) -> getConcrete gi t) params
          in (Map.insert fName (resC, paramsC) sigs, typedefs)
        addInfoFromGDecl gi@(sigs, typedefs) (AST.FExt t fName params) =
          let resC = getConcrete gi t
              paramsC = map (\(Common.Param t _) -> getConcrete gi t) params
          in (Map.insert fName (resC, paramsC) sigs, typedefs)
        addInfoFromGDecl gi@(sigs, typedefs) (AST.FDefn t fName params _) =
          let resC = getConcrete gi t
              paramsC = map (\(Common.Param t _) -> getConcrete gi t) params
          in (Map.insert fName (resC, paramsC) sigs, typedefs)
        addInfoFromGDecl gi (AST.SDefn _ _) = gi

        getSeqInfo :: GlobalInfo -> [AST.GDecl] -> SeqInfo
        getSeqInfo gi = foldl (addFromGDecl gi) emptySeqInfo

        addFromGDecl :: GlobalInfo -> SeqInfo -> AST.GDecl -> SeqInfo
        addFromGDecl gi si (AST.Typedef t _) = addFromType gi t si
        addFromGDecl gi si (AST.Sigdef t _ params) =
          addFromParams gi params $ addFromType gi t si
        addFromGDecl gi si (AST.FDecl t _ params) =
          addFromParams gi params $ addFromType gi t si
        addFromGDecl gi si (AST.FExt t _ params) =
          addFromParams gi params $ addFromType gi t si
        addFromGDecl gi si (AST.FDefn t _ params stmts) =
          addFromStmts gi (addFromParams gi params $ addFromType gi t si) stmts
        addFromGDecl gi si (AST.SDefn _ params) = addFromParams gi params si

        addFromParams :: GlobalInfo -> [Common.Param] -> SeqInfo -> SeqInfo
        addFromParams gi params si = foldl (addFromParam gi) si params

        addFromParam :: GlobalInfo -> SeqInfo -> Common.Param -> SeqInfo
        addFromParam gi si (Common.Param t _) = addFromType gi t si

        addFromStmts :: GlobalInfo -> SeqInfo -> [AST.Stmt] -> SeqInfo
        addFromStmts gi = foldl (addFromStmt gi)

        addFromStmt :: GlobalInfo -> SeqInfo -> AST.Stmt -> SeqInfo
        addFromStmt gi si (AST.Assn _ lv e) =
          addFromExp gi e $ addFromLValue gi lv si
        addFromStmt gi si (AST.If e tStmts fStmts) =
          addFromStmts gi (addFromStmts gi (addFromExp gi e si) tStmts) fStmts
        addFromStmt gi si (AST.While e stmts) =
          addFromStmts gi (addFromExp gi e si) stmts
        addFromStmt gi si (AST.Return eOpt) = addFromEOpt gi eOpt si
        addFromStmt gi si (AST.Decl t v eOpt stmts) =
          addFromStmts gi (addFromEOpt gi eOpt $ addFromType gi t si) stmts
        addFromStmt gi si (AST.Assert e) = addFromExp gi e si
        addFromStmt gi si (AST.Exp e) = addFromExp gi e si

        addFromExp :: GlobalInfo -> AST.Exp -> SeqInfo -> SeqInfo
        addFromExp _ (AST.IntLit _) si = si
        addFromExp _ (AST.BoolLit _) si = si
        addFromExp _ (AST.CharLit _) si = si
        addFromExp _ (AST.StringLit _) si = si
        addFromExp _ (AST.Ident _) si = si
        addFromExp gi (AST.Binop _ e1 e2) si =
          addFromExp gi e1 $ addFromExp gi e2 si
        addFromExp gi (AST.Unop _ e) si = addFromExp gi e si
        addFromExp gi (AST.Cond e1 e2 e3) si =
          addFromExp gi e1 $ addFromExp gi e2 $ addFromExp gi e3 si
        addFromExp gi (AST.Call e es) si =
          addFromExp gi e $ foldr (addFromExp gi) si es
        addFromExp _ (AST.Alloc _) si = si
        addFromExp gi (AST.AllocArray _ e) si = addFromExp gi e si
        addFromExp gi (AST.Index e1 e2) si =
          addFromExp gi e1 $ addFromExp gi e2 si
        addFromExp gi (AST.Star e) si = addFromExp gi e si
        addFromExp gi (AST.Dot e _) si = addFromExp gi e si
        addFromExp gi (AST.Amp _) si = si
        addFromExp gi (AST.Cast _ e) si = addFromExp gi e si
        addFromExp _ (AST.Null) si = si
        addFromExp gi@(sigs, _) (AST.Tabulate (AST.Ident f) e) si =
          let si' = addFromExp gi e si
              (retC, _) = sigs Map.! f
          in addTabulateCall si' f retC
        addFromExp _ (AST.ListSeq es) si = error "List seq unimplemented"
        addFromExp gi (AST.RangeSeq e1 e2) si =
          addFromExp gi e2 $ addFromExp gi e1 $ addRangeCall $
          addSeqType si Conc.IntC
        addFromExp gi@(sigs, _) (AST.Map (AST.Ident f) e) si =
          let si' = addFromExp gi e si
              (retC, [paramC]) = sigs Map.! f
          in addMapCall si' f retC paramC
        addFromExp gi@(sigs, _) (AST.Reduce (AST.Ident f) e1 e2) si =
          let si' = addFromExp gi e1 $ addFromExp gi e2 si
              (retC, _) = sigs Map.! f
          in addReduceCall si' f retC
        addFromExp gi@(sigs, _) (AST.Filter (AST.Ident f) e) si =
          let si' = addFromExp gi e si
              (_, [argC]) = sigs Map.! f
          in addFilterCall si' f argC
        addFromExp gi@(sigs, _) (AST.Combine (AST.Ident f) e1 e2) si =
          let si' = addFromExp gi e1 $ addFromExp gi e2 si
              (retC, [arg1C, arg2C]) = sigs Map.! f
          in addCombineCall si' f retC arg1C arg2C

        addFromLValue :: GlobalInfo -> AST.LValue -> SeqInfo -> SeqInfo
        addFromLValue gi (AST.LIdent _) si = si
        addFromLValue gi (AST.LStar lv) si = addFromLValue gi lv si
        addFromLValue gi (AST.LDot lv _) si = addFromLValue gi lv si
        addFromLValue gi (AST.LIndex lv e) si =
          addFromLValue gi lv $ addFromExp gi e si

        addFromEOpt :: GlobalInfo -> (Maybe AST.Exp) -> SeqInfo -> SeqInfo
        addFromEOpt _ Nothing si = si
        addFromEOpt gi (Just e) si = addFromExp gi e si

        addFromType :: GlobalInfo -> Common.Type -> SeqInfo -> SeqInfo
        addFromType gi t si =
          case (getConcrete gi t) of
            (Conc.SeqC c) -> addSeqType si c
            _ -> si

        getConcrete :: GlobalInfo -> Common.Type -> Conc.Conc
        getConcrete _ Common.VoidT = Conc.VoidC
        getConcrete _ Common.StringT = Conc.StringC
        getConcrete _ Common.CharT = Conc.CharC
        getConcrete _ Common.IntT = Conc.IntC
        getConcrete _ Common.BoolT = Conc.BoolC
        getConcrete _ (Common.StructT ident) = Conc.StructC ident
        getConcrete _ (Common.FnT ident) = Conc.FnC ident
        getConcrete gi (Common.ArrT t) = Conc.ArrC $ getConcrete gi t
        getConcrete gi (Common.SeqT t) = Conc.SeqC $ getConcrete gi t
        getConcrete gi (Common.PtrT t) = Conc.PtrC $ Conc.One $ getConcrete gi t
        getConcrete (_, typedefs) (Common.DefT ident) = typedefs Map.! ident



transGDecl :: AST.GDecl -> CTree.GDecl
transGDecl (AST.Typedef t ident) =
  CTree.Typedef (transType t) ident
transGDecl (AST.Sigdef t ident params) =
  CTree.Sigdef (transType t) ident $ map transParam params
transGDecl (AST.FDecl t ident params) =
  CTree.FDecl (transType t) ident $ map transParam params
transGDecl (AST.FExt t ident params) =
  CTree.FDecl (transType t) ident $ map transParam params
transGDecl (AST.FDefn t ident params stmts) =
  CTree.FDefn (transType t) ident (map transParam params) $ transStmts stmts
transGDecl (AST.SDefn ident params) =
  CTree.SDefn ident $ map transParam params

transStmts :: [AST.Stmt] -> [CTree.Stmt]
transStmts = map transStmt

transStmt :: AST.Stmt -> CTree.Stmt
transStmt (AST.Assn op lv e) = CTree.Assn op (transLValue lv) (transExp e)
transStmt (AST.If e1 tStmts fStmts) =
  CTree.If (transExp e1) (transStmts tStmts) (transStmts fStmts)
transStmt (AST.While e stmts) = CTree.While (transExp e) (transStmts stmts)
transStmt (AST.Return Nothing) = CTree.Return Nothing
transStmt (AST.Return (Just e)) = CTree.Return $ Just $ transExp e
transStmt (AST.Decl t ident Nothing stmts) =
  CTree.Decl (transType t) ident Nothing $ transStmts stmts
transStmt (AST.Decl t ident (Just e) stmts) =
  CTree.Decl (transType t) ident (Just $ transExp e) (transStmts stmts)
transStmt (AST.Assert e) =
  CTree.Exp $ CTree.Call (CTree.Ident assertFnName) [transExp e]
transStmt (AST.Exp e) = CTree.Exp $ transExp e

transLValue :: AST.LValue -> CTree.LValue
transLValue (AST.LIdent ident) = CTree.LIdent ident
transLValue (AST.LStar lv) = CTree.LStar $ transLValue lv
transLValue (AST.LDot lv ident) = CTree.LDot (transLValue lv) ident
transLValue (AST.LIndex lv e) = CTree.LIndex (transLValue lv) (transExp e)

transExp :: AST.Exp -> CTree.Exp
transExp (AST.IntLit n) = CTree.IntLit n
transExp (AST.BoolLit b) = CTree.BoolLit b
transExp (AST.CharLit c) = CTree.CharLit c
transExp (AST.StringLit s) = CTree.StringLit s
transExp (AST.Ident ident) = CTree.Ident ident
transExp (AST.Binop op e1 e2) =
  CTree.Binop (transBinop op) (transExp e1) (transExp e2)
transExp (AST.Unop op e) = CTree.Unop op (transExp e)
transExp (AST.Cond e1 e2 e3) =
  CTree.Cond (transExp e1) (transExp e2) (transExp e3)
transExp (AST.Call e args) = CTree.Call (transExp e) (map transExp args)
transExp (AST.Alloc t) =
  CTree.Call (CTree.Ident callocFnName) [CTree.IntLit $ Common.Dec 1,
                                         CTree.Sizeof $ transType t]
transExp (AST.AllocArray t e) =
  CTree.Call (CTree.Ident callocFnName) [transExp e, CTree.Sizeof $ transType t]
transExp (AST.Index e1 e2) = CTree.Index (transExp e1) (transExp e2)
transExp (AST.Star e) = CTree.Star (transExp e)
transExp (AST.Dot e ident) = CTree.Dot (transExp e) ident
transExp (AST.Amp ident) = CTree.Amp ident
transExp (AST.Cast t e) = CTree.Cast (transType t) (transExp e)
transExp (AST.Null) = CTree.Null
transExp (AST.ListSeq _) = error "list seq unimplemented"
transExp (AST.RangeSeq e1 e2) =
  CTree.Call (CTree.Ident $ libFnName "range" "") [transExp e1, transExp e2]
transExp (AST.Tabulate (AST.Ident fName) e) =
  CTree.Call (CTree.Ident $ libFnName "tabulate" fName) [transExp e]
transExp (AST.Map (AST.Ident fName) e) =
  CTree.Call (CTree.Ident $ libFnName "map" fName) [transExp e]
transExp (AST.Reduce (AST.Ident fName) e1 e2) =
  CTree.Call (CTree.Ident $ libFnName "reduce" fName) [transExp e1, transExp e2]
transExp (AST.Filter (AST.Ident fName) e) =
  CTree.Call (CTree.Ident $ libFnName "filter" fName) [transExp e]
transExp (AST.Combine (AST.Ident fName) e1 e2) =
  CTree.Call (CTree.Ident $ libFnName "combine" fName) [transExp e1, transExp e2]

transBinop :: AST.Binop -> CTree.Binop
transBinop (AST.ArithOp op) = CTree.ArithOp op
transBinop (AST.CmpOp op) = CTree.CmpOp op
transBinop (AST.LogOp op) = CTree.LogOp op

transParam :: Common.Param -> CTree.CParam
transParam (Common.Param t ident) = CTree.CParam (transType t) ident

transType :: Common.Type -> CTree.CType
transType (Common.DefT ident) = CTree.CDefT ident
transType (Common.StructT ident) = CTree.CStructT ident
transType (Common.FnT ident) = CTree.CFnT ident
transType (Common.ArrT t) = CTree.CPtrT $ transType t
transType (Common.PtrT t) = CTree.CPtrT $ transType t
transType (Common.IntT) = CTree.CIntT
transType (Common.BoolT) = CTree.CBoolT
transType (Common.VoidT) = CTree.CVoidT
transType (Common.CharT) = CTree.CCharT
transType (Common.StringT) = CTree.CPtrT CTree.CCharT
transType (Common.SeqT Common.BoolT) =
  CTree.CPtrT $ CTree.CDefT $ seqTypeName Conc.BoolC
transType (Common.SeqT Common.IntT) =
  CTree.CPtrT $ CTree.CDefT $ seqTypeName Conc.IntC
