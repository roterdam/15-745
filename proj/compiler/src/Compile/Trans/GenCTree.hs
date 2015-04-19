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



astToCTree :: GlobalState -> AST.AST -> CTree.CTree
astToCTree gs (AST.Prog gDecls) =
  let seqInfo = getSeqInfo gDecls
  in  CTree.Prog $ concat [stdIncludes,
                           getSeqDecls seqInfo,
                           map transGDecl gDecls,
                           getSeqDefns seqInfo]
  where stdIncludes :: [CTree.GDecl]
        stdIncludes = [CTree.Include "<stdlib.h>", CTree.Include "<stdbool.h>",
                       CTree.Include "<assert.h>"]

        getSeqInfo :: [AST.GDecl] -> SeqInfo
        getSeqInfo = foldl addFromGDecl emptySeqInfo

        addFromGDecl :: SeqInfo -> AST.GDecl -> SeqInfo
        addFromGDecl si (AST.Typedef t _) = addFromType t si
        addFromGDecl si (AST.Sigdef t _ params) =
          addFromParams params $ addFromType t si
        addFromGDecl si (AST.FDecl t _ params) =
          addFromParams params $ addFromType t si
        addFromGDecl si (AST.FExt t _ params) =
          addFromParams params $ addFromType t si
        addFromGDecl si (AST.FDefn t _ params stmts) =
          addFromStmts (addFromParams params $ addFromType t si) stmts
        addFromGDecl si (AST.SDefn _ params) = addFromParams params si

        addFromParams :: [Common.Param] -> SeqInfo -> SeqInfo
        addFromParams params si = foldl addFromParam si params

        addFromParam :: SeqInfo -> Common.Param -> SeqInfo
        addFromParam si (Common.Param t _) = addFromType t si

        addFromStmts :: SeqInfo -> [AST.Stmt] -> SeqInfo
        addFromStmts = foldl addFromStmt

        addFromStmt :: SeqInfo -> AST.Stmt -> SeqInfo
        addFromStmt si (AST.Assn _ lv e) = addFromExp e $ addFromLValue lv si
        addFromStmt si (AST.If e tStmts fStmts) =
          addFromStmts (addFromStmts (addFromExp e si) tStmts) fStmts
        addFromStmt si (AST.While e stmts) =
          addFromStmts (addFromExp e si) stmts
        addFromStmt si (AST.Return eOpt) = addFromEOpt eOpt si
        addFromStmt si (AST.Decl t v eOpt stmts) =
          addFromStmts (addFromEOpt eOpt $ addFromType t si) stmts
        addFromStmt si (AST.Assert e) = addFromExp e si
        addFromStmt si (AST.Exp e) = addFromExp e si

        addFromExp :: AST.Exp -> SeqInfo -> SeqInfo
        addFromExp (AST.IntLit _) si = si
        addFromExp (AST.BoolLit _) si = si
        addFromExp (AST.CharLit _) si = si
        addFromExp (AST.StringLit _) si = si
        addFromExp (AST.Ident _) si = si
        addFromExp (AST.Binop _ e1 e2) si = addFromExp e1 $ addFromExp e2 si
        addFromExp (AST.Unop _ e) si = addFromExp e si
        addFromExp (AST.Cond e1 e2 e3) si =
          addFromExp e1 $ addFromExp e2 $ addFromExp e3 si
        addFromExp (AST.Call e es) si =
          addFromExp e $ foldr addFromExp si es
        addFromExp (AST.Alloc _) si = si
        addFromExp (AST.AllocArray _ e) si = addFromExp e si
        addFromExp (AST.Index e1 e2) si = addFromExp e1 $ addFromExp e2 si
        addFromExp (AST.Star e) si = addFromExp e si
        addFromExp (AST.Dot e _) si = addFromExp e si
        addFromExp (AST.Amp _) si = si
        addFromExp (AST.Cast _ e) si = addFromExp e si
        addFromExp (AST.Null) si = si
        addFromExp (AST.Tabulate (AST.Ident f) e) si =
          let si' = addFromExp e si
              (_, (retC, _)) = (fnSigs gs) Map.! f
          in addTabulateCall si' f retC
        addFromExp (AST.ListSeq es) si = error "unimplemented"
        addFromExp (AST.RangeSeq e1 e2) si =
          addFromExp e2 $ addFromExp e1 $ addRangeCall $ addSeqType si Conc.IntC
        addFromExp (AST.Map (AST.Ident f) e) si =
          let si' = addFromExp e si
              (_, (retC, [paramC])) = (fnSigs gs) Map.! f
          in addMapCall si' f retC paramC
        addFromExp (AST.Reduce (AST.Ident f) e1 e2) si =
          let si' = addFromExp e1 $ addFromExp e2 si
              (_, (retC, _)) = (fnSigs gs) Map.! f
          in addReduceCall si' f retC
        addFromExp (AST.Filter (AST.Ident f) e) si =
          let si' = addFromExp e si
              (_, (_, [argC])) = (fnSigs gs) Map.! f
          in addFilterCall si' f argC
        addFromExp (AST.Combine (AST.Ident f) e1 e2) si =
          let si' = addFromExp e1 $ addFromExp e2 si
              (_, (retC, [arg1C, arg2C])) = (fnSigs gs) Map.! f
          in addCombineCall si' f retC arg1C arg2C

        addFromLValue :: AST.LValue -> SeqInfo -> SeqInfo
        addFromLValue (AST.LIdent _) si = si
        addFromLValue (AST.LStar lv) si = addFromLValue lv si
        addFromLValue (AST.LDot lv _) si = addFromLValue lv si
        addFromLValue (AST.LIndex lv e) si = addFromLValue lv $ addFromExp e si

        addFromEOpt :: (Maybe AST.Exp) -> SeqInfo -> SeqInfo
        addFromEOpt Nothing si = si
        addFromEOpt (Just e) si = addFromExp e si

        addFromType :: Common.Type -> SeqInfo -> SeqInfo
        addFromType t si =
          case (getConcrete t) of
            (Conc.SeqC c) -> addSeqType si c
            _ -> si

        getConcrete :: Common.Type -> Conc.Conc
        getConcrete Common.VoidT = Conc.VoidC
        getConcrete Common.StringT = Conc.StringC
        getConcrete Common.CharT = Conc.CharC
        getConcrete Common.IntT = Conc.IntC
        getConcrete Common.BoolT = Conc.BoolC
        getConcrete (Common.StructT ident) = Conc.StructC ident
        getConcrete (Common.FnT ident) = Conc.FnC ident
        getConcrete (Common.ArrT t) = Conc.ArrC $ getConcrete t
        getConcrete (Common.SeqT t) = Conc.SeqC $ getConcrete t
        getConcrete (Common.PtrT t) = Conc.PtrC $ Conc.One $ getConcrete t
        getConcrete (Common.DefT ident) = (typedefs gs) Map.! ident



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
transExp (AST.ListSeq _) = error "unimplemented"
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
