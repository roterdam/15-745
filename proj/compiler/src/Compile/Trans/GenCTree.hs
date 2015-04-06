{-
  Converts ASTs into CTrees.
-}

module Compile.Trans.GenCTree where

import qualified Compile.Types.Common as Common
import qualified Compile.Types.AST as AST
import qualified Compile.Types.CTree as CTree
import qualified Job as Job

assertFnName :: String
assertFnName = "assert"
callocFnName :: String
callocFnName = "calloc"

stdIncludes :: [CTree.GDecl]
stdIncludes = [CTree.Include "<stdlib.h>", CTree.Include "<stdbool.h>"]

astToCTree :: Job.Job -> AST.AST -> CTree.CTree
astToCTree _ (AST.Prog gs) = CTree.Prog $ stdIncludes ++ (map transGDecl gs)

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
