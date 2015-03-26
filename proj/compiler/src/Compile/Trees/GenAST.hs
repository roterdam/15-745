{-
  This file contains functions for elaborating ParseTrees into ASTs.
-}

module Compile.Trees.GenAST where

import qualified Compile.Types.Common as Common
import qualified Compile.Types.AST as AST
import qualified Compile.Types.ParseTree as PT
import qualified Job

{-
  Transforms a ParseTree to an AST. In addition to straightforward syntax conversions:
    * removes postops, and replaces them with equivalent binary ops
    * removes logical ops, and replaces them with cond expressions
    * removes unary ops, and replaces them with equivalent binop statements
    * removes -> operators, and replaces them with * and . operators
    * transforms for-loops into while-loops
    * remove struct declarations, since they have no semantic meaning
-}
ptToAST :: Job.Job -> PT.ParseTree -> AST.AST
ptToAST _ (PT.Prog gDecls) = AST.Prog $ concatMap elabGDecl gDecls
  where elabGDecl :: PT.GDecl -> [AST.GDecl]
        elabGDecl (PT.Typedef t ident) = [AST.Typedef t ident]
        elabGDecl (PT.Sigdef t ident params) = [AST.Sigdef t ident params]
        elabGDecl (PT.FDecl t ident params) = [AST.FDecl t ident params]
        elabGDecl (PT.FExt t ident params) = [AST.FExt t ident params]
        elabGDecl (PT.SDecl ident) = []
        elabGDecl (PT.SDefn ident params) = [AST.SDefn ident params]
        elabGDecl (PT.FDefn t ident params block) = [AST.FDefn t ident params $ elabBlock block]

-- Elaborates a PT.Block into its closest AST equivalent: a list of AST.Stmts.
elabBlock :: PT.Block -> [AST.Stmt]
elabBlock (PT.Stmts stmts) = elabStmts stmts

-- Elaborates a list of PT.Stmts (i.e. from a block) into a list of AST.Stmts.
elabStmts :: [PT.Stmt] -> [AST.Stmt]
elabStmts [] = []
elabStmts ((PT.Ctrl ctrl) : stmts) = elabCtrl ctrl ++ elabStmts stmts
elabStmts ((PT.Block block) : stmts) = elabBlock block ++ elabStmts stmts
elabStmts ((PT.Simp (PT.Decl t ident maybeE)) : stmts) =
    let maybeDef = fmap elabExp maybeE
        scope = elabStmts stmts
    in  [AST.Decl t ident maybeDef scope]
elabStmts ((PT.Simp (PT.Assn asop lValue e)) : stmts) =
  let astLValue = elabLValue lValue
      astE = elabExp e
      rest = elabStmts stmts
  in  [AST.Assn asop astLValue astE] ++ rest
elabStmts ((PT.Simp (PT.Post lValue postop)) : stmts) =
  let astLValue = elabLValue lValue
      asop = Common.SetOp $ case postop of
        PT.PP -> Common.AAdd
        PT.MM -> Common.ASub
      rest = elabStmts stmts
  in  [AST.Assn asop astLValue one] ++ rest
elabStmts ((PT.Simp (PT.Exp e)) : stmts) =
  let rest = elabStmts stmts
  in  [AST.Exp (elabExp e)] ++ rest

-- Elaborates a ParseTree LValue to an AST LValue.
elabLValue :: PT.LValue -> AST.LValue
elabLValue (PT.LIdent ident) = AST.LIdent ident
elabLValue (PT.LStar addr) = AST.LStar $ elabLValue addr
elabLValue (PT.LDot struct field) = AST.LDot (elabLValue struct) field
elabLValue (PT.LArrow addr field) = AST.LDot (AST.LStar $ elabLValue addr) field
elabLValue (PT.LIndex arr index) = AST.LIndex (elabLValue arr) (elabExp index)

-- Elaborates a control structure.
elabCtrl :: PT.Ctrl -> [AST.Stmt]
elabCtrl (PT.If e stmt maybeElseStmt) =
  let thenStmts = elabStmts [stmt]
      elseStmts = case maybeElseStmt of
        Nothing -> []
        Just stmt -> elabStmts [stmt]
  in  [AST.If (elabExp e) thenStmts elseStmts]
elabCtrl (PT.While e stmt) =
  let bodyStmts = elabStmts [stmt]
  in  [AST.While (elabExp e) bodyStmts]
elabCtrl (PT.For maybePre e maybePost ptStmt) =
  let postStmts = case maybePost of
        Nothing -> []
        Just simp ->
          let postStmts = elabStmts [PT.Simp simp]
          in  case head postStmts of
            -- NOTE: This check can't occur in Check.hs, since we've done away with for-loops at
            -- that point.
            AST.Decl _ _ _ _ -> error "Post portion of for-loop can't declare a variable"
            _ -> postStmts
      bodyStmts = elabStmts [ptStmt] ++ postStmts
      while = [AST.While (elabExp e) bodyStmts]
  in  case maybePre of
    Nothing -> while
    Just simp -> case simp of
      PT.Decl t ident maybeExp ->
        let maybeDefine = fmap elabExp maybeExp
        in  [AST.Decl t ident maybeDefine while]
      _ -> elabStmts [PT.Simp simp] ++ while
elabCtrl (PT.Assert e) = [AST.Assert $ elabExp e]
elabCtrl (PT.Return maybeE) = [AST.Return $ fmap elabExp maybeE]

-- Elaborates an expression.
elabExp :: PT.Exp -> AST.Exp
elabExp (PT.IntLit num) = AST.IntLit num
elabExp (PT.BoolLit b) = AST.BoolLit b
elabExp (PT.CharLit c) = AST.CharLit c
elabExp (PT.StringLit s) = AST.StringLit s
elabExp (PT.Ident ident) = AST.Ident ident
-- TODO: do we need the special case for neg?
elabExp (PT.Unop Common.Neg e) =
  case (elabExp e) of
    (AST.IntLit (Common.Dec d)) -> AST.IntLit $ Common.Dec $ -1 * d
    e' -> AST.Binop (AST.ArithOp Common.ASub) zero e'
elabExp (PT.Unop unop e ) = AST.Unop unop $ elabExp e
elabExp (PT.Binop binop e1 e2) = AST.Binop (elabBinop binop) (elabExp e1) (elabExp e2)
elabExp (PT.Cond e1 e2 e3) = AST.Cond (elabExp e1) (elabExp e2) (elabExp e3)
elabExp (PT.Call e args) = AST.Call (elabExp e) $ map elabExp args
elabExp (PT.Alloc t) = AST.Alloc t
elabExp (PT.AllocArray t e) = AST.AllocArray t $ elabExp e
elabExp (PT.Index arr index) = AST.Index (elabExp arr) (elabExp index)
elabExp (PT.Star addr) = AST.Star $ elabExp addr
elabExp (PT.Arrow addr ident) = AST.Dot (AST.Star $ elabExp addr) ident
elabExp (PT.Dot struct ident) = AST.Dot (elabExp struct) ident
elabExp PT.Null = AST.Null
elabExp (PT.Amp ident) = AST.Amp ident
elabExp (PT.Cast t e) = AST.Cast t $ elabExp e

-- Assumes that the input is not LOr or LAnd (which are elaborated seperately).
elabBinop :: PT.Binop -> AST.Binop
elabBinop (PT.ArithOp arithop) = AST.ArithOp arithop
elabBinop (PT.CmpOp cmpop) = AST.CmpOp cmpop
elabBinop (PT.LogOp logop) = AST.LogOp logop

-- Useful constants.
zero = AST.IntLit $ Common.Dec 0
one = AST.IntLit $ Common.Dec 1
allOnes = AST.IntLit $ Common.Hex 0xffffffff
false = AST.BoolLit False
true = AST.BoolLit True
