{-
  AST is a more condensed, functional tree representation (compared to ParseTree).
  An AST is easier to navigate than a ParseTree. Therefore, AST will be used
  for type-checking, control-flow checks, and certain optimizations.
-}

module Compile.Types.AST where

import qualified Compile.Types.Common as Common
import Data.List (intercalate)

-- Shows a list of statements, all indented, without a trailing newline.
-- TODO: try this out to make sure it actually works
showStmts :: [Stmt] -> String
showStmts b = case unlines $ map ((++) "\t" . show) b of
  "" -> ""
  s -> init s
showTuple :: (Show a) => [a] -> String
showTuple xs = "(" ++ (intercalate ", " $ map show xs) ++ ")"

-- An abstract syntax tree.
data AST = Prog [GDecl]

-- A declaration in the global scope.
-- TODO: should AST and ParseTree differentiate FnName and TypeName from Common.Ident like other types do?
data GDecl = Typedef Common.Type Common.Ident | Sigdef Common.Type Common.Ident [Common.Param]
           | FDecl Common.Type Common.Ident [Common.Param] | FExt Common.Type Common.Ident [Common.Param]
           | FDefn Common.Type Common.Ident [Common.Param] [Stmt]
           | SDefn Common.Ident [Common.Param]

-- A statement. Note that we separate declaration and assignment for easier typechecking.
data Stmt = Assn Common.Asop LValue Exp | If Exp [Stmt] [Stmt] | While Exp [Stmt]
          | Return (Maybe Exp) | Decl Common.Type Common.Ident (Maybe Exp) [Stmt] | Assert Exp | Exp Exp

-- An expression.
-- Note that we remove unary operators, since we can elaborate them to Binops.
data Exp = IntLit Common.IntLit | BoolLit Bool | CharLit Char | StringLit String
         | Ident Common.Ident | Binop Binop Exp Exp | Unop Unop Exp | Cond Exp Exp Exp | Call Exp [Exp]
         | Alloc Common.Type | AllocArray Common.Type Exp | Index Exp Exp | Star Exp | Dot Exp Common.Ident
         | Amp Common.Ident | Cast Common.Type Exp | Null

-- A Binary operator.
data Binop = ArithOp Common.ArithOp | CmpOp Common.CmpOp | LogOp Common.LogOp

-- A unary operator.
type Unop = Common.Unop

-- A value we can assign into.
data LValue = LIdent Common.Ident | LStar LValue | LDot LValue Common.Ident | LIndex LValue Exp

{-
  Show implementations at the end for clarity
-}
instance Show AST where
  show (Prog gdecls) = intercalate "\n\n" $ (map show) gdecls

instance Show GDecl where
  show (Typedef t ident) = "typedef(" ++ show t ++ ", " ++ ident ++ ")"
  show (Sigdef t ident params) = "sigdef(" ++ show t ++ ", " ++ ident ++ ", params" ++ showTuple params ++ ")"
  show (FDecl t ident params) = "fdecl(" ++ show t ++ ", " ++ ident ++ ", params" ++ showTuple params ++ ")"
  show (FExt t ident params) = "fext(" ++ show t ++ ", " ++ ident ++ ", params" ++ showTuple params ++ ")"
  show (FDefn t ident params stmts) = "fdecl(" ++ show t ++ ", " ++ ident ++ ", params" ++ showTuple params ++ "):\n" ++ showStmts stmts
  show (SDefn ident fields) = "sDef(" ++ ident ++ ", " ++ showTuple fields ++ ")"

{-
  NOTE: this implementation of show doesn't do pretty-printing b/c it can't :(
  The only way to get pretty-printed statements is to show the whole AST
-}
-- TODO: come back and clean this up with some helpers
instance Show Stmt where
  show (Assn asop lValue e) = show lValue ++ " " ++ show asop ++ " " ++ show e
  show (If e beforeStmts elseStmts) = "if (" ++ show e ++ "):\n" ++ showStmts beforeStmts ++ "\nelse:\n" ++ showStmts elseStmts
  show (While e bodyStmts) = "while (" ++ show e ++ "):\n" ++ showStmts bodyStmts
  show (Return maybeE) = "return (" ++ maybe "" show maybeE ++ ")"
  show (Decl t ident maybeE scopeStmts) =
    let defStmts = case maybeE of
          Nothing -> []
          Just e -> [Assn Common.Set (LIdent ident) e]
    in "decl (" ++ show t ++ " " ++ show ident ++ "):\n" ++ showStmts (defStmts ++ scopeStmts)
  show (Assert e) = "assert (" ++ show e ++ ")"
  show (Exp e) = "exp (" ++ show e ++ ")"

instance Show Exp where
  show (IntLit num) = "num(" ++ show num ++ ")"
  show (BoolLit bool) = "bool(" ++ show bool ++ ")"
  show (CharLit c) = show c
  show (StringLit s) = show s
  show (Ident ident) = ident
  show (Binop binop e1 e2) = "(" ++ show e1 ++ " " ++ show binop ++ " " ++ show e2 ++ ")"
  show (Unop unop e) = "(" ++ show unop ++ show e ++ ")"
  show (Cond e1 e2 e3) = "(" ++ show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3 ++ ")"
  show (Call e args) = "call(" ++ show e ++ ", args" ++ showTuple args ++ ")"
  show (Alloc t) = "alloc(" ++ show t ++ ")"
  show (AllocArray t e) = "alloc_array(" ++ show t ++ ", " ++ show e ++ ")"
  show (Index addr index) = show addr ++ "[" ++ show index ++ "]"
  show (Star addr) = "(*" ++ show addr ++ ")"
  show (Dot addr field) = show addr ++ "." ++ field
  show Null = "NULL"
  show (Amp ident) = "(&" ++ ident ++ ")"
  show (Cast t e) = "((" ++ show t ++ ") " ++ show e ++ ")"

instance Show LValue where
  show (LIdent ident) = ident
  show (LIndex arr index) = show arr ++ "[" ++ show index ++ "]"
  show (LStar addr) = "(*" ++ show addr ++ ")"
  show (LDot struct field) = show struct ++ "." ++ field

instance Show Binop where
  show (CmpOp cmpop) = show cmpop
  show (ArithOp arithop) = show arithop
  show (LogOp logop) = show logop
