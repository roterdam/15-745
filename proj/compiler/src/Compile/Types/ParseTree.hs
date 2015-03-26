{-
  ParseTree is, as the name states, almost a Parse Tree. It isn't quite a parse tree, since it
  does away with some lexical tokens like parentheses. But, like a parse tree, it contains
  every semantic construct that is in the language with little simplification.

  ParseTree closely follows the L3 Grammar given on page 2 of the handout.
-}

{-
  TODO:
  * Is this the best way to represent function types? With functions not as normal types, but only defined types?
    * Pro:
      * Fn types are special, since we'll never have to actually display a funciton type
      * This keeps with the idea that function types, like struct types, should be "nomially equal".
        They should be defined by their names, not by their contents.
    * Con:
      * When we get to Concs, we'll want some values to have a concrete type of a signature...maybe?

    For now, we *will* use this method, and even go a little further: differentiating between
    types defined with a normal typedef and a fn typedef. This is because the later acts much
    more like a struct def than a normal typedef -- like a struct def, a fn typdef is an endpoint.
    If we get here, we're guaranteed we can determine exactly which type something is with one
    lookup (what signature, actually).
-}

module Compile.Types.ParseTree where

import qualified Compile.Types.Common as Common
import Data.List (intercalate)

-- Helper functions.
maybeShow :: (Show a) => Maybe a -> String
maybeShow m =
  case m of
    Nothing -> ""
    Just x -> show x
tab :: String -> String
tab = (++) "\t"
indent :: String -> String
indent = unlines . map tab . lines
showTuple :: (Show a) => [a] -> String
showTuple xs = "(" ++ (intercalate ", " $ map show xs) ++ ")"

-- A Parse Tree.
data ParseTree = Prog [GDecl]

-- A declaration in the program's global scope.
-- TODO: Is it ideal to have ExDecl here?
-- TODO: it's a little weird to use Params in SDefn
data GDecl = Typedef Common.Type Common.Ident | Sigdef Common.Type Common.Ident [Common.Param]
           | FDecl Common.Type Common.Ident [Common.Param] | FExt Common.Type Common.Ident [Common.Param]
           | FDefn Common.Type Common.Ident [Common.Param] Block
           | SDecl Common.Ident | SDefn Common.Ident [Common.Param]

-- A brace-delimited block.
data Block = Stmts [Stmt]

-- A statement.
data Stmt = Simp Simp | Ctrl Ctrl | Block Block

-- A simple statement (i.e. a statement which doesn't contain another block or control structure).
-- Note that Decl optionally takes a value to assign to the identifier.
data Simp = Assn Common.Asop LValue Exp | Post LValue Postop | Decl Common.Type Common.Ident (Maybe Exp) | Exp Exp

-- A control structure.
-- NOTE: that If optionally takes an else statement.
data Ctrl = If Exp Stmt (Maybe Stmt) | While Exp Stmt | For (Maybe Simp) Exp (Maybe Simp) Stmt
          | Assert Exp | Return (Maybe Exp)

-- An expression.
-- NOTE: Haskell Strings differ from C0 strings in that Haskell Strings can contain null
-- characters. However, that's what we want at this point (null characters will causes check errors
-- later).
data Exp = IntLit Common.IntLit | BoolLit Bool | CharLit Char | StringLit String | Ident Common.Ident
         | Unop Unop Exp | Binop Binop Exp Exp | Cond Exp Exp Exp | Call Exp [Exp]
         | Alloc Common.Type | AllocArray Common.Type Exp | Index Exp Exp | Star Exp | Arrow Exp Common.Ident
         | Dot Exp Common.Ident | Null | Amp Common.Ident | Cast Common.Type Exp

-- A value that can be assigned into (i.e. can go on the left side of an assignement operation).
data LValue = LIdent Common.Ident | LDot LValue Common.Ident | LArrow LValue Common.Ident | LStar LValue
            | LIndex LValue Exp

-- A binary operator.
-- TODO: move this out into common
data Binop = CmpOp Common.CmpOp | LogOp Common.LogOp | ArithOp Common.ArithOp

-- A unary operator.
type Unop = Common.Unop

-- A postfix operator.
data Postop = PP | MM

instance Show ParseTree where
  show (Prog gdecls) = intercalate "\n\n" $ map show gdecls

instance Show GDecl where
  show (Typedef t ident) = "typedef " ++ show t ++ " " ++ ident ++ ";"
  show (FDecl t ident params) = show t ++ " " ++ ident ++ showTuple params ++ ";"
  show (FExt t ident params) = show t ++ " " ++ ident ++ showTuple params ++ ";"
  show (FDefn t ident params block) = show t ++ " " ++ ident ++ showTuple params ++ " " ++ show block
  show (SDecl ident) = "struct " ++ ident ++ ";"
  show (SDefn ident fields) = "struct " ++ ident ++ " {\n" ++ concatMap (indent . (++ ";") . show) fields ++ "};"
  show (Sigdef t ident params) = "typedef" ++ show t ++ " " ++ ident ++ showTuple params ++ ";"

instance Show Block where
  show (Stmts stmts) = "{\n" ++ concatMap (indent . show) stmts ++ "}"

instance Show Stmt where
  show (Simp simp) = show simp ++ ";"
  show (Ctrl ctrl) = show ctrl
  show (Block block) = show block

instance Show Simp where
  show (Assn asop lValue e) = show lValue ++ " " ++ show asop ++ " " ++ show e
  show (Post lValue postop) = show lValue ++ show postop
  show (Decl t ident maybeExp) = show t ++ " " ++ ident ++
    (case maybeExp of
      Nothing -> ""
      Just e -> " = " ++ show e)
  show (Exp e) = show e

instance Show Ctrl where
  show (If e stmt maybeStmt) = "if (" ++ show e ++ ") " ++ show stmt ++
    (case maybeStmt of
      Nothing -> ""
      Just stmt -> "\nelse " ++ show stmt)
  show (While e stmt) = "while (" ++ show e ++ ") " ++ show stmt
  show (For maybePre e maybePost stmt) = "for (" ++ maybeShow maybePre ++ "; " ++ show e ++ ";" ++
    maybeShow maybePost ++ ") " ++ show stmt
  show (Assert e) = "assert (" ++ show e ++ ");"
  show (Return maybeE) = "return " ++ maybe "" show maybeE ++ ";"

instance Show Exp where
  show (IntLit n) = show n
  show (BoolLit bool) = if bool then "true" else "false"
  show (CharLit c) = show c
  show (StringLit s) = show s
  show (Ident ident) = ident
  show (Unop unop e) = show unop ++ show e
  show (Binop binop e1 e2) = "(" ++ show e1 ++ " " ++ show binop ++ " " ++ show e2 ++ ")"
  show (Cond e1 e2 e3) = "(" ++ show e1 ++ " ? " ++ show e2 ++ " : " ++ show e3 ++ ")"
  show (Call e args) = show e ++ showTuple args
  show (Alloc t) = "alloc(" ++ show t ++ ")"
  show (AllocArray t e) = "alloc_array(" ++ show t ++ ", " ++ show e ++ ")"
  show (Index addr index) = show addr ++ "[" ++ show index ++ "]"
  show (Star addr) = "(*" ++ show addr ++ ")"
  show (Arrow addr field) = show addr ++ "->" ++ field
  show (Dot addr field) = show addr ++ "." ++ field
  show Null = "NULL"
  show (Amp ident) = "&" ++ ident
  show (Cast t e) = "(" ++ show t ++ ") " ++ show e

instance Show LValue where
  show (LIdent ident) = ident
  show (LDot struct field) = show struct ++ "." ++ field
  show (LArrow addr field) = show addr ++ "->" ++ field
  show (LStar addr) = "(*" ++ show addr ++ ")"
  show (LIndex arr index) = show arr ++ "[" ++ show index ++ "]"

instance Show Binop where
  show (CmpOp cmpop) = show cmpop
  show (ArithOp arithop) = show arithop
  show (LogOp logop) = show logop

instance Show Postop where
  show PP = "++"
  show MM = "--"
