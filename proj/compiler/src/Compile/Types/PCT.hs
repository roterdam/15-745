{-
  A simplified version of the AST, with all the information that was only retailed for
  typechecing stripped away. The acronym stands for post-check tree.
-}

module Compile.Types.PCT where

import Compile.Types.Common (Asop, Unop, ArithOp, CmpOp, LogOp, Size, showSizeNum, Tag)

import Data.Int (Int32, Int8)
import Data.List (intercalate)

-- TODO: Now that the typecheck is done, turn Cast into Tag and Untag

-- Is it ok for these to be 32 bits? Hmm...
type ElemSize = Int32
type Offset = Int32

{-
  Represents a pct program as just a list of functions.
-}
data PCT = Prog [Fn]

{-
  Represents a pct function as a name, list of argument names, and list of body statements.
-}
data Fn = Fn Ident [(Ident, Size)] [Stmt]

{-
  Represents a pct statement.
  TODO: remove decls, if at all possible.
-}
data Stmt = Assn Asop LValue Exp | If Exp [Stmt] [Stmt] | While Exp [Stmt]
          | Return Exp | Decl Ident Size [Stmt] | Assert Exp | Exp Exp

{-
  Represents a pct expression. Note that the more complicated Dot/Star/Index structure of AST
  memory accesses has been simplified to Star/Index with offsets, and that Alloc/AllocArray
  take sizes instead of types.
  RULE: the size of the result must always be derivable for an expression, given a context
        mapping variable names to their sizes.
-}
data Exp = NumLit Int32 | BoolLit Bool | CharLit Int8 | StringLit String | Null | Ident Ident
         | Binop Binop Exp Exp | Unop Unop Exp | Cond Exp Exp Exp | Tag Exp Tag | Untag Exp Tag
         | CompareTagged CmpOp Exp Exp | Amp Ident | Call Size Ident [Exp]
         | CallPtr Size Exp [Exp] | Alloc ElemSize | AllocArray ElemSize Exp
         | Index Size ElemSize Exp Exp Offset | Star Size Exp Offset deriving Eq

{-
  Represents a binary operator. Arithmetic and comparison operators are separated for
  modularity.
-}
data Binop = ArithOp ArithOp | CmpOp CmpOp | LogOp LogOp deriving Eq

{-
  Represents an lvalue (a destination to which the value of an expression can be written).
  Note that, same as in Exp, the Dot/Star/Index memory model has been flattened into just
  Star/Index, but with memory offsets.
  NOTE: the offset is for LStar is an option because "*p = e" and "p->0 = e" have different
        evaluation semantics.
-}
data LValue = LIdent Ident | LStar LValue (Maybe Offset) | LIndex ElemSize LValue Exp Offset

{-
  Represents an identifier (a function or variable name) as a string.
-}
type Ident = String


{-
  Show implementations grouped at the end for clarity.
-}
instance Show PCT where
  show (Prog fns) = joinWith "\n\n" $ (map show) fns

instance Show Fn where
  show (Fn ident params stmts) =
    join [ident, showTupleWithSizes params, ":\n", showStmts stmts]

instance Show Stmt where
  show (Assn asop lValue e) = join [show lValue, " ", show asop, " ", show e]
  show (If e ifStmts elseStmts) =
    join ["if (", show e, "):\n", showStmts ifStmts, "\nelse:\n", showStmts elseStmts]
  show (While e bodyStmts) =
    join ["while (", show e, "):\n", showStmts bodyStmts]
  show (Return e) = join ["return (", show e, ")"]
  show (Decl ident sz ss) =
    join ["decl ", show ident, "#", showSizeNum sz, ":\n", showStmts ss]
  show (Assert e) = join ["assert (", show e, ")"]
  show (Exp e) = join ["exp (", show e, ")"]

instance Show Exp where
  show (NumLit n) =
    show n
  show (BoolLit b) =
    show b
  show (CharLit n) =
    show n
  show (StringLit s) =
    show s
  show (Null) =
    "NULL"
  show (Ident ident) =
    ident
  show (Binop binop e1 e2) =
    join ["(", show e1, " ", show binop, " ", show e2, ")"]
  show (Unop unop e) =
    join [show unop, show e]
  show (Cond e1 e2 e3) =
    join ["(", show e1, " ? ", show e2, " : ", show e3, ")"]
  show (Tag e tag) =
    join ["tag(", show e, ", ", show tag, ")"]
  show (Untag e tag) =
    join ["untag(", show e, ", ", show tag, ")"]
  show (CompareTagged cmpOp e1 e2) =
    join [show e1, " ", show cmpOp, "t ", show e2]
  show (Amp fName) =
    join ["&", fName]
  show (Call sz ident args) =
    join [show ident, "(", joinWith ", " $ map show args, ")#", showSizeNum sz]
  show (CallPtr sz e args) =
    join ["(*", show e, ")(", joinWith ", " $ map show args, ")#", showSizeNum sz]
  show (Alloc t) =
    join ["alloc(", show t, ")"]
  show (AllocArray t e) =
    join ["alloc_array(", show t, ", ", show e, ")"]
  show (Index sz esz addr index o) =
    join [show addr, "[", show index, "].", show o, "#", showSizeNum sz]
  show (Star sz addr o) =
    join ["(*", show addr, ").", show o, "#", showSizeNum sz]

instance Show LValue where
  show (LIdent ident) = ident
  show (LIndex esz arr index o) = join [show arr, "[", show index, "]", ".", show o]
  show (LStar addr (Just o)) = join ["(*", show addr, ")", ".", show o]
  show (LStar addr Nothing) = join ["(*", show addr, ")"]

instance Show Binop where
  show (CmpOp cmpop) = show cmpop
  show (ArithOp arithop) = show arithop
  show (LogOp logop) = show logop


joinWith :: String -> [String] -> String
joinWith = intercalate
join :: [String] -> String
join = joinWith ""
showTupleWithSizes :: (Show a) => [(a, Size)] -> String
showTupleWithSizes xs = join ["(", joinWith ", " $ map showItemWithSize xs, ")"]
showItemWithSize :: (Show a) => (a, Size) -> String
showItemWithSize (x, sz) = join [show x, "#", showSizeNum sz]
showStmts :: [Stmt] -> String
showStmts stmts = case unlines $ map (("  " ++) . show) stmts of
  "" -> ""
  s -> init s
