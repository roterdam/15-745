{-
  IR Trees are an intermediate step between the AST and abstract assembly, used here to:
    1. Extract out the resolving of variable name ambiguity
    2. Represent expressions with side effects (i.e. THE WORST THING EVER) more cleanly

  An IRT has the following structure invariants:
    - Each control path ends in exactly one return, even if the function is void
    - Exps are guaranteed to have no effect on control flow and to not throw error, provided
      that the statements before them have already been evaluated.
  The purpose of the IR tree is to expand complicated/effectful expressions into simple ones,
  so that converting to 3-ASM is not so big of a step.
-}

{-
  TODO: should we elaborate alloc and alloc_array higher up? It seems like it would be clean to
        do so.
  * However, it would be hard to type-check them, since they take a type name as a parameter,
    which isn't in the type system.
  * In terms of naming, it's safe to elaborate them in IRT to functions with names 'alloc' and
    'alloc_array', since C0 does not allow functions with these names.
-}

module Compile.Types.IRT where

import Compile.Types.Common (TmpIdent, FnName, ArithOp, CmpOp, Size, showSizeNum, Error)

import Data.Int (Int32)
import Data.Word (Word8)
import Data.List (intercalate)
import qualified Data.Map as Map

type FnMap = Map.Map FnName Fn

type ElemSize = Int32
type Offset = Int32

{-
  Represents an IR-Tree as a collection of named functions.
-}
data IRT = Prog [Fn]

{-
  Represents a function as a name, list of parameters, and list of statements.
-}
data Fn = Fn FnName [(TmpIdent, Size)] [Stmt]

{-
  Represents a statement in an IR tree.
  NOTE: Call and MemRead contain a size because it's not derivable from the rest of their
        parameters.
-}
data Stmt = Assn LValue Exp | Memcpy TmpIdent [Word8] | Call Size TmpIdent FnName [Exp]
          | CallPtr Size TmpIdent Term [Exp] | MemRead Size TmpIdent Mem
          | If Exp [Stmt] [Stmt] | While [Stmt] Exp [Stmt] | Return Exp
          | CheckNonZero Term Error | CheckNonNeg Term Error | CheckEqual Term Term Error
          | CheckDivOverflow Term Term | CheckShiftMax Term | CheckArrEnd Term Term
          | Raise Error

{-
  Represents an expression in an IR Tree.
  Expressions are guaranteed to not cause runtime errors, even though they contains ops which
  theoretically could crash, because all failure cases are separated out into CheckFoo
  statements that occur before the expression is evaluated.
  NOTE: the size of the result of evaluating an expression is guaranteed to be derivable at
        compile time, given a map from TmpIdents to their sizes.
  NOTE: integer constants are represented as Int32s, because all possible values for a constant
        (true/false, 32-bit ints, and Null) can be represented in 32 bits, and the only
        constants which can have non-comparison operations applied to them are 32-bit ints.
-}
data Exp = Term Term | Binop Binop Exp Exp
data Term = NumLit Size Int32 | Tmp TmpIdent | FnPtr FnName

{-
  A value that we can assign into.
-}
data LValue = LTmp TmpIdent | LMem Mem

-- Some indirect addressing modes.
data Mem = ArrMem ElemSize Term Term Offset | PtrMem Term Offset

{-
  Represents a binary operation in an IR Tree. Though some Ops can crash, those with might
  crash are guaranteed to have checks ahead of them, so we can safely treat them as having no
  side effects here.
-}
-- TODO: do the common stuff here
data Binop = ArithOp ArithOp | CmpOp CmpOp deriving Eq

instance Show IRT where
  show (Prog fns) = joinWith "\n" $ map show fns

{- Joins a list of strings, sticking the given string in between each pair -}
joinWith :: String -> [String] -> String
joinWith = intercalate
join :: [String] -> String
join = joinWith ""

instance Show Fn where
  show (Fn name tmps stmts) =
    let header = name ++ "(" ++ (joinWith ", " $ map showParam tmps) ++ "):\n"
    in  header ++ (showStmts "  " stmts) ++ (if null stmts then "pass\n" else "\n")

{-
  Shows a list of statements, each indented by prefix and newline-separated.
-}
showStmts :: String -> [Stmt] -> String
showStmts prefix ss = joinWith "\n" $ map (showStmt prefix) ss

{-
  Shows a statement indented to the given level.

  NOTE: the way this renders while loops is kinda weird :(
-}
showStmt :: String -> Stmt -> String
showStmt prefix (Assn l e) =
  join [prefix, show l, " = ", show e]
showStmt prefix (Memcpy t bytes) =
  join [prefix, "M[", showTmp t, "] = ", show bytes]
showStmt prefix (Call sz t name es) =
  join [prefix, showTmp t, "#", showSizeNum sz, " = ", name, "(", joinWith ", " (map show es),
        ")"]
showStmt prefix (CallPtr sz t fPtrTerm es) =
  join [prefix, showTmp t, "#", showSizeNum sz, " = (* ", show fPtrTerm, ")(",
        joinWith ", " (map show es), ")"]
showStmt prefix (MemRead sz t mem) =
  join [prefix, showTmp t, "#", showSizeNum sz, " = ", show mem]
showStmt prefix (If e tss fss) =
  join [prefix, "if (", show e, "):\n", showStmts ("  " ++ prefix) tss, "\n", prefix,
        "else:\n", showStmts ("  " ++ prefix) fss]
showStmt prefix (While css ce bss) =
  join [showStmts prefix css, "\n", prefix, "while (", show ce, "):\n",
        showStmts ("  " ++ prefix) (bss ++ css)]
showStmt prefix (Return e) =
  join [prefix, "return ", show e]
showStmt prefix (CheckNonZero term err) =
  join [prefix, "checkNonZero(", show term, ", ", show err, ")"]
showStmt prefix (CheckNonNeg term err) =
  join [prefix, "checkNonNeg(", show term, ", ", show err, ")"]
showStmt prefix (CheckEqual term1 term2 err) =
  join [prefix, "checkEqual(", show term1, ", ", show term2, ", ", show err, ")"]
showStmt prefix (CheckDivOverflow term1 term2) =
  join [prefix, "checkDivOverflow(", show term1, ", ", show term2, ")"]
showStmt prefix (CheckShiftMax term) =
  join [prefix, "checkShiftMax(", show term, ")"]
showStmt prefix (CheckArrEnd term1 term2) =
  join [prefix, "checkArrEnd(", show term1, ", ", show term2, ")"]
showStmt prefix (Raise err) =
  join [prefix, "raise (", show err, ")"]

instance Show Stmt where
  show s = showStmt "" s

showParam :: (TmpIdent, Size) -> String
showParam (t, sz) = join [showTmp t, "#", showSizeNum sz]

showTmp :: TmpIdent -> String
showTmp t = "T_" ++ show t

instance Show Exp where
  show (Term term) = show term
  show (Binop binop eL eR) =
    join ["(", show eL, " ", show binop, " ", show eR, ")"]

instance Show Binop where
  show (ArithOp arithop) = show arithop
  show (CmpOp cmpop) = show cmpop

instance Show Term where
  show (NumLit sz n) = join [show n, "#", showSizeNum sz]
  show (Tmp t) = showTmp t
  show (FnPtr fName) = join ["&", fName]

instance Show LValue where
  show (LTmp t) = showTmp t
  show (LMem mem) = show mem

instance Show Mem where
  show (ArrMem esz arr index offset) =
    join ["M[", show arr, " + ", show index, " * ", show esz, " + ", show offset, "]"]
  show (PtrMem addr offset) =
    join ["M[", show addr, " + ", show offset, "]"]
