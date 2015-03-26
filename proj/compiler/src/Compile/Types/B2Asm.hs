{-
  The datatype for 2-argument assembly: lower-level than 3-argument assembly in that the
  operations are simpler and sometimes have state, and there are special/reserved temps.
-}

module Compile.Types.B2Asm where

import Compile.Types.Common (TmpIdent, BlockIdent, ArithOp, Cmp, FnName, Size, showSizeNum,
                             Error)

import Data.Int (Int32)
import Data.Word (Word8)
import Data.List (intercalate)
import qualified Data.Map as Map

type BlockMap = Map.Map BlockIdent Block
type ElemSize = Int32
type Offset = Int32

{-
  Represents a file of abstract 2-argument assembly, where a file is a collection of functions,
  and each function is a collection of blocks.
-}
data B2Asm = Prog [Fn]

{-
  Represents a function in 2-argument assembly, where a function has a name, a list of input
  parameters, and a collection of blocks containing its code.
-}
data Fn = Fn FnName BlockMap

data Block = BBlock [SIns] JIns

{-
  Represents a single straight-line instruction: either 'x <op>= b' or 'x <- c'.
  TODO: tighten the restrictions on operand types, like in XAsm
-}
data SIns = Asop             Size ArithOp Src Dst
          | Set              Size Src Dst
          | ToReserved       Size Src RTmp
          | FromReserved     Size RTmp Dst
          | Load             Size Mem Dst
          | Store            Size Src Mem
          | Cmp              Size Src Dst
          | Test             Size Src Dst
          | CSet             Size Cmp Src Dst
          | Memcpy           [Word8] Src
          | Call             FnName
          | CallPtr          Src
          | CheckNonZero     Size Src Error
          | CheckNonNeg      Size Src Error
          | CheckEqual       Size Src Src Error
          | CheckDivOverflow Src Src
          | CheckShiftMax    Src
          | CheckArrEnd      Src Src

{-
  Represents a nonlocal instruction (i.e. one that ends a basic block).
-}
data JIns = Ret
          | Jmp   BlockIdent
          | Jmpcc Cmp BlockIdent BlockIdent
          | Raise Error

{-
  A valid input for an operation (either a constant, temp variable,
  or pointer generated from a function ident).
-}
data Src = STmp TmpIdent | Imm Int32 | FnPtr FnName

{-
  A valid output for an operation (only a temp variable)
-}
data Dst = DTmp TmpIdent

{-
  A Reserved temporary (res0, arg1, arg2, ..., argn).
-}
data RTmp = RTmp TmpIdent

{-
  Types of heap memory.
-}
data Mem = ArrMem ElemSize Src Src Offset | PtrMem Src Offset


{- Convenience functions -}

res0 :: RTmp
res0 = RTmp 0
argI :: Integer -> RTmp
argI n = if (n <= 0) then error ("argument registers start at 1, got " ++ show n) else RTmp n


{- Implementations of Show for everything -}

instance Show B2Asm where
  show (Prog fns) = joinWith "\n" $ map show fns

{- Joins a list of strings, sticking the given string in between each pair -}
joinWith :: String -> [String] -> String
joinWith = intercalate
join :: [String] -> String
join = joinWith ""

instance Show Fn where
  show (Fn name bMap) =
    let header = name ++ ":\n"
    in header ++ showBlocks bMap ++ "\n"
    where showBlocks m =
            if (Map.size m == 0)
            then "  <pass>"
            else concatMap showPair $ Map.assocs m
          showPair (k, bl) = showLabel k ++ ":\n" ++ blockToStr bl ++ "\n"
          blockToStr (BBlock ss j) =
            concatMap (\s -> "  " ++ show s ++ "\n") ss ++ "  " ++ show j

instance Show SIns where
  show (Asop sz op src dest) =
    join [show dest, "#", showSizeNum sz, " ", show op, "= ", show src]
  show (Set sz src dest) =
    join [show dest, "#", showSizeNum sz, " = ", show src]
  show (ToReserved sz src dest) =
    join [show dest, "#", showSizeNum sz, " = ", show src]
  show (FromReserved sz src dest) =
    join [show dest, "#", showSizeNum sz, " = ", show src]
  show (Load sz mem dest) =
    join [show dest, "#", showSizeNum sz, " = ", show mem]
  show (Store sz src mem) =
    join [show mem, "#", showSizeNum sz, " = ", show src]
  show (Cmp sz l r) =
    join ["cmp#", showSizeNum sz, " ", show l, " ", show r]
  show (Test sz l r) =
    join ["testEq#", showSizeNum sz, " ", show l, " ", show r]
  show (CSet sz cmp src dest) =
    join [show dest, "#", showSizeNum sz, " =(", show cmp, ") ", show src]
  show (Memcpy bytes src) =
    join ["(*", show src, ") = ", show bytes]
  show (Call fn) =
    join ["call ", fn]
  show (CallPtr fnPtrSrc) =
    join ["call (*", show fnPtrSrc, ")"]
  show (CheckNonZero sz src err) =
    join ["checkNonZero(", show src, "#", showSizeNum sz, ", ", show err, ")"]
  show (CheckNonNeg sz src err) =
    join ["checkNonNeg(", show src, "#", showSizeNum sz, ", ", show err, ")"]
  show (CheckEqual sz src1 src2 err) =
    join ["checkEqual(", show src1, "#", showSizeNum sz, ", ", show src2, ", ", show err, ")"]
  show (CheckDivOverflow src1 src2) =
    join ["checkDivOverflow(", show src1, ", ", show src2, ")"]
  show (CheckShiftMax src) =
    join ["checkShiftMax(", show src, ")"]
  show (CheckArrEnd src1 src2) =
    join ["checkArrEnd(", show src1, ", ", show src2, ")"]

instance Show JIns where
  show Ret =
    "ret"
  show (Jmp bl) =
    join ["goto ", showLabel bl]
  show (Jmpcc cmp bl br) =
    join ["goto(", show cmp, ") ", showLabel bl, " else ", showLabel br]
  show (Raise err) =
    join ["raise(", show err, ")"]

instance Show Src where
  show (STmp t) = showTmp t
  show (Imm x) = show x
  show (FnPtr fName) = join ["&", fName]

instance Show Dst where
  show (DTmp t) = showTmp t

instance Show RTmp where
  show (RTmp r) = if (r == 0) then "res0" else "arg" ++ show r

instance Show Mem where
  show (ArrMem elemSz addrSrc indSrc o) = join [show addrSrc, "[", show indSrc, "] + ", show o]
  show (PtrMem addrSrc o) = join ["*(", show addrSrc, " + ", show o, ")"]

showTmp t = "T_" ++ show t
showLabel b = "L" ++ show b
