{-
  The datatype for blocked 3-argument abstract assembly: the assembly code is divided into
  basic blocks, where each block is straight-line code with one entry point and one exit point.
  Valid exit points are:
  1. Return
  2. Goto <bblock>
  3. Goto <bblock> if <x> else <bblock>
  4. Raise <error>

  The overall program (for now a single main function) is implemented as a set of bblocks.
-}

module Compile.Types.B3Asm where

import Compile.Types.Common (TmpIdent, BlockIdent, FnName, ArithOp, CmpOp, Cmp, Size,
                             showSizeNum, Error)

import Data.Int (Int32)
import Data.Word (Word8)
import Data.List (intercalate)
import qualified Data.Map as Map

type BlockMap = Map.Map BlockIdent Block
type ElemSize = Int32
type Offset = Int32

{-
  Represents a file of abstract 3-argument assembly, where a file is a collection of functions,
  and each function is a collection of blocks.
-}
data B3Asm = Prog [Fn]

{-
  Represents a function in 3-argument assembly, where a function has a name, a list of input
  parameters, and a collection of blocks containing its code.
-}
data Fn = Fn FnName [(TmpIdent, Size)] BlockMap

{-
  Represents a single basic block of code. A block has a list of straight-line instructions,
  followed by one jump instruction (ret, goto, conditional goto).
-}
data Block = BBlock [SIns] JIns

{-
  Represents a single straight-line instruction.
  The Binop sizes are the sizes of the srcs and dsts, respectively.
-}
data SIns = Binop            Size Size Binop Src Src Dst
          | Set              Size Src Dst
          | Load             Size Mem Dst
          | Store            Size Src Mem
          | Memcpy           [Word8] Src
          | Call             Size FnName [(Src, Size)] Dst
          | CallPtr          Size Src [(Src, Size)] Dst
          | CheckNonZero     Size Src Error
          | CheckNonNeg      Size Src Error
          | CheckEqual       Size Src Src Error
          | CheckDivOverflow Src Src
          | CheckShiftMax    Src
          | CheckArrEnd      Src Src

{-
  Represents a binary operator.
-}
data Binop = ArithOp ArithOp | CmpOp CmpOp

{-
  Represents a nonlocal instruction (i.e. one that ends a basic block).
-}
data JIns = Ret    Size Src
          | Jmp    BlockIdent
          | CmpJmp Size Cmp Src Src BlockIdent BlockIdent
          | Raise  Error

{-
  A valid input for an operation (either a constant or temp variable)
-}
data Src = STmp TmpIdent | Imm Int32 | FnPtr FnName

{-
  A valid output for an operation (only a temp variable)
-}
data Dst = DTmp TmpIdent

{-
  Types of heap memory.
-}
data Mem = ArrMem ElemSize Src Src Offset | PtrMem Src Offset


{- Implementations of Show for everything (grouped at the end for clarity) -}
instance Show B3Asm where
  show (Prog fns) = joinWith "\n" $ map show fns

{- Joins a list of strings, sticking the given string in between each pair -}
joinWith :: String -> [String] -> String
joinWith = intercalate
join = joinWith ""

instance Show Fn where
  show (Fn name tmps blocks) =
    join [name, "(", joinWith ", " $ map showParam tmps, "):\n", showBlocks blocks, "\n"]
    where showBlocks m =
            if (Map.null m)
            then "<pass>"
            else concatMap (\(k, bl) -> join [showLabel k, ":\n", blockToStr bl, "\n"]) $
                           Map.assocs m
          blockToStr (BBlock ss j) =
            join $ concatMap (\s -> ["  ", show s, "\n"]) ss ++ ["  ", show j]

instance Show SIns where
  show (Binop srcSz dstSz binop l r dest) =
    join [show dest, "#", showSizeNum dstSz, " = ", show l, " ", show binop, "#",
          showSizeNum srcSz, " ", show r]
  show (Set sz src dest) =
    join [show dest, "#", showSizeNum sz, " = ", show src]
  show (Load sz mem dest) =
    join [show dest, "#", showSizeNum sz, " = ", show mem]
  show (Store sz src mem) =
    join [show mem, "#", showSizeNum sz, " = ", show src]
  show (Memcpy bytes src) =
    join ["(*", show src, ") = ", show bytes]
  show (Call sz name args dest) =
    join [show dest, "#", showSizeNum sz, " = ", name, "(", joinWith ", " (map show args), ")"]
  show (CallPtr sz fSrc args dest) =
    join [show dest, "#", showSizeNum sz, " = ", show fSrc, "(", joinWith ", " (map show args),
          ")"]
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

instance Show Binop where
  show (ArithOp arithop) = show arithop
  show (CmpOp cmpop) = show cmpop

instance Show JIns where
  show (Ret sz s) =
    join ["return#", showSizeNum sz, " ", show s]
  show (Jmp bl) =
    join ["goto ", showLabel bl]
  show (CmpJmp sz c l r bl br) =
    join ["goto ", showLabel bl, " if (", show l, " ", show c, "#", showSizeNum sz, " ",
             show r, ") else ", showLabel br]
  show (Raise err) =
    join ["raise(", show err, ")"]

instance Show Src where
  show (STmp t) = showTmp t
  show (Imm x) = show x
  show (FnPtr fName) = join ["&", fName]

instance Show Dst where
  show (DTmp t) = showTmp t

instance Show Mem where
  show (ArrMem elemSz addrSrc indSrc o) =
    join [show addrSrc, "[", show indSrc, "] + ", show o]
  show (PtrMem addrSrc o) =
    join ["*(", show addrSrc, " + ", show o, ")"]

showParam :: (TmpIdent, Size) -> String
showParam (t, sz) = join [showTmp t, "#", showSizeNum sz]

showTmp :: TmpIdent -> String
showTmp t = "T_" ++ (show t)

showLabel :: BlockIdent -> String
showLabel b = "L" ++ show b
