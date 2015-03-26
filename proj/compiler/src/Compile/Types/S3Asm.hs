{-
  The datatype for blocked 3-argument abstract assembly, once it's been converted to SSA form.
  Note that it's very similar to B3Asm, only the blocks are also parameterized with input
  arguments.
-}

module Compile.Types.S3Asm where

import Compile.Types.Common (TmpIdent, BlockIdent, FnName, ArithOp, CmpOp, Cmp, Size,
                             showSizeNum, Error)

import Data.List (intercalate)
import Data.Int (Int32)
import Data.Word (Word8)
import qualified Data.Map as Map

type BlockMap = Map.Map BlockIdent Block
type ElemSize = Int32
type Offset = Int32

{-
  Represents a program in ssa 3-asm assembly as a collection of functions.
-}
data S3Asm = Prog [Fn]

{-
  Represents a function in ssa 3-asm form. A function consists of a name, list of parameters,
  and collection of code blocks.
-}
data Fn = Fn FnName [(TmpIdent, Size)] BlockMap

{-
  Represents a single basic block of code. A block has a list of input paremeters, a list of
  straight-line instructions, and one jump instruction (ret, goto, conditional goto).
-}
data Block = BBlock [(TmpIdent, Size)] [SIns] JIns

{-
  Represents a single straight-line instruction.
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
data Binop = ArithOp ArithOp | CmpOp CmpOp deriving (Eq, Ord)

{-
  Represents a nonlocal instruction (i.e. one that ends a basic block).
-}
data JIns = Ret    Size Src
          | Jmp    Label
          | CmpJmp Size Cmp Src Src Label Label
          | Raise  Error

{-
  Represents a parameterized label (a block ID and list of args to pass it).
-}
data Label = Label BlockIdent [(Src, Size)]

{-
  A valid input for an operation (either a constant or temp variable)
-}
data Src = STmp TmpIdent | Imm Int32 | FnPtr FnName deriving Eq

{-
  A valid output for an operation (only a temp variable)
-}
data Dst = DTmp TmpIdent

{-
  Types of heap memory.
-}
data Mem = ArrMem ElemSize Src Src Offset | PtrMem Src Offset deriving (Ord, Eq)


{- Implementations of Ord, where needed -}
instance Ord Src where
  compare s1 s2 =
    case (s1, s2) of
      (Imm x, Imm y) -> compare x y
      (Imm _, _) -> LT
      (STmp _, Imm _) -> GT
      (STmp t1, STmp t2) -> compare t1 t2
      (STmp _, FnPtr _) -> LT
      (FnPtr name1, FnPtr name2) -> compare name1 name2
      (FnPtr _, _) -> GT


{- Implementations of Show for everything (grouped at the end for clarity) -}
instance Show S3Asm where
  show (Prog fns) = joinWith "\n\n" $ map show fns

instance Show Fn where
  show (Fn name params m) =
    let header = name ++ "(" ++ (joinWith ", " $ map showParam params) ++ "):\n"
    in header ++ (concatMap (\(k, bl) -> blockToStr k bl ++ "\n") $ Map.assocs m)

instance Show Block where
  show block = blockToStr "<block>" block

blockToStr k (BBlock params ss j) =
  join ["L", show k, "(", joinWith ", " $ map showParam params, "):\n",
        concatMap (\s -> join ["  ", show s, "\n"]) ss, "  ", show j]

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
    join [show dest, "#", showSizeNum sz, " = ", name, "(", joinWith ", " (map showArg args),
          ")"]
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
    join ["goto ", show bl]
  show (CmpJmp sz c l r bl br) =
    join ["goto ", show bl, " if (", show l, " ", show c, "#", showSizeNum sz, " ", show r,
          ") else ", show br]
  show (Raise err) =
    join ["raise(", show err, ")"]

instance Show Label where
  show (Label b args) = join ["L", show b, "(", joinWith ", " $ map showArg args, ")"]

instance Show Src where
  show (STmp t) = showTmp t
  show (Imm x) = show x
  show (FnPtr fName) = join ["&", fName]

instance Show Dst where
  show (DTmp t) = showTmp t

instance Show Mem where
  show (ArrMem elemSz addrSrc indSrc o) = join [show addrSrc, "[", show indSrc, "] + ", show o]
  show (PtrMem addrSrc o) = join ["*(", show addrSrc, " + ", show o, ")"]

joinWith :: String -> [String] -> String
joinWith = intercalate
join :: [String] -> String
join = joinWith ""

showParam :: (TmpIdent, Size) -> String
showParam (t, sz) = join [showTmp t, "#", showSizeNum sz]
showArg :: (Src, Size) -> String
showArg (src, sz) = join [show src, "#", showSizeNum sz]

showTmp :: TmpIdent -> String
showTmp t = join ["T_", show t]
