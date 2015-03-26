{-
  Common datatypes shared by multiple intermediate languages.
-}

module Compile.Types.Common where

import Data.Char (intToDigit)
import Data.Int (Int32)
import Data.Bits (xor, shiftL, shiftR, (.|.), (.&.))
import Numeric (showIntAtBase)
import System.Info (os)

type TmpIdent = Integer
type BlockIdent = Integer
type FnName = String
type Ident = String
type Tag = Int32

{-
 - An interface representing assembly instructions that can be written out one-per-line to
 - generate a file.
 -}
class (Show a) => Asm a where
  genFile :: [a] -> String
  genFile = concatMap (\i -> (show i) ++ "\n")


-- Shows an integer as a hex string.
toHexStr :: (Integral n, Show n) => n -> String
toHexStr n =
  let sign = if n < 0 then "-" else ""
      -- NOTE: abs not not enough here, because it can return a negative value for some Integral
      -- types.
      magnitude = abs $ toInteger n
  in  sign ++ "0x" ++ showIntAtBase 16 intToDigit magnitude ""

-- Sizes of instructions and registers
-- TODO: Should we have a NONE option?
data Size = Byte | Long | Quad deriving (Eq, Ord)
instance Show Size where
  show Byte  = "b"
  show Long  = "l"
  show Quad  = "q"

-- Gives the number of bytes associated with each size.
bytesForSize :: (Integral n) => Size -> n
bytesForSize Byte = 1
bytesForSize Long = 4
bytesForSize Quad = 8

-- Gives the number of bytes associated with each size, as a string.
showSizeNum :: Size -> String
showSizeNum = show . bytesForSize

{-
  The types of errors the program can throw during execution.
-}
data Error = ArithError | MemError | AssertError
instance Show Error where
  show ArithError = "ArithError"
  show MemError = "MemError"
  show AssertError = "AssertError"

-- Types of numbers for AST and ParseTree.
-- NOTE: This must be an integer instead of an Int32, because a number's actual value (not its value
-- mod 2^32) must be known for the typecheck.
data IntLit = Hex Integer | Dec Integer
instance Show IntLit where
  show (Hex x) = toHexStr x
  show (Dec n) = show n

{-
  An assignment operator.
-}
data Asop = Set | SetOp ArithOp
instance Show Asop where
  show Set = "="
  show (SetOp arithop) = "=" ++ show arithop

{-
  Binary comparison operators.
-}
data CmpOp = CmpOp Cmp deriving (Eq, Ord)
instance Show CmpOp where
  show (CmpOp L) = "<"
  show (CmpOp LE) = "<="
  show (CmpOp G) = ">"
  show (CmpOp GE) = ">="
  show (CmpOp E) = "=="
  show (CmpOp NE) = "!="

{-
  Binary arithmetic and bitwise operators.
-}
data ArithOp = AAdd | ASub | AMul | ADiv | AMod | AAnd | AOr | AXor | AShL | AShR
               deriving (Eq, Ord)
instance Show ArithOp where
  show AAdd = "+"
  show ASub = "-"
  show AMul = "*"
  show ADiv = "/"
  show AMod = "%"
  show AAnd = "&"
  show AOr = "|"
  show AXor = "^"
  show AShL = "<<"
  show AShR = ">>"

{-
  Applies the given arithmetic operation to the two numbers (used in constant folding).
-}
applyArithOp :: ArithOp -> Int32 -> Int32 -> Int32
applyArithOp AAdd n1 n2 = n1 + n2
applyArithOp ASub n1 n2 = n1 - n2
applyArithOp AMul n1 n2 = n1 * n2
applyArithOp ADiv n1 n2 = n1 `quot` n2
applyArithOp AMod n1 n2 = n1 `rem` n2
applyArithOp AAnd n1 n2 = n1 .&. n2
applyArithOp AOr  n1 n2 = n1 .|. n2
applyArithOp AXor n1 n2 = n1 `xor` n2
applyArithOp AShL n1 n2 = n1 `shiftL` (fromIntegral n2)
applyArithOp AShR n1 n2 = n1 `shiftR` (fromIntegral n2)

{-
  Binary logical operators.
-}
data LogOp = LAnd | LOr deriving Eq
instance Show LogOp where
  show LAnd = "&&"
  show LOr = "||"

{-
  Unary operators.
-}
data Unop = Bang | Inv | Neg deriving Eq
instance Show Unop where
  show Bang = "!"
  show Inv = "~"
  show Neg = "-"

{-
  Represents the different types of comparisons supported.
-}
data Cmp = L | LE | E | NE | GE | G deriving (Eq, Ord)
instance Show Cmp where
  show L = "l"
  show LE = "le"
  show E = "e"
  show NE = "ne"
  show GE = "ge"
  show G = "g"

-- A Type.
data Type = DefT Ident | StructT Ident | FnT Ident | ArrT Type | PtrT Type
          | IntT | BoolT | VoidT | CharT | StringT deriving Eq
instance Show Type where
  show IntT = "int"
  show BoolT = "bool"
  show VoidT = "void"
  show CharT = "char"
  show StringT = "string"
  show (DefT ident) = ident
  show (StructT ident) = "struct " ++ ident
  show (FnT ident) = ident
  show (PtrT t) = show t ++ "*"
  show (ArrT t) = show t ++ "[]"


-- A (type, ident) pair.
data Param = Param {
  paramT :: Type,
  paramIdent :: Ident }
instance Show Param where
  show (Param t ident) = show t ++ " " ++ ident

-- Given a cmp c, gets c' such that for all a, b: c a b == not (c' a b).
flipCmp :: Cmp -> Cmp
flipCmp L = G
flipCmp LE = GE
flipCmp E = E
flipCmp NE = NE
flipCmp GE = LE
flipCmp G = L

isEqCmp :: Cmp -> Bool
isEqCmp E = True
isEqCmp NE = True
isEqCmp _ = False

applyCmp :: Cmp -> Int32 -> Int32 -> Bool
applyCmp L x y = x < y
applyCmp LE x y = x <= y
applyCmp E x y = x == y
applyCmp NE x y = x /= y
applyCmp GE x y = x >= y
applyCmp G x y = x > y


-- All registers (not using the 1-byte registers)
data Reg = R { size :: Size,
               name :: RName }
instance Show Reg where
  show (R size name)
    | isClassic name && size == Byte = "%" ++ getOneByteClassicName name
    | isClassic name = "%" ++ prefixForSize size ++ show name
    | otherwise = "%" ++ show name ++ suffixForSize size
    where prefixForSize Long = "e"
          prefixForSize Quad = "r"
          suffixForSize Byte = "b"
          suffixForSize Long = "d"
          suffixForSize Quad = ""
          getOneByteClassicName AX = "al"
          getOneByteClassicName BX = "bl"
          getOneByteClassicName CX = "cl"
          getOneByteClassicName DX = "dl"
          getOneByteClassicName name = (show name) ++ "l"

-- The 16 underlying registers that all x86-64 registers the compiler uses are made of
data RName = AX | BX | CX  | DX  | BP  | SP  | SI  | DI   -- "classic"
           | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15  -- "new"
           deriving (Bounded, Enum, Ord, Eq) -- TODO: do we need all of these?
instance Show RName where
  show AX = "ax"
  show BX = "bx"
  show CX = "cx"
  show DX = "dx"
  show BP = "bp"
  show SP = "sp"
  show SI = "si"
  show DI = "di"
  show R8 = "r8"
  show R9 = "r9"
  show R10 = "r10"
  show R11 = "r11"
  show R12 = "r12"
  show R13 = "r13"
  show R14 = "r14"
  show R15 = "r15"

-- An ordered list of all register names.
rNames :: [RName]
rNames = [(minBound :: RName) .. (maxBound :: RName)]

-- Returns true iff the given register name is NOT new to x86-64.
isClassic :: RName -> Bool
isClassic AX = True
isClassic BX = True
isClassic CX = True
isClassic DX = True
isClassic BP = True
isClassic SP = True
isClassic SI = True
isClassic DI = True
isClassic _  = False

-- Convenience constructions for the most common registers
eax :: Reg
eax = R Long AX
ebx = R Long BX
ecx = R Long CX
edx = R Long DX
edi = R Long DI

rax = R Quad AX
rbx = R Quad BX
rcx = R Quad CX
rdx = R Quad DX
rbp = R Quad BP
rsp = R Quad SP
rdi = R Quad DI
rsi = R Quad SI
r12 = R Quad R12
r13 = R Quad R13
r14 = R Quad R14
r15 = R Quad R15

-- Updates the register to have the given size.
adjustSize :: Reg -> Size -> Reg
adjustSize (R _ name) sz = R sz name

callerSaveRegs :: [RName]
callerSaveRegs = [ AX, DI, SI, DX, CX, R8, R9 ]

calleeSaveRegs :: [RName]
calleeSaveRegs = [ BX, BP, R12, R13, R14, R15 ]

-- Used to hold spilled dsts while we perform operations that might affect them.
spillReg :: RName
spillReg = R10

-- Used for other operations which require an extra register and may interfere with spillReg.
scratchReg :: RName
scratchReg = R11

-- TOOD: Will we have to call this on external functions as well? Or calls to them?
wrapFnName :: String -> String
wrapFnName = (++) (if os == "darwin" then "_" else "")

-- Converts a boolean value to the representative Int32 value.
boolToInt32 :: Bool -> Int32
boolToInt32 b = if b then 1 else 0

-- Converts an int32 value to the boolean value it represents.
int32ToBool :: Int32 -> Bool
int32ToBool 0 = False
int32ToBool 1 = True
int32ToBool _ = error "Cannot convert integers other than 0 and 1 to booleans"
