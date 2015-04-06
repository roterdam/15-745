{-
  Common datatypes shared by multiple intermediate languages.
-}

module Compile.Types.Common where

import Data.Char (intToDigit)
import Data.Int (Int32)
import Data.Bits (xor, shiftL, shiftR, (.|.), (.&.))
import Numeric (showIntAtBase)
import System.Info (os)

type Ident = String



-- Shows an integer as a hex string.
toHexStr :: (Integral n, Show n) => n -> String
toHexStr n =
  let sign = if n < 0 then "-" else ""
      -- NOTE: abs not not enough here, because it can return a negative value for some Integral
      -- types.
      magnitude = abs $ toInteger n
  in  sign ++ "0x" ++ showIntAtBase 16 intToDigit magnitude ""


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
  show (SetOp arithop) = show arithop ++ "="

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
  paramIdent :: Ident
}
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
