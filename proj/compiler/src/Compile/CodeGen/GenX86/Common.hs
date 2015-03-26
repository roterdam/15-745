{-
	This file contains types common to the X86-64 generation files.
-}

module Compile.CodeGen.GenX86.Common where

import qualified Compile.Types.Common as Common

import qualified Data.Map as Map
import Data.Int (Int32)

-- List in which a FnName's index is its integer Ident.
type FnList = [Common.FnName]

-- Map from temps to assignments.
type TmpAssignments = Map.Map Common.TmpIdent Assignment

-- State carried by translation.
type TransMaps = (TmpAssignments, TmpAssignments, FnList)

-- Represents a place in the computer to store a temporary (either a register or stack index).
data Assignment = Reg Common.RName | Spill Integer deriving Eq
instance Show Assignment where
  show (Reg rName) = show rName
  show (Spill idx) = "spill (" ++ show idx ++ ")"
instance Ord Assignment where
  compare a1 a2 =
    case (a1, a2) of
      (Reg name1, Reg name2) -> compare name1 name2
      (Spill i1, Spill i2) -> compare i1 i2
      (Reg _, Spill _) -> LT
      (Spill _, Reg _) -> GT
