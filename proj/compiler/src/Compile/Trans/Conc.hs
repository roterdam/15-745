{-
  This file defines the ConcT type for concrete types (vs. types that contain typedef'd types).
-}

module Compile.Trans.Conc where

import qualified Compile.Types.Common as Common

import Data.List (intercalate)

-- A concrete param (i.e. a name/conc pair).
data CParam = CParam {
  cParamT :: Conc,
  cParamIdent :: Common.Ident }

-- A concrete function signature.
type CSig = (Conc, [Conc])

-- A concrete type.
-- NOTE: This can only construct valid types.
-- TODO: actually, the NOTE isn't true...ideally, it would be.
-- But currently, we can construct void[], which isn't valid.
data Conc = ArrC Conc | PtrC Pointed | StructC Common.Ident | FnC Common.Ident
          | VoidC | BoolC | IntC | CharC | StringC deriving (Eq, Ord)

-- Concs that we can have a pointer to, as well as some intermediate states needed by the check.
-- NOTE: Any refers to the type of the value referenced by a NULL pointer,
-- and AnyFn refers to a function type that has not yet been associated with a function typedef.
data Pointed = Any | AnyFn CSig | One Conc deriving (Eq, Ord)

instance Show CParam where
  show (CParam conc ident) = show conc ++ " " ++ ident

instance Show Conc where
  show (ArrC conc) = show conc ++ "[]"
  show (PtrC pointed) = show pointed ++ "*"
  show (StructC ident) = "struct " ++ ident
  show (FnC ident) = "fn " ++ ident
  show VoidC = "void"
  show StringC = "string"
  show CharC = "char"
  show IntC = "int"
  show BoolC  = "bool"

instance Show Pointed where
  show Any = "ANY"
  show (AnyFn (retC, cParams)) = "(" ++ show cParams ++ " --> " ++ show retC ++ ")"
  show (One c) = show c
