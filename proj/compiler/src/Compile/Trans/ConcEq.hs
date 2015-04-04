{-
    This file contains equality helper functions for concrete types (Concs).
-}
module Compile.Trans.ConcEq where

import Compile.Trans.Conc
import Compile.Trans.CheckState (GlobalState, sigdefs)

import qualified Data.Map as Map

-- Determines whether two concs are equal.
cEq :: GlobalState -> Conc -> Conc -> Bool
cEq gs c1 c2 =
  case (c1, c2) of
    (PtrC p1, PtrC p2) -> pEq gs p1 p2
    (ArrC c1, ArrC c2) -> cEq gs c1 c2
    (StructC i1, StructC i2) -> i1 == i2
    (FnC i1, FnC i2) -> i1 == i2
    (VoidC, VoidC) -> True
    (BoolC, BoolC) -> True
    (IntC, IntC) -> True
    (CharC, CharC) -> True
    (StringC, StringC) -> True
    _ -> False

-- Checks whether two point targets have compatible type.
-- NOTE: Any's are equal to anything, and AnyFns are equal to sigdef'd types with
-- compatible sigs.
pEq :: GlobalState -> Pointed -> Pointed -> Bool
pEq gs p1 p2 =
  case (p1, p2) of
    (Any, _) -> True
    (_, Any) -> True
    (AnyFn sig1, One (FnC ident2)) ->
      case Map.lookup ident2 $ sigdefs gs of
        Nothing -> False
        Just sig2 -> cSigEq gs sig1 sig2
    (One (FnC ident1), AnyFn sig2) ->
      case Map.lookup ident1 $ sigdefs gs of
        Nothing -> False
        Just sig1 -> cSigEq gs sig1 sig2
    (AnyFn sig1, AnyFn sig2) -> cSigEq gs sig1 sig2
    (One c1, One c2) -> cEq gs c1 c2

-- Determines whether two concs are not equal.
cNeq :: GlobalState -> Conc -> Conc -> Bool
cNeq gs c1 c2 = not $ cEq gs c1 c2

-- Determines whether two concrete sigs are equal.
cSigEq :: GlobalState -> CSig -> CSig -> Bool
cSigEq gs (c1, c1s) (c2, c2s) =
    let retEq = cEq gs c1 c2
        arityEq = length c1s == length c2s
        argsEq = and (zipWith (cEq gs) c1s c2s)
    in  retEq && arityEq && argsEq

-- Determines whether two concrete sigs are not equal.
cSigNeq :: GlobalState -> CSig -> CSig -> Bool
cSigNeq gs sig1 sig2 = not $ cSigEq gs sig1 sig2
