{-
  This file contains types used to maintain state in the type-checker.
-}

module Compile.Trees.CheckState where

import Compile.Types.AST
import Compile.Trees.Conc
import qualified Compile.Types.Common as Common

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Error (Error(..))

-- Custom error data type.
data CheckErr = CheckErr {
  name :: String,
  object :: String,
  reason :: String }
instance Error CheckErr where
  strMsg s = CheckErr "" "" s
instance Show CheckErr where
  show (CheckErr name object reason) = "Invalid " ++ name ++ ":\n\n" ++ indent object ++ "\n" ++ reason

tab :: String -> String
tab = (++) "\t"
indent :: String -> String
indent = unlines . map tab . lines

-- Monad for propagating CheckErrs.
type CheckMonad = Either CheckErr

-- A set of the idents of functions that are defined at *any* point.
-- NOTE: This set is not built as we go, like the fnSigs field in GlobalState.
type DefinedFns = Set.Set Common.Ident

{-
  A GlobalState contains information about the GDecls encountered so far in the code.
    - typeDefs: Maps idents to concrete types for typedefs. Built as we go in main check phase.
    - structs: Maps struct idents to lists of fields.
    - fnSigs: Maps function idents to function signatures and information about whether these
      functions are defined. Built as we go in the main check phase.
  The global state can be modified by GDecls, but will never be modified from within a function.
-}
data GlobalState = GlobalState {
  typedefs :: Map.Map Common.Ident Conc,
  sigdefs :: Map.Map Common.Ident CSig,
  structs :: Map.Map Common.Ident [CParam],
  fnSigs :: Map.Map Common.Ident (DefState, CSig) }
instance Show GlobalState where
  show (GlobalState typedefs sigdefs structs fnSigs) =
    "GLOBALSTATE:"
    ++ "\n\ttypedefs: " ++ show typedefs
    ++ "\n\tsigdefs: " ++ show sigdefs
    ++ "\n\tstructs: " ++ show structs
    ++ "\n\tfnSigs: " ++ show fnSigs
    ++ "\n"

{-
  A function's declaration definition state. There are three states:
    - Ext: Declared in an external file. Cannot be declared or defined internally,
      but can be used (e.g. called, for functions) internally.
    - IDecl: Declared in the source file, and defined at some later time.
      Cannot be delcared externally, but can be called, redeclared, or defined internally.
    - IDef: Defined (and possibly also pre-declared) in the source file. Cannot be
      declared externally or redefined, but can be redeclared or used internally.
    - INever: Declared in the source file, but defined at no later/earlier time.
      Cannot be defined externally or called, but can be redeclared.
-}
data DefState = EDecl | IDecl | IDef | INever deriving (Eq, Show)

-- Makes a fresh global state.
-- Note that we start with the 'main' function implicitly declared.
newGlobalState :: GlobalState
newGlobalState =
  let fnSigs = Map.singleton "main" (IDecl, (IntC, []))
  in  GlobalState {typedefs = Map.empty, sigdefs = Map.empty, structs = Map.empty, fnSigs = fnSigs}

{-
  A FnState contains info that we need to pass around within a function during type checking.
    - declared: Maps idents of local vars/function parameters to their types.
    - defined: A set of all variables that are defined.
    - foundRet: Has every control path up to the current point ended in a "return"?
  A new FnState is created for each function.
-}
data FnState = FnState {
  declared :: Map.Map Common.Ident Conc,
  defined :: Set.Set Common.Ident,
  foundRet :: Bool }
instance Show FnState where
  show (FnState declared defined foundRet) =
    "FNSTATE:"
    ++ "\n\tdeclared: " ++ show declared
    ++ "\n\tdefined: " ++ show defined
    ++ "\n\tfoundRet: " ++ show foundRet
    ++ "\n"

-- Makes a fresh function state from the function's signature.
newFnState :: [Common.Ident] -> [Conc] -> FnState
newFnState idents concs =
  let declared = Map.fromList $ zip idents concs
      defined = Map.keysSet declared
  in  FnState {declared = declared, defined = defined, foundRet = False}
