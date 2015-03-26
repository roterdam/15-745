{-
  This file runs a number of static checks on an AST. It checks:
    - Types: All expressions type-check, and all statements/functions/typedefs follow typing rules.
    - Control: Each control path ends in a return in every function, excluding void funtions.
    - Definition: Funtions, variables, and defined types are defined before use.
    - Integers: Integer constants are within bounds.

  The type-checking process involves two steps:
    1. pre-processing:
      - Runs through the file (in reality, we only need to scan the gDecls), building sets of
        information that is important *regardless of its position in the file* (which functions are
        ever defined, etc.).
      - Note that this step can never fail.
    2. main check:
      - Recursively descend through the gDecls in order, maintaining:
        - GlobalState: Information about what constructs we have seen so far. Unlike the
          information built during the pre-processing step, this infromation is position-dependent.
          For instance, what functions have been declared *so far*?
        - FnState: Information about what constructs have been defined/declared in the current
          function. This informatio is position-dependent, and is thrown out whenever we leave the
          function.
-}

module Compile.Trees.Check where

import Compile.Types.AST
import qualified Compile.Types.Common as Common
import Compile.Trees.CheckState
import Compile.Trees.Conc
import Compile.Trees.ConcEq

import Control.Monad.Error (catchError, throwError)
import Control.Monad (foldM)

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import qualified Data.List as List

import Debug.Trace (trace)

{-
  Runs static checks on an AST.

  Runs in two steps:
    1. Pre-process the GDecls to pull out global information relevant to the check.
    2. Recursively descend through the AST, failing if we encounter pieces that do not check.
-}
check :: AST -> CheckMonad GlobalState
check (Prog gDecls) = do
  let definedFns = getDefinedFns gDecls
  checkGDecls definedFns gDecls

{-
  Runs the first step of the type-checking process: the pre-processing step.
  The check cannot fail during the pre-processing step.
  Instead, its job is to build a set of function idents that are defd at any point in the program.
  Note that we start with "main" defined automatically.
-}
getDefinedFns :: [GDecl] -> DefinedFns
getDefinedFns gDecls = Set.fromList $ "main" : (Maybe.mapMaybe identIfFDefn gDecls)
  where identIfFDefn :: GDecl -> Maybe Common.Ident
        identIfFDefn (FDefn _ ident _ _) = Just ident
        identIfFDefn _ = Nothing

{-
  Runs the main step of the type-checking process on all GDecls.
  Unlike the pre-process, this step can fail.
  If it succeeds, it returns the final GlobalState.
-}
checkGDecls :: DefinedFns -> [GDecl] -> CheckMonad GlobalState
checkGDecls definedFns gDecls = foldM checkGDecl newGlobalState gDecls
  where checkGDecl :: GlobalState -> GDecl -> CheckMonad GlobalState
        checkGDecl gs (Typedef t ident) = do
          {-
            A typedef is valid if there are no other types or function signatures so far with this
            ident.
          -}
          let errPrefix = CheckErr "typedef" $ show (Typedef t ident)
          conc <- getConcrete gs t
          throwIf (cEq gs conc VoidC) $ errPrefix "cannot typedef void"
          throwIf (isType gs ident) $ errPrefix "name is already a defined type"
          throwIf (Map.member ident $ fnSigs gs) $ errPrefix "name is already a function name"
          return gs {typedefs = Map.insert ident conc $ typedefs gs}

        checkGDecl gs (Sigdef t ident params) = do
          {-
            A sigdef is valid if there are no other types of function signatures so far with this
            ident.
          -}
          let errPrefix = CheckErr "sigdef" $ show (Sigdef t ident params)
          sigConc <- getConcreteSig gs (t, params)
          throwIf (isType gs ident) $ errPrefix "name is already a defined type"
          return gs {sigdefs = Map.insert ident sigConc $ sigdefs gs}

        checkGDecl gs (FDecl t ident params) = do
          {-
            A function declaration is valid if there are no types so far with this ident,
            any other declarations with this ident have the same signature,
            and this ident is never used in an external declaration.
          -}
          let errPrefix = CheckErr "function declaration" $ show (FDecl t ident params)
          sigConc <- getConcreteSig gs (t, params)
          sigMatches gs ident sigConc
          throwIf (isType gs ident) $ errPrefix "name is already a defined type"
          throwIf (sigHasDefState gs EDecl ident) $ errPrefix "already declared externally"
          let defState' = if sigHasDefState gs IDef ident
                then IDef
                else if Set.member ident definedFns
                  then IDecl
                  else INever
          return gs {fnSigs = Map.insert ident (defState', sigConc) $ fnSigs gs}

        checkGDecl gs (FExt t ident params) = do
          {-
            A external function declaration is valid if there are no types so far with this ident,
            there are not internally declared/defined functions with this ident,
            and any other declarations with this ident have the same signature.
          -}
          let errPrefix = CheckErr "external function declaration" $ show (FExt t ident params)
          sigConc <- getConcreteSig gs (t, params)
          sigMatches gs ident sigConc
          throwIf (isType gs ident) $ errPrefix "name is already a defined type"
          throwIf (sigHasDefState gs IDecl ident || sigHasDefState gs IDef ident) $ errPrefix "already declared internally"
          return gs {fnSigs = Map.insert ident (EDecl, sigConc) $ fnSigs gs}

        checkGDecl gs (FDefn t ident params stmts) = do
          {-
            A function definition is valid if there are no types so far with this ident,
            it matches any type signatures already seen, it is never declared externally,
            it has not already been defined, and its body statements are valid.
          -}
          let errPrefix = CheckErr "function definition" $ show (FDefn t ident params stmts)
          sigConc <- getConcreteSig gs (t, params)
          sigMatches gs ident sigConc
          let (retConc, paramConcs) = sigConc
              gs' = gs {fnSigs = Map.insert ident (IDef, sigConc) $ fnSigs gs}
              fns = newFnState (map Common.paramIdent params) paramConcs
          fns' <- checkStmts retConc (fns, gs') stmts
          throwIf (isType gs ident) $ errPrefix "name is already a defined type"
          throwIf (sigHasDefState gs EDecl ident) $ errPrefix "already declared externally"
          throwIf (sigHasDefState gs IDef ident) $ errPrefix "cannot redefine function"
          throwIf (not (foundRet fns') && cNeq gs retConc VoidC) $ errPrefix "non-void functions must always return"
          return gs'

        checkGDecl gs (SDefn ident fields) = do
          {-
            A struct definition is valid if it has not been defined before,
            it has no duplicate field names,
            it does not contain itself (though containing a pointer to itself is okay),
            its fields do not contain implicit definitions of structs
            (though pointers to implicitly defined structs are okay),
            and it has no void fields.
          -}
          let errPrefix = CheckErr "struct definition" $ show (SDefn ident fields)
          concFields <- mapM (getConcreteParam gs) fields
          throwIf (Map.member ident $ structs gs) $ errPrefix "cannot redefine struct"
          throwIf (length (List.nub $ map cParamIdent concFields) /= length concFields) $ errPrefix "cannot have duplicate field names"
          let structFields = Maybe.mapMaybe (identIfStruct . cParamT) concFields
          throwIf (any ((==) ident) structFields) $ errPrefix "structs cannot contain themselves"
          throwIf (any (flip Map.notMember $ structs gs) structFields) $ errPrefix "fields cannot implicitly define structs"
          throwIf (any (cEq gs VoidC . cParamT) concFields) $ errPrefix "cannot have void fields"
          throwIf (any (isFn . cParamT) concFields) $ errPrefix "cannot have function fields"
          return gs {structs = Map.insert ident concFields $ structs gs}

        -- If a concrete type is a struct, gets its ident.
        -- NOTE: We do not count pointers to structs.
        identIfStruct :: Conc -> Maybe Common.Ident
        identIfStruct (StructC ident) = Just ident
        identIfStruct _ = Nothing

        -- Determines whether a type is a function type.
        isFn :: Conc -> Bool
        isFn (FnC _) = True
        isFn _ = False

        -- Determines whether an ident is already a type (or signature) name.
        isType :: GlobalState -> Common.Ident -> Bool
        isType gs ident =
          Map.member ident (typedefs gs) || Map.member ident (sigdefs gs)

        -- Checkes that a signature matches any existing signatures with the same ident.
        sigMatches :: GlobalState -> Common.Ident -> CSig -> CheckMonad ()
        sigMatches gs ident sig = do
          let errPrefix = CheckErr ("signature for '" ++ ident ++ "'") (show sig)
          case Map.lookup ident $ fnSigs gs of
            Nothing -> return ()
            Just (_, prevSig) -> throwIf (cSigNeq gs sig prevSig) $ errPrefix "does not match existing signature"

        {-
          Returns a sig for the given (retT, params) if:
            - doesn't use void types
            - doesnt' collide with type names
            - doesn't have dupicate idents
            - all types resolve to concrete types
        -}
        getConcreteSig :: GlobalState -> (Common.Type, [Common.Param]) -> CheckMonad CSig
        getConcreteSig gs (retT, params) = do
          retConc <- getConcrete gs retT
          cParams <- mapM (getConcreteParam gs) params
          let errPrefix = CheckErr "signature" (show (retT, params))
              repeats = length (List.nub $ map Common.paramIdent params) /= length params
              someVoids = any (cEq gs VoidC . cParamT) cParams
              someLarges = any (isLarge . cParamT) cParams
              someTypes = any (\ident -> Map.member ident $ typedefs gs) (map Common.paramIdent params)
              retLarge = isLarge retConc
          throwIf repeats $ errPrefix "repeated parameter names"
          throwIf someVoids $ errPrefix "cannot have void parameters"
          throwIf someLarges $ errPrefix "cannot have large parameters"
          throwIf someTypes $ errPrefix "some parameters already used as defined type names"
          throwIf retLarge $ errPrefix "cannot return a large type"
          return (retConc, map cParamT cParams)

        -- Gets a concrete param (CParam) from a Param.
        getConcreteParam :: GlobalState -> Common.Param -> CheckMonad CParam
        getConcreteParam gs (Common.Param t ident) = do
          conc <- getConcrete gs t
          return $ CParam conc ident

        -- Checks whether an identifier is a function with the given DefState.
        sigHasDefState :: GlobalState -> DefState -> Common.Ident -> Bool
        sigHasDefState gs expected ident =
          case Map.lookup ident $ fnSigs gs of
            Nothing -> False
            Just (actual, _) -> actual == expected

-- Type-check a sequence of stmts under global and local states to a given return type.
checkStmts :: Conc -> (FnState, GlobalState) -> [Stmt] -> CheckMonad FnState
checkStmts expectedRetConc (fns, gs) stmts = foldM (checkStmt expectedRetConc) fns stmts
  where checkStmt :: Conc -> FnState -> Stmt -> CheckMonad FnState
        checkStmt expectedRetConc fns (Assn asop lValue e) = do
          {-
            An Assn is valid iff the ident is defined and has the same type as the expression.
            If the asop is an <op>= and the destination is a variable, we also require that the
            destination is defined.
          -}
          let errPrefix = CheckErr "assignment" $ show (Assn asop lValue e)
          expectedConc <- checkLValue (fns, gs) lValue
          actualConc <- checkExp (fns, gs) e
          throwIf (cNeq gs expectedConc actualConc) $ errPrefix "expression type does not match variable type"
          throwIf (isLarge actualConc) $ errPrefix "cannot assign values to large types"
          case lValue of
            LIdent ident -> case asop of
              Common.SetOp _ -> do
                throwIf (Set.notMember ident $ defined fns) $ errPrefix "destination not defined"
                return fns
              Common.Set -> return fns {defined = Set.insert ident $ defined fns}
            _ -> return fns
            {-
              TODO: Instead of checking whether expectedConc /= actualConc, we should do this *and*
              include the case where expected is a Prf FnC and actual is an Ptr AnyFn
            -}

        checkStmt expectedRetConc fns (If e thenStmts elseStmts) = do
          {-
            An If is valid iff the expression checks to a BoolT and both branches are valid under the
            fns before the If.
          -}
          let errPrefix = CheckErr "if-statment" $ show (If e thenStmts elseStmts)
          eConc <- checkExp (fns, gs) e
          thenFnState <- checkStmts expectedRetConc (fns, gs) thenStmts
          elseFnState <- checkStmts expectedRetConc (fns, gs) elseStmts
          throwIf (cNeq gs eConc BoolC) $ errPrefix "condition must be a 'bool'"
          return fns {
            defined = Set.intersection (defined thenFnState) (defined elseFnState),
            foundRet = foundRet fns || (foundRet thenFnState && foundRet elseFnState) }

        checkStmt expectedRetConc fns (While e bodyStmts) = do
          {-
            A While is valid iff its condition checks to a BoolT and its body is valid.
          -}
          let errPrefix = CheckErr "loop" $ show (While e bodyStmts)
          eConc <- checkExp (fns, gs) e
          bodyFnState <- checkStmts expectedRetConc (fns, gs) bodyStmts
          throwIf (cNeq gs eConc BoolC) $ errPrefix "condition must be a 'bool'"
          return fns

        checkStmt expectedRetConc fns (Return maybeE) = do
          {-
            A Return is valid iff its expression checks to the provided type,
            or if it has no expression and the provided type is Void.
            Note that return implicitly defines all variables that are declared before it.
          -}
          let errPrefix = CheckErr "return statment" $ show (Return maybeE)
          case maybeE of
            Nothing -> throwIf (cNeq gs expectedRetConc VoidC) $ errPrefix "non-void functions must return values"
            Just e -> do
              actualRetConc <- checkExp (fns, gs) e
              throwIf (cNeq gs actualRetConc expectedRetConc) $ errPrefix "actual and expected return types don't match"
              throwIf (cEq gs actualRetConc VoidC) $ errPrefix "void functions must have empty return statements"
          return fns {defined = Map.keysSet $ declared fns, foundRet = True}

        checkStmt expectedRetConc fns (Decl t ident maybeDef scopeStmts) = do
          {-
            A Decl is valid iff the variable that it declares does not collide with an existing
            variable or type name,
            its type is not void (because we cannot declare variables of type void),
            its definition expression (if it has one) has a type that matches the decl type,
            and its scopeStmts are valid once it is included in the fns.
          -}
          let errPrefix = CheckErr "variable declaration" $ show (Decl t ident maybeDef scopeStmts)
          conc <- getConcrete gs t
          fns' <- case maybeDef of
            Nothing -> return fns
            Just def -> do
              defConc <- checkExp (fns, gs) def
              throwIf (cNeq gs conc defConc) $ errPrefix $ "variable declaration type (" ++ show conc ++ ") does not match definition type (" ++ show defConc ++ ")"
              return fns {defined = Set.insert ident $ defined fns}
          let fns'' = fns' {declared = Map.insert ident conc $ declared fns'}
          scopeFnState <- checkStmts expectedRetConc (fns'', gs) scopeStmts
          throwIf (cEq gs conc VoidC) $ errPrefix "variables cannot be void"
          throwIf (isLarge conc) $ errPrefix "variables cannot be large types (structs)"
          throwIf (Map.member ident $ declared fns) $ errPrefix "variable already declared"
          throwIf (Map.member ident $ typedefs gs) $ errPrefix "variable name conflicts with type name"
          return fns'' {
            defined = Set.union (defined fns) $ Set.delete ident (defined scopeFnState),
            declared = Map.delete ident $ declared fns'',
            foundRet = foundRet fns || foundRet scopeFnState }

        checkStmt expectedRetConc fns (Assert e) = do
          {-
            An Assert is valid iff its condition checks to a BoolT.
          -}
          let errPrefix = CheckErr "assert" $ show (Assert e)
          conc <- checkExp (fns, gs) e
          throwIf (cNeq gs conc BoolC) $ errPrefix "condition must be a 'bool'"
          return fns

        checkStmt expectedRetConc fns (Exp e) = do
          {-
            An Exp is valid if its expression is well-typed.
          -}
          let errPrefix = CheckErr "expression as statement" $ show (Exp e)
          conc <- checkExp (fns, gs) e
          throwIf (isLarge conc) $ errPrefix "cannot evaluate to large type"
          return fns

-- Type-check an LValue under the given function and global states.
-- NOTE: Indent can be undefined only if they are at the top level, assuming that they are assigned
-- into with '=' as opposed to an asop like '+='.
checkLValue :: (FnState, GlobalState) -> LValue -> CheckMonad Conc
checkLValue (fns, gs) lValue = checkLValue' (fns, gs) True lValue
  where checkLValue' :: (FnState, GlobalState) -> Bool -> LValue -> CheckMonad Conc
        checkLValue' (fns, gs) isTop (LIdent ident) = do
          let errPrefix = CheckErr "variable" (show $ LIdent ident)
          throwIf (not isTop && (Set.notMember ident $ defined fns)) $ errPrefix "must be defined before use"
          lookupType fns ident
        checkLValue' (fns, gs) isTop (LDot struct field) = do
          let errPrefix = CheckErr "field access" (show $ LDot struct field)
          structConc <- checkLValue' (fns, gs) False struct
          case structConc of
            StructC ident -> lookupField gs ident field
            _ -> throwError $ errPrefix "operand is not a struct"
        checkLValue' (fns, gs) isTop (LStar addr) = do
          let errPrefix = CheckErr "dereference" (show $ LStar addr)
          addrConc <- checkLValue' (fns, gs) False addr
          case addrConc of
            PtrC pointed -> checkPointed pointed
            _ -> throwError $ errPrefix "operand is not a pointer"
        checkLValue' (fns, gs) isTop (LIndex arr index) = do
          let errPrefix = CheckErr "array access" (show $ LIndex arr index)
          indexConc <- checkExp (fns, gs) index
          throwIf (cNeq gs indexConc IntC) $ errPrefix "index is not an integer"
          arrConc <- checkLValue' (fns, gs) False arr
          case arrConc of
            ArrC conc -> return conc
            _ -> throwError $ errPrefix "operand is not an array address"

-- Gets the concrete type of an expression under the given fns.
checkExp :: (FnState, GlobalState) -> Exp -> CheckMonad Conc
checkExp (fns, gs) e = do
  -- NOTE: Other than functions, no expressions are allowed to check to 'void'.
  let errPrefix = CheckErr "expression" (show e)
  conc <- checkExp' (fns, gs) e
  throwIf (cEq gs conc VoidC && isntCall e) $ errPrefix "non-call expressions cannot be void"
  return conc
  where isntCall :: Exp -> Bool
        isntCall (Call _ _) = False
        isntCall _ = True

        checkExp' :: (FnState, GlobalState) -> Exp -> CheckMonad Conc
        checkExp' (fns, gs) (IntLit (Common.Dec d)) = do
          let errPrefix = CheckErr "decimal literal" $ show d
          throwIf (d > 2^31 || d < -2^31) $ errPrefix "out of range"
          return IntC
        checkExp' (fns, gs) (IntLit (Common.Hex h)) = do
          let errPrefix = CheckErr "hex literal" $ show h
          throwIf (h >= 2^32 || h < 0) $ errPrefix "out of range"
          return IntC
        checkExp' (fns, gs) (CharLit c) = do
          let errPrefix = CheckErr "character literal" $ show c
              ord = Char.ord c
          throwIf (ord >= 128 || ord < 0) $ errPrefix "invalid ASCII value"
          return CharC
        checkExp' (fns, gs) (StringLit s) = do
          let errPrefix = CheckErr "string literal" $ show s
          throwIf (any ((==) '\0') s) $ errPrefix "contains null character"
          return StringC
        checkExp' (fns, gs) (BoolLit b) = return BoolC
        checkExp' (fns, gs) Null = return $ PtrC Any
        checkExp' (fns, gs) (Ident ident) = do
          {-
            An ident has a type if it was both declared with that type and defined as a value of that
            type.
          -}
          let errPrefix = CheckErr "variable name" $ show e
          conc <- lookupType fns ident
          throwIf (Set.notMember ident $ defined fns) $ errPrefix "declared, but never defined"
          return conc
        checkExp' (fns, gs) (Binop binop e1 e2) = do
          conc1 <- checkExp (fns, gs) e1
          conc2 <- checkExp (fns, gs) e2
          checkBinop gs binop (conc1, conc2)
        checkExp' (fns, gs) (Unop unop e) = do
          conc <- checkExp (fns, gs) e
          checkUnop gs unop conc
        checkExp' (fns, gs) (Cond e1 e2 e3) = do
          let errPrefix = CheckErr "ternary expression" $ show e
          conc1 <- checkExp (fns, gs) e1
          conc2 <- checkExp (fns, gs) e2
          conc3 <- checkExp (fns, gs) e3
          throwIf (cNeq gs conc1 BoolC) $ errPrefix $ "first operand must be a 'bool'"
          throwIf (cNeq gs conc2 conc3) $ errPrefix "branches have different types"
          throwIf (isLarge conc2) $ errPrefix "branches cannot have large types"

          -- NOTE: Handles the case where exactly one of conc2 and conc3 is an Any* or AnyFn*,
          -- and we should take the type of the other.
          return $ case conc2 of
            PtrC Any -> conc3
            PtrC (AnyFn _) -> conc3
            _ -> conc2
        checkExp' (fns, gs) (Call e args) = do
          {-
            A call typechecks to the return type of its signature if the signature has already been
            encountered (and matches the args given here),
            the function is defined at some point (not necessarily before this),
            and the function ident is not currently shadowed by a local variable.

            TODO: This is a little messy. Is there a better way?
            or, will be able to share some of this code with the Assn case?
          -}
          let errPrefix = CheckErr "function call" $ show e
          sig <- case e of
            Ident ident -> do
              throwIf (Map.member ident $ declared fns) $ errPrefix "function defined, but shadowed"
              lookupSigFromIdent gs ident
            _ -> do
              conc <- checkExp (fns, gs) e
              case conc of
                FnC ident -> lookupSigFromType gs ident
                _ -> throwError $ errPrefix "cannot call a non-function"
          let (concRetT, concExpectedArgs) = sig
          concActualArgs <- mapM (checkExp (fns, gs)) args
          let badArgs = (or $ zipWith (cNeq gs) concActualArgs concExpectedArgs) || length concActualArgs /= length concExpectedArgs
          throwIf badArgs $ errPrefix "argument types do not match signature"
          return concRetT
        checkExp' (fns, gs) (Alloc t) = do
          let errPrefix = CheckErr "allocation" $ show e
          conc <- getConcrete gs t
          throwIf (cEq gs conc VoidC) $ errPrefix "cannot allocate a void*"
          case conc of
            StructC ident -> throwIf (Map.notMember ident $ structs gs) $ errPrefix "cannot allocate struct without explicit definition"
            _ -> return ()
          return $ PtrC $ One conc
        checkExp' (fns, gs) (AllocArray t e) = do
          let errPrefix = CheckErr "array allocation" $ show e
          conc <- getConcrete gs t
          eConc <- checkExp (fns, gs) e
          throwIf (cEq gs conc VoidC) $ errPrefix "cannot allocate a void[]"
          throwIf (cNeq gs eConc IntC) $ errPrefix "size is not an integer"
          case conc of
            StructC ident -> throwIf (Map.notMember ident $ structs gs) $ errPrefix "cannot allocate struct without explicit definition"
            _ -> return ()
          return $ ArrC conc
        checkExp' (fns, gs) (Dot struct field) = do
          let errPrefix = CheckErr "field access" $ show e
          structConc <- checkExp (fns, gs) struct
          case structConc of
            StructC ident -> lookupField gs ident field
            _ -> throwError $ errPrefix "operand is not a struct"
        checkExp' (fns, gs) (Star addr) = do
          let errPrefix = CheckErr "dereference" $ show e
          addrConc <- checkExp (fns, gs) addr
          case addrConc of
            PtrC pointed -> checkPointed pointed
            _ -> throwError $ errPrefix "operand is not a pointer"
        checkExp' (fns, gs) (Index arr index) = do
          let errPrefix = CheckErr "array access" $ show e
          indexConc <- checkExp (fns, gs) index
          throwIf (cNeq gs indexConc IntC) $ errPrefix "index is not an integer"
          arrConc <- checkExp (fns, gs) arr
          case arrConc of
            ArrC conc -> return conc
            _ -> throwError $ errPrefix "operand is not an array address"
        checkExp' (fns, gs) (Amp ident) = do
          let errPrefix = CheckErr "reference" $ show e
          sig <- lookupSigFromIdent gs ident
          return $ PtrC $ AnyFn sig
        checkExp' (fns, gs) (Cast t ptr) = do
          let errPrefix = CheckErr "cast" $ show e
          fromConc <- checkExp (fns, gs) ptr
          toConc <- getConcrete gs t
          case (fromConc, toConc) of
            (PtrC (One VoidC),  PtrC (One VoidC))   -> throwError $ errPrefix "cannot cast from (void*) to (void*)"
            (PtrC (One _),      PtrC (One VoidC))   -> return toConc
            (PtrC (One VoidC),  PtrC (One _))       -> return toConc
            (PtrC Any,          PtrC (One VoidC))   -> return toConc
            (PtrC Any,          PtrC (One _))       -> throwError $ errPrefix "cannot untag NULL"
            _ -> throwError $ errPrefix "Invalid source or destination type"

{-
  Checks the value pointed to by a pointer.
-}
checkPointed :: Pointed -> CheckMonad Conc
checkPointed pointed =
  let errPrefix = CheckErr "pointer target" $ show pointed
  in case pointed of
    Any -> throwError $ errPrefix "cannot dereference NULL"
    AnyFn _ -> throwError $ errPrefix "cannot dereference function pointer before assigning type"
    One conc -> return conc

{-
  Given a unary operator and the concrete type of its input, returns an output type if the input
  is of the correct type for the operator.
-}
checkUnop :: GlobalState -> Unop -> Conc -> CheckMonad Conc
checkUnop gs unop =
  case unop of
    Common.Bang -> unopChecker BoolC BoolC
    Common.Inv -> unopChecker IntC IntC
    Common.Neg -> unopChecker IntC IntC
  where unopChecker :: Conc -> Conc -> Conc -> CheckMonad Conc
        unopChecker expected out actual = do
          let errPrefix = CheckErr "unary operation" $ show unop
          throwIf (cNeq gs expected actual) $ errPrefix "invalid operand type"
          return out

{-
  Given a binary operator and the concrete types of its inputs, returns an output type if the
  inputs are of the correct type for the operator.
-}
checkBinop :: GlobalState -> Binop -> (Conc, Conc) -> CheckMonad Conc
checkBinop gs binop =
  case binop of
    CmpOp (Common.CmpOp Common.E) -> eqNeqChecker
    CmpOp (Common.CmpOp Common.NE) -> eqNeqChecker
    CmpOp _ -> compoundOpChecker [
      basicOpChecker (IntC,   IntC)   BoolC ,
      basicOpChecker (CharC,  CharC)  BoolC ]
    LogOp _ -> basicOpChecker (BoolC, BoolC) BoolC
    ArithOp _ ->  basicOpChecker (IntC, IntC) IntC
  where errPrefix :: String -> CheckErr
        errPrefix = CheckErr "binary operation" $ show binop

        basicOpChecker :: (Conc, Conc) -> Conc -> (Conc, Conc) -> CheckMonad Conc
        basicOpChecker expected@(expectedIn1, expectedIn2) out actual@(actualIn1, actualIn2) = do
          throwIf (cNeq gs expectedIn1 actualIn1 || cNeq gs expectedIn2 actualIn2) $ errPrefix "invalid operand types"
          return out

        arrChecker :: (Conc, Conc) -> CheckMonad Conc
        arrChecker (ArrC conc1, ArrC conc2) = do
          throwIf (cNeq gs conc1 conc2) $ errPrefix "unequal array types"
          return $ BoolC
        arrChecker _ = throwError $ errPrefix "invalid operand types"

        ptrChecker :: (Conc, Conc) -> CheckMonad Conc
        ptrChecker (PtrC pointed1, PtrC pointed2) = do
          throwIf (not $ pEq gs pointed1 pointed2) $ errPrefix "unequal pointer types"
          return $ BoolC
        ptrChecker _ = throwError $ errPrefix "invalid operand types"

        compoundOpChecker :: [(Conc, Conc) -> CheckMonad Conc] -> (Conc, Conc) -> CheckMonad Conc
        compoundOpChecker [] actualIn = throwError $ errPrefix $ "invalid operand types"
        compoundOpChecker (checker : checkers) actualIn =
          catchError (checker actualIn) (\_ -> compoundOpChecker checkers actualIn)

        eqNeqChecker :: (Conc, Conc) -> CheckMonad Conc
        eqNeqChecker = compoundOpChecker [  basicOpChecker (BoolC, BoolC) BoolC ,
                                            basicOpChecker (IntC,  IntC ) BoolC ,
                                            basicOpChecker (CharC, CharC) BoolC ,
                                            arrChecker                          ,
                                            ptrChecker                          ]

-- Determines whether a concrete type is a large type (i.e. a struct or function).
isLarge :: Conc -> Bool
isLarge (StructC _) = True
isLarge (FnC _) = True
isLarge _ = False

-- Throws an error if a condition is met.
throwIf :: Bool -> CheckErr -> CheckMonad ()
throwIf cond err = do
  if cond
    then throwError err
    else return ()

-- Attempts to resolve a (possibly DefT) type to a Concrete type in this GlobalState.
-- NOTE: Any struct type is automatically concrete. But this does not guarantee that it's been defined.
-- TODO: Should we really be checking for whether Arrs contain VoidT here?
getConcrete :: GlobalState -> Common.Type -> CheckMonad Conc
getConcrete gs Common.VoidT = return VoidC
getConcrete gs Common.StringT = return StringC
getConcrete gs Common.CharT = return CharC
getConcrete gs Common.IntT = return IntC
getConcrete gs Common.BoolT = return BoolC
getConcrete gs (Common.StructT ident) = return $ StructC ident
getConcrete gs (Common.FnT ident) = return $ FnC ident
getConcrete gs (Common.ArrT t) = do
  conc <- getConcrete gs t
  if cNeq gs conc VoidC
    then return $ ArrC conc
    else throwError $ CheckErr "type" (show $ Common.ArrT t) "cannot have a void[]"
getConcrete gs (Common.PtrT t) = do
  conc <- getConcrete gs t
  return $ PtrC $ One conc
getConcrete gs (Common.DefT ident) = do
  case Map.lookup ident $ typedefs gs of
    Nothing -> throwError $ CheckErr "type" (show $ Common.DefT ident) "no defined type with this name"
    Just conc -> return conc

-- Attempts to find an idents concrete type in a FnState.
lookupType :: FnState -> Common.Ident -> CheckMonad Conc
lookupType fns ident = do
  case Map.lookup ident $ declared fns of
    Nothing -> throwError $ CheckErr "variable" ident "not declared in this context"
    Just conc -> return conc

-- Gets a funciton's signature, given a type defined with a sigdef.
-- The ident given is the ident from a 'FnC <ident>' concrete type.
lookupSigFromType :: GlobalState -> Common.Ident -> CheckMonad CSig
lookupSigFromType gs ident = do
  let errPrefix = CheckErr "function type" ident
  case Map.lookup ident $ sigdefs gs of
    Nothing -> throwError $ errPrefix "no function type with this name"
    Just sig -> return sig

-- Gets a function's signature, giving its ident.
-- NOTE: We throw an error if the function is never defined, and therefore unusable.
lookupSigFromIdent :: GlobalState -> Common.Ident -> CheckMonad CSig
lookupSigFromIdent gs ident = do
  let errPrefix = CheckErr "function name" ident
  case Map.lookup ident $ fnSigs gs of
    Nothing -> throwError $ errPrefix "function not defined"
    Just (INever, _) -> throwError $ errPrefix "function declared but never defined"
    Just (_, sig) -> return sig

-- Attempts to find the concrete type for the given field in the given struct.
-- NOTE: We require structs to be defined here.
lookupField :: GlobalState -> Common.Ident -> Common.Ident -> CheckMonad Conc
lookupField gs structIdent field = do
  let errPrefix = CheckErr "field lookup" $ structIdent ++ "." ++ field
  case Map.lookup structIdent $ structs gs of
    Nothing -> throwError $ errPrefix "struct must be explicitly defined for field lookup"
    Just fields -> case lookup field $ map (\(CParam conc ident) -> (ident, conc)) fields of
      Nothing -> throwError $ errPrefix $ "no field with this name of this struct"
      Just conc -> return conc
