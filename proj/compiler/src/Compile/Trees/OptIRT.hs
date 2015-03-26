module Compile.Trees.OptIRT where

import Compile.Types.IRT
import Compile.Types.Common (TmpIdent, Error(..), Size(..), boolToInt32)
import qualified Compile.Types.Common as Common
import qualified Job

import Data.Int (Int32)
import Data.List (findIndex)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- A function for determining which functions to inline.
type InlineHeuristic = Fn -> Bool

-- A map from inlineable function names to their definitions.
type InlineMap = Map.Map Common.FnName Fn

-- Applies a number of optimizations to an IRT.
optIRT :: Job.Job -> IRT -> IRT
optIRT job =
  if Job.lvl job < 2
    then id
    else inlineFns noCtrlStmts . simplify

{-
  Simplifies the given IRT by removing unreachable code and folding constants.
  Note that, even though both these were done in OptPCT, there's still potential for
  improvement:
  --> Cond being folded out separately in GenIRT allows for the introduction of new literals
      which could then be folded away.
  --> Removing decls in GenIRT makes for flatter code, and therefore for more code being
      unreachable behind returns/other terminals.
  --> Some of the CheckFoos generated in GenIRT are obviously unnecessary, and might as well be
      removed now.
-}
simplify :: IRT -> IRT
simplify (Prog fns) = Prog (map simplifyFn fns)
  where simplifyFn :: Fn -> Fn
        simplifyFn (Fn fName params stmts) = Fn fName params (simplifyStmts stmts)

        simplifyStmts :: [Stmt] -> [Stmt]
        simplifyStmts stmts = trimStmts $ concatMap simplifyStmt stmts

        trimStmts :: [Stmt] -> [Stmt]
        trimStmts stmts =
          case (findIndex isTerminal stmts) of
            Nothing -> stmts
            (Just i) -> take (i+1) stmts

        isTerminal :: Stmt -> Bool
        isTerminal (While _ (Term (NumLit _ 1)) _) = True
        isTerminal (Return _) = True
        isTerminal (Raise _) = True
        isTerminal _ = False

        {-
          Does constant and statement folding.
        -}
        simplifyStmt :: Stmt -> [Stmt]
        simplifyStmt (Assn lVal e) = [Assn lVal (foldExp e)]
        simplifyStmt stmt@(Memcpy _ _) = [stmt]
        simplifyStmt (Call sz t name args) = [Call sz t name $ map foldExp args]
        simplifyStmt (CallPtr sz t ptrTerm args) =
          case ptrTerm of
            (FnPtr fName) -> [Call sz t fName $ map foldExp args]
            _ -> [CallPtr sz t ptrTerm $ map foldExp args]
        simplifyStmt (MemRead sz t mem) = [MemRead sz t mem]
        simplifyStmt (If e thenStmts elseStmts) =
          case foldExp e of
            (Term (NumLit _ n)) -> (if Common.int32ToBool n
                                    then simplifyStmts thenStmts
                                    else simplifyStmts elseStmts)
            e' -> [If e' (simplifyStmts thenStmts) (simplifyStmts elseStmts)]
        simplifyStmt (While condStmts e bodyStmts) =
          case (foldExp e) of
            t@(Term (NumLit _ n)) -> (if Common.int32ToBool n
                                      then [While (simplifyStmts condStmts) t
                                                  (simplifyStmts bodyStmts)]
                                      else [])
            e' -> [While (simplifyStmts condStmts) e' (simplifyStmts bodyStmts)]
        simplifyStmt (Return e) = [Return $ foldExp e]
        simplifyStmt stmt@(CheckNonZero term err) =
          case term of
            (NumLit _ n) -> if n == 0 then [Raise err] else []
            _ -> [stmt]
        simplifyStmt stmt@(CheckNonNeg term err) =
          case term of
            (NumLit _ n) -> if n < 0 then [Raise err] else []
            _ -> [stmt]
        simplifyStmt stmt@(CheckEqual term1 term2 err) =
          case (term1, term2) of
            (NumLit _ n1, NumLit _ n2) -> if (n1 == n2) then [] else [Raise err]
            (Tmp t1, Tmp t2) -> if (t1 == t2) then [] else [stmt]
            _ -> [stmt]
        simplifyStmt stmt@(CheckDivOverflow term1 term2) =
          case (term1, term2) of
            (NumLit _ n1, NumLit _ (-1)) -> if n1 == minInt then [Raise ArithError] else []
            (_, NumLit _ n) -> if n /= (-1) then [] else [stmt]
            (NumLit _ n, _) -> if n /= minInt then [] else [stmt]
            (_, _) -> [stmt]
        simplifyStmt stmt@(CheckShiftMax term) =
          case term of
            (NumLit _ n) -> if n >= 32 then [Raise ArithError] else []
            _ -> [stmt]
        simplifyStmt stmt@(CheckArrEnd _ _) = [stmt]
        simplifyStmt stmt@(Raise _) = [stmt]

        foldExp :: Exp -> Exp
        foldExp e@(Term _) = e
        foldExp (Binop binop e1 e2) =
          let e1' = foldExp e1
              e2' = foldExp e2
          in case (tryFold binop e1' e2') of
              Nothing -> Binop binop e1' e2'
              Just e' -> e'

        {-
          Tries to fold two expressions with a binary operator.
          If it succeeds, returns (Just result). Otherwise, returns Nothing.
          NOTE: this folds ops that could fail without checking them, since anything that would
                fail is preceeded by a check that would be marked as terminal, so the folding
                wouldn't be evaluated anyway.
        -}
        tryFold :: Binop -> Exp -> Exp -> Maybe Exp
        tryFold (CmpOp (Common.CmpOp c)) (Term (NumLit _ n1)) (Term (NumLit _ n2)) =
          Just $ Term $ NumLit Common.Byte $ Common.boolToInt32 $ Common.applyCmp c n1 n2
        tryFold (ArithOp op) (Term (NumLit sz n1)) (Term (NumLit _ n2)) =
          Just $ Term $ NumLit sz $ Common.applyArithOp op n1 n2
        tryFold _ _ _ = Nothing

        minInt :: Int32
        minInt = minBound

{-
  Inlines all function calls to functions which satisfy the given heuristic.
  Inlining is all-or-nothing: a function is either always inlined, or never inlined.
  General inlining process is:
    1.  Recursively inline all inlineable statements in the function being inlined.
    2.  Move all arguments from the inlined call to the corresponding temps in the function
        being inlined.
    3.  Replace all Return statements in the function being inlined with moves to the temp that
        that function was called into before inlining.
-}
inlineFns :: InlineHeuristic -> IRT -> IRT
inlineFns heuristic (Prog fns) =
  let iMap = Map.fromList $ map pairWithName $ filter heuristic fns
  in  Prog $ map (inlineFnsFn iMap) fns
  where pairWithName :: Fn -> (Common.FnName, Fn)
        pairWithName fn@(Fn name _ _) = (name, fn)

-- Inline all inlineable funciton calls in this function.
inlineFnsFn :: InlineMap -> Fn -> Fn
inlineFnsFn iMap (Fn name paramTs stmts) =
  Fn name paramTs $ concatMap (inlineFnsStmt iMap) stmts

-- Inlines any statements that are calls. Passes other statments through.
inlineFnsStmt :: InlineMap -> Stmt -> [Stmt]
inlineFnsStmt iMap (Call sz t name args) =
  case Map.lookup name iMap of
    Nothing -> [Call sz t name args]
    Just (Fn _ params bodyStmts) -> getInlineableCode iMap t args (map fst params) bodyStmts
inlineFnsStmt iMap stmt = [stmt]

{-
  Translates a function body into a chunk of inlineable code:
    1.  Shift all temps (including params) forward so that the will not conflict with temps
        above the spot where the code will be inlined.
        NOTE: Here we take advantage of the fact that resT was the next available temp when
        this Call was created, so all temps > it can be safely used as 'scratch space' by the
        inlined code.
    2.  Replace all return statments with Assns into the temp which originally received the
        result of the function call.
    3.  Add Assn statments to move all arguments into params.
-}
getInlineableCode :: InlineMap -> TmpIdent -> [Exp] -> [TmpIdent] -> [Stmt] -> [Stmt]
getInlineableCode iMap resT args params bodyStmts =
  let bodyStmts' = map (replaceReturns resT . shiftStmt resT) bodyStmts
      params' = map (shiftTmp resT) params
      moveArgs = zipWith (\t -> \e -> Assn (LTmp t) e) params' args
  in  moveArgs ++ bodyStmts'
  where -- Shifts temps in a statement forward so that the do not conflict with temps <= resT.
        shiftStmt :: TmpIdent -> Stmt -> Stmt
        shiftStmt resT (Assn l e) = Assn (shiftLValue resT l) (shiftExp resT e)
        shiftStmt resT (Memcpy t bytes) = Memcpy (shiftTmp resT t) bytes
        shiftStmt resT (Call sz t name args) =
          let t' = shiftTmp resT t
              args' = map (shiftExp resT) args
          in Call sz t' name args'
        shiftStmt resT (CallPtr sz t fPtrTerm args) =
          let t' = shiftTmp resT t
              fPtrTerm' = shiftTerm resT fPtrTerm
              args' = map (shiftExp resT) args
          in CallPtr sz t' fPtrTerm' args'
        shiftStmt resT (MemRead sz t mem) = MemRead sz (shiftTmp resT t) (shiftMem resT mem)
        shiftStmt resT (If e thenStmts elseStmts) =
          let e' = shiftExp resT e
              thenStmts' = map (shiftStmt resT) thenStmts
              elseStmts' = map (shiftStmt resT) elseStmts
          in  If e' thenStmts' elseStmts'
        shiftStmt resT (While condStmts e bodyStmts) =
          let condStmts' = map (shiftStmt resT) condStmts
              e' = shiftExp resT e
              bodyStmts' = map (shiftStmt resT) bodyStmts
          in  While condStmts' e' bodyStmts'
        shiftStmt resT (Return e) = Return (shiftExp resT e)
        shiftStmt resT (CheckNonZero t err) = CheckNonZero (shiftTerm resT t) err
        shiftStmt resT (CheckNonNeg t err) = CheckNonNeg (shiftTerm resT t) err
        shiftStmt resT (CheckEqual term1 term2 err) =
          CheckEqual (shiftTerm resT term1) (shiftTerm resT term2) err
        shiftStmt resT (CheckDivOverflow lTerm rTerm) =
          CheckDivOverflow (shiftTerm resT lTerm) (shiftTerm resT rTerm)
        shiftStmt resT (CheckShiftMax term) = CheckShiftMax (shiftTerm resT term)
        shiftStmt resT (CheckArrEnd addrTerm idxTerm) =
          CheckArrEnd (shiftTerm resT addrTerm) (shiftTerm resT idxTerm)
        shiftStmt _ stmt@(Raise _) = stmt

        -- Shifts temps in an lvalue forward so that they do not conflict with temps <= resT.
        shiftLValue :: TmpIdent -> LValue -> LValue
        shiftLValue resT (LTmp t) = LTmp $ shiftTmp resT t
        shiftLValue resT (LMem mem) = LMem $ shiftMem resT mem

        -- Shifts temps in an expression forward so that they do not conflict with temps <= resT.
        shiftExp :: TmpIdent -> Exp -> Exp
        shiftExp resT (Term term) = Term $ shiftTerm resT term
        shiftExp resT (Binop op l r) = Binop op (shiftExp resT l) (shiftExp resT r)

        -- Shifts temps in a term forward so that they do not conflict with temps <= resT.
        shiftTerm :: TmpIdent -> Term -> Term
        shiftTerm resT (NumLit sz n) = NumLit sz n
        shiftTerm resT (Tmp t) = Tmp $ shiftTmp resT t
        shiftTerm resT (FnPtr fnName) = FnPtr fnName

        -- Shift temps t into new temps that are strictly greater than resT.
        shiftTmp :: TmpIdent -> TmpIdent -> TmpIdent
        shiftTmp resT t = resT + 1 + t

        -- Shifts temps in a memory location so that they do not conflict with temps <= resT.
        shiftMem :: TmpIdent -> Mem -> Mem
        shiftMem resT (ArrMem eSz addrTerm idxTerm offset) =
          ArrMem eSz (shiftTerm resT addrTerm) (shiftTerm resT idxTerm) offset
        shiftMem resT (PtrMem addrTerm offset) = PtrMem (shiftTerm resT addrTerm) offset

        -- Replaces return statements with moves into a the original function call's result temp.
        replaceReturns :: TmpIdent -> Stmt -> Stmt
        replaceReturns resT (Return e) = Assn (LTmp resT) e
        replaceReturns resT (If e thenStmts elseStmts) =
          let thenStmts' = map (replaceReturns resT) thenStmts
              elseStmts' = map (replaceReturns resT) elseStmts
          in  If e thenStmts' elseStmts'
        replaceReturns resT (While condStmts e bodyStmts) =
          let condStmts' = map (replaceReturns resT) condStmts
              bodyStmts' = map (replaceReturns resT) bodyStmts
          in While condStmts' e bodyStmts'
        replaceReturns _ stmt = stmt

{-
  Determines whether a function is purely a return statement.
-}
-- TODO: could we also allow functions that just make a call, then return that call?
{-
  TODO: big bug -- merely replacing returns with assns is not enough...must make sure that
  to assns happen *after* the replaced return as well.
  For instance, consider an if statement that returns in the then branch, but not the else.
  However, note that this is only a problem if we are allowed to return functions which
  contain control flow, so it's fine with the current heuristic.
-}
isReturn :: Fn -> Bool
isReturn (Fn name paramTs stmts) =
  case stmts of
    [Return _] -> True
    _ -> False

noCtrlStmts :: Fn -> Bool
noCtrlStmts (Fn name paramTs stmts) = all isntCtrl stmts
  where isntCtrl (If _ _ _) = False
        isntCtrl (While _ _ _) = False
        isntCtrl _ = True
