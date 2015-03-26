{-
  Does optimizations on the PCT, mainly focusing on those which are about to be rendered much
  more difficult by the elaboration to IRT, like &&/|| folding and cancelling unops.
-}

module Compile.Trees.OptPCT where

import Compile.Types.PCT
import qualified Compile.Types.Common as Common
import qualified Compile.Graph.Graph as Graph
import qualified Compile.Graph.GraphAlgs as GraphAlgs
import qualified Job

import qualified Data.List as List
import qualified Data.Map as Map
import Data.List (findIndex)
import Data.Int (Int32)
import Data.Bits (xor, shiftL, shiftR, (.|.), (.&.))
import Data.Maybe (mapMaybe)

{-
  Performs the following optimizations on the PCT:
  --> Removing expressions after a return
  --> constant folding (and subsequent statement pruning)
  --> crossing out pairs of unops (~~x  ->  x, etc)
  --> removing binops that do nothing (0 + x  ->  x, etc)
-}
optPCT :: Job.Job -> PCT -> PCT
optPCT job (Prog fns) =
  if (Job.lvl job < 2)
  then (Prog fns)
  else (Prog $ map optFn fns)
  where optFn :: Fn -> Fn
        optFn (Fn name params stmts) = Fn name params (optStmts stmts)

        optStmts :: [Stmt] -> [Stmt]
        optStmts stmts = trimStmts $ concatMap optStmt stmts

        {-
          Removes all statements after the first Return.
        -}
        trimStmts :: [Stmt] -> [Stmt]
        trimStmts stmts =
          case (findIndex isReturn stmts) of
            Nothing -> stmts
            (Just i) -> take (i+1) stmts

        isReturn :: Stmt -> Bool
        isReturn (Return _) = True
        isReturn _ = False

        {-
          Optimizes the given PCT statement, returning the new statements generated from it.
        -}
        optStmt :: Stmt -> [Stmt]
        optStmt (Assn op lv e) = [Assn op (optLValue lv) (optExp e)]
        optStmt (If e ss1 ss2) =
          case (optExp e) of
            (BoolLit True) -> optStmts ss1
            (BoolLit False) -> optStmts ss2
            e' -> [If e' (optStmts ss1) (optStmts ss2)]
        optStmt (While e ss) =
          case (optExp e) of
            (BoolLit False) -> []
            e' -> [While e' (optStmts ss)]
        optStmt (Return e) = [Return (optExp e)]
        optStmt (Decl v sz stmts) = [Decl v sz (optStmts stmts)]
        optStmt (Assert e) = [Assert (optExp e)]
        optStmt (Exp e) = [Exp (optExp e)]

        optLValue :: LValue -> LValue
        optLValue (LIdent v) = LIdent v
        optLValue (LStar lv oOpt) = LStar (optLValue lv) oOpt
        optLValue (LIndex eSz lv idxE o) = LIndex eSz (optLValue lv) (optExp idxE) o

        {-
          Optimizes the given PCT expression by:
          --> arithmetic constant folding
          --> cancelling unops
          --> folding &&/||, where the side effects permit it
        -}
        optExp :: Exp -> Exp
        optExp e@(NumLit _) = e
        optExp e@(BoolLit _) = e
        optExp e@(CharLit _) = e
        optExp e@(StringLit _) = e
        optExp e@Null = e
        optExp e@(Ident _) = e
        optExp (Binop op e1 e2) = tryFoldBinop op (optExp e1) (optExp e2)
        optExp (Unop op e) =
          case (optExp e) of
            (Unop op' e') -> (if op == op'
                              then e'
                              else Unop op (Unop op' e'))
            e' -> Unop op e'
        optExp (Cond e1 e2 e3) =
          case (optExp e1) of
            (BoolLit True) -> optExp e2
            (BoolLit False) -> optExp e3
            e1' -> Cond e1' (optExp e2) (optExp e3)
        optExp (Tag e tag) = Tag (optExp e) tag
        optExp (Untag e tag) = Untag (optExp e) tag
        optExp (CompareTagged cOp e1 e2) = CompareTagged cOp (optExp e1) (optExp e2)
        optExp e@(Amp fName) = e
        optExp (Call sz fName args) = Call sz fName (map optExp args)
        optExp (CallPtr sz e args) =
          case (optExp e) of
            (Amp fName) -> Call sz fName (map optExp args)
            _ -> CallPtr sz e (map optExp args)
        optExp e@(Alloc _) = e
        optExp (AllocArray eSz nE) = AllocArray eSz (optExp nE)
        optExp (Index sz esz arrE idxE o) = Index sz esz (optExp arrE) (optExp idxE) o
        optExp (Star sz addrE o) = Star sz (optExp addrE) o

        {-
          Tries to perform constant folding on the given binary operator and two operands, and
          returns the resulting expression.
        -}
        tryFoldBinop :: Binop -> Exp -> Exp -> Exp
        tryFoldBinop op@(ArithOp aOp) e1 e2 =
          case (aOp, e1, e2) of
            (Common.AAdd, NumLit n1, NumLit n2) -> NumLit (n1 + n2)
            (Common.AAdd, NumLit 0, _) -> e2
            (Common.AAdd, _, NumLit 0) -> e1
            (Common.ASub, NumLit n1, NumLit n2) -> NumLit (n1 - n2)
            (Common.ASub, _, NumLit 0) -> e1
            (Common.AMul, NumLit n1, NumLit n2) -> NumLit (n1 * n2)
            (Common.AMul, NumLit 1, _) -> e2
            (Common.AMul, _, NumLit 1) -> e1
            (Common.ADiv, NumLit n1, NumLit n2) ->
              if (n2 /= 0 && (n2 /= (-1) || n1 /= minInt))
              then NumLit (n1 `quot` n2)
              else Binop op e1 e2
            (Common.ADiv, _, NumLit 1) -> e1
            (Common.AMod, NumLit n1, NumLit n2) ->
              if (n2 /= 0 && (n2 /= (-1) || n1 /= minInt))
              then NumLit (n1 `rem` n2)
              else Binop op e1 e2
            (Common.AAnd, NumLit n1, NumLit n2) -> NumLit (n1 .&. n2)
            (Common.AAnd, NumLit (-1), _) -> e2
            (Common.AAnd, _, NumLit (-1)) -> e1
            (Common.AOr, NumLit n1, NumLit n2) -> NumLit (n1 .|. n2)
            (Common.AOr, NumLit 0, _) -> e2
            (Common.AOr, _, NumLit 0) -> e1
            (Common.AXor, NumLit n1, NumLit n2) -> NumLit (n1 `xor` n2)
            (Common.AXor, NumLit 0, _) -> e2
            (Common.AXor, _, NumLit 0) -> e1
            (Common.AShL, NumLit n1, NumLit n2) ->
              if (n2 >= 0 && n2 < 32)
              then NumLit (n1 `shiftL` (fromIntegral n2))
              else Binop op e1 e2
            (Common.AShL, _, NumLit 0) -> e1
            (Common.AShR, NumLit n1, NumLit n2) ->
              if (n2 >= 0 && n2 < 32)
              then NumLit (n1 `shiftR` (fromIntegral n2))
              else Binop op e1 e2
            (Common.AShR, _, NumLit 0) -> e1
            _ -> Binop op e1 e2
        tryFoldBinop op@(CmpOp (Common.CmpOp cmp)) e1 e2 =
          case (cmp, e1, e2) of
            (Common.L, NumLit n1, NumLit n2) -> BoolLit (n1 < n2)
            (Common.LE, NumLit n1, NumLit n2) -> BoolLit (n1 <= n2)
            (Common.G, NumLit n1, NumLit n2) -> BoolLit (n1 > n2)
            (Common.GE, NumLit n1, NumLit n2) -> BoolLit (n1 >= n2)
            (Common.E, _, _) ->
              case (tryPolyEQ e1 e2) of
                Nothing -> Binop op e1 e2
                Just True -> BoolLit True
                Just False -> BoolLit False
            (Common.NE, _, _) ->
              case (tryPolyEQ e1 e2) of
                Nothing -> Binop op e1 e2
                Just True -> BoolLit False
                Just False -> BoolLit True
            _ -> Binop op e1 e2
        tryFoldBinop op@(LogOp lOp) e1 e2 =
          case (lOp, e1, e2) of
            (Common.LAnd, BoolLit True, _) -> e2
            (Common.LAnd, BoolLit False, _) -> BoolLit False
            (Common.LAnd, _, BoolLit True) -> e1
            (Common.LOr, BoolLit True, _) -> BoolLit True
            (Common.LOr, BoolLit False, _) -> e2
            (Common.LOr, _, BoolLit False) -> e1
            _ -> Binop op e1 e2

        {-
          Compares two expressions for equality, returning Just (answer) if they're non-nested
          and so can be compared, and Nothing otherwise.
        -}
        tryPolyEQ :: Exp -> Exp -> Maybe Bool
        tryPolyEQ (NumLit n1) (NumLit n2) = Just (n1 == n2)
        tryPolyEQ (BoolLit b1) (BoolLit b2) = Just (b1 == b2)
        tryPolyEQ (CharLit c1) (CharLit c2) = Just (c1 == c2)
        tryPolyEQ (Null) (Null) = Just True
        tryPolyEQ (Ident v1) (Ident v2) = (if v1 == v2 then Just True else Nothing)
        tryPolyEQ _ _ = Nothing

        {-
          TODO: move the following to IRT.
          {-
            Woo, 213 party!
          -}
            case (tryFoldBinop op (optExp e1) (optExp e2)) of
              e'@(Binop (ArithOp Common.AMod) e1' (NumLit n)) ->
                if (isPow2 n)
                then Binop (ArithOp Common.AAnd) e1' (NumLit (n - 1))
                else e'
              e'@(Binop (ArithOp Common.AMul) e1' (NumLit n)) ->
                case (getPow2 n) of
                  (Just pow) -> Binop (ArithOp Common.AShL) e1' (NumLit pow)
                  Nothing -> e'
              e' -> e'
          isPow2 :: Int32 -> Bool
          isPow2 n = (n /= 0) && (n .&. (n - 1) == 0)

          getPow2 :: Int32 -> Maybe Int32
          getPow2 n =
            if (isPow2 n)
            then Just (countBits (n `xor` (n - 1)) - 1)
            else Nothing

          countBits :: Int32 -> Int32
          countBits n =
            let n' = ((n `shiftR` 1) .&. 0x55555555) + (n .&. 0x55555555)
                n'' = ((n' `shiftR` 2) .&. 0x33333333) + (n' .&. 0x33333333)
                n''' = ((n'' `shiftR` 4) .&. 0x0f0f0f0f) + (n'' .&. 0x0f0f0f0f)
                n'''' = ((n''' `shiftR` 8) .&. 0x00ff00ff) + (n''' .&. 0x00ff00ff)
                n''''' = ((n'''' `shiftR` 16) .&. 0x0000ffff) + (n'''' .&. 0x0000ffff)
            in n'''''
        -}

        minInt :: Int32
        minInt = minBound
