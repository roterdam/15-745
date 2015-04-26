module Compile.Trans.Compress (compress) where

import qualified Compile.Types.Common as Common
import Compile.Types.AST
import Compile.Trans.CheckState
import Data.List
import Debug.Trace (trace, traceShow)

-- Compress an AST
compress :: AST -> AST
compress (Prog gdecls) = Prog (map compressGDecl gdecls)

-- Compress all GDecls
compressGDecl :: GDecl -> GDecl
compressGDecl (FDefn t n params body) =
  FDefn t n params (compressStmts' body)
compressGDecl rest = rest

-- Operates on the non-reversed list and returns a non-reversed list
compressStmts' :: [Stmt] -> [Stmt]
compressStmts' body = reverse $ compressStmts $ reverse body

-- Compress a function body. For each statement, create
-- a list of possible identifiers denoting sequences. For each
-- possible sequence and its dependent sequence, try to pull it down
-- as far as possible
-- NOTE: the input is the reverse of the function body since this
-- function does a backwards flow
compressStmts :: [Stmt] -> [Stmt]
compressStmts [] = []
compressStmts (stmt@(Assn _ l e):prevStmts) =
  stmt:(compressStmts $ compressLValue l $ compressExp e prevStmts)
compressStmts (stmt@(If c s1 s2):prevStmts) =
  (If c (compressStmts' s1) (compressStmts' s2)):
  (compressStmts $ compressExp c prevStmts)
compressStmts (stmt@(While e s):prevStmts) =
   (While e (compressStmts' s)):
   (compressStmts $ compressExp e prevStmts)
compressStmts (stmt@(Return Nothing):prevStmts) =
  stmt:(compressStmts prevStmts)
compressStmts (stmt@(Return (Just e)):prevStmts) =
  stmt:(compressStmts $ compressExp e prevStmts)
compressStmts (stmt@(Decl t i Nothing scope):prevStmts) =
  (Decl t i Nothing (compressStmts' scope)):(compressStmts prevStmts)
compressStmts ((Decl t i (Just e) scope):prevStmts) =
  (Decl t i (Just e) (compressStmts' scope)):
  (compressStmts $ compressExp e prevStmts)
compressStmts (stmt@(Assert e):prevStmts) =
   stmt:(compressStmts $ compressExp e prevStmts)
compressStmts (stmt@(Exp e):prevStmts) =
   stmt:(compressStmts $ compressExp e prevStmts)

-- Looks at an expression and then tells you what possible sequences
-- you can apply lazy code motion on.
-- For eg
--    map f (map f' S) = [S]
--    reduce f (reduce f' b S) S' = [S, S']

dependentSeqs :: Exp -> [Common.Ident]
-- NOTE: We try with all idents, but only the ones which have type
-- seq, and therefore appear in seq operations, will change the AST.
-- We know this to be true since this stage runs after typechecking
dependentSeqs (Ident i) = [i]
dependentSeqs (Binop _ e1 e2) =
    nub $ dependentSeqs e1 ++ dependentSeqs e2
dependentSeqs (Unop _ e) = dependentSeqs e
dependentSeqs (Cond pred e1 e2) =
    nub $ dependentSeqs pred ++ dependentSeqs e1 ++
        dependentSeqs e2
dependentSeqs (Call f args) =
    nub $ concatMap dependentSeqs args
dependentSeqs (AllocArray _ e) = dependentSeqs e
dependentSeqs (Index _ i) = dependentSeqs i
dependentSeqs (Tabulate _ n) = nub $ dependentSeqs n
dependentSeqs (ListSeq args) =
    nub $ concatMap dependentSeqs args
dependentSeqs (RangeSeq start end) =
    nub $ dependentSeqs start ++ dependentSeqs end
dependentSeqs (Map _ s) = dependentSeqs s
dependentSeqs (Filter _ s) = dependentSeqs s
dependentSeqs (Combine _ s1 s2) =
    nub $ dependentSeqs s1 ++ dependentSeqs s2
dependentSeqs (Reduce _ b s) =
    nub $ dependentSeqs b ++ dependentSeqs s
dependentSeqs _ = []

isSeqOperator :: Exp -> Bool
isSeqOperator (Tabulate _ _) = True
isSeqOperator (ListSeq _) = True
isSeqOperator (RangeSeq _ _) = True
isSeqOperator (Map _ _) = True
isSeqOperator (Filter _ _) = True
isSeqOperator (Combine _ _ _) = True
isSeqOperator (Reduce _ _ _) = True
isSeqOperator _ = False

isReduceOp :: Exp -> Bool
isReduceOp (Reduce _ _ _) = True
isReduceOp _ = False


compressLValue :: LValue -> [Stmt] -> [Stmt]
compressLValue (LIdent i) stmts =
    let (res, _) = foldl lazyCodeMotion ([], stmts) [i]
    in res
compressLValue (LIndex l e) stmts =
    compressLValue l $ compressExp e stmts
compressLValue _ stmts = stmts

-- Extracts identifiers from an expression and try to move any
-- seq operation as down as possible
-- exp: expression from which to extract all the potential seq
-- identifiers that can be compressed
-- stmts: Stmts appearing before the stmt with exp, in program order
compressExp :: Exp -> [Stmt] -> [Stmt]
compressExp e stmts =
    let
      potentialSeqs = dependentSeqs e
      (res, _) = foldl lazyCodeMotion ([], stmts) potentialSeqs
      stmts' = if length potentialSeqs == 0
                  then stmts
                  else res
    in stmts'

-- We only move down assgnment statements
-- NOTE: The [Stmt] given is in program order
lazyCodeMotion' :: Stmt -> [Stmt] -> [Stmt]
lazyCodeMotion' s [] = [s]
-- We can move down past assignments
lazyCodeMotion' stmt@(Assn _ (LIdent l) _) (nstmt@(Assn _ l' e):afterStmts) =
  if (l `elem` (dependentSeqs e))
    then stmt:nstmt:afterStmts
    else nstmt:(lazyCodeMotion' stmt afterStmts)
-- We don't want to move into if statements, while loops
lazyCodeMotion' stmt@(Assn _ (LIdent l) _) (nstmt@(Decl t i Nothing scope):
                                   afterStmts) =
  nstmt:(lazyCodeMotion' stmt scope) ++ afterStmts
lazyCodeMotion' stmt@(Assn _ (LIdent l) _) (nstmt@(Decl t i (Just e) scope):
                                   afterStmts) =
 if (l `elem` dependentSeqs e)
    then stmt:nstmt:afterStmts
    else nstmt:(lazyCodeMotion' stmt scope) ++ afterStmts
lazyCodeMotion' stmt@(Assn _ (LIdent l) _) (nstmt@(Assert e):afterStmts) =
 if (l `elem` dependentSeqs e)
    then stmt:nstmt:afterStmts
    else nstmt:(lazyCodeMotion' stmt afterStmts)
lazyCodeMotion' stmt (nstmt@(Exp e):afterStmts) =
    stmt:nstmt:afterStmts

-- Purpose of function: To move the operations associated with the
-- sequence specified as far down as possible.
-- afterStmts refers to stmts that occur after the one we are about to
-- process in the actual program.
-- NOTE: Input stmt lists are reversed, as are outputs.
lazyCodeMotion :: ([Stmt],[Stmt]) -> Common.Ident -> ([Stmt],[Stmt])
-- Done processing block of statements
lazyCodeMotion (afterStmts, []) _ = (afterStmts, [])
lazyCodeMotion (afterStmts, stmt@(Assn op (LIdent l) e):prevStmts) s =
  trace ("-----------------\n" ++
         "afterStmts: " ++ show (reverse afterStmts) ++
         "\nident: " ++ s ++ ", stmt: " ++ show stmt ++
         "prevStmts: " ++ show (reverse prevStmts))
  (if (l == s && (isSeqOperator e))
    then
      (if (isReduceOp e)
         then
            let
             -- 2. We try to move the seq that the reduce op was
             -- based on, as far down as possible
                prevStmts' = compressExp e prevStmts
             -- 1. We keep stmt where it is in the program order.
            in trace ("prevStmts': " ++ show (reverse prevStmts'))
                      (afterStmts ++ [stmt], prevStmts')
         else
            let
             -- 1. We want to move stmt further down somewhere in afterStmts
               afterStmts' = reverse $ lazyCodeMotion' stmt $ reverse afterStmts
             -- 2. We want to move dependent seqs as far down as possible
               afterStmts'' = compressExp e (afterStmts' ++ prevStmts)
            in trace ("\nafterStmts': " ++ show (reverse afterStmts') ++
                       "\nafterStmts'': " ++ show (reverse afterStmts''))
                (afterStmts'', []))
    else lazyCodeMotion (afterStmts ++ [stmt], prevStmts) s)
lazyCodeMotion (afterStmts, stmt@(Decl t i Nothing scope):prevStmts) s =
  let
    (scope', _) = lazyCodeMotion ([], reverse scope) s
  in
    (afterStmts ++ [Decl t i Nothing (reverse scope')], prevStmts)
-- Ignore all other cases
lazyCodeMotion input _ = input


{-
Example 1:
Original program:
[ R = tabulate g n,
  S = map f' R,
  non-seq-stuff,
  A = reduce f b S]

lazyCodeMotion (A = reduce f b S)::prevStmts@[non-seq-stuff,(S = map f' R),(R = tabulate g n)..] S
--> (A = reduce f S)::(lazyCodeMotion prevStmts S)
--> (A = reduce f S)::(S = map f' R)::(lazyCodeMotion
                                      ([non-seq-stuff]++ prevStmts') R)
--> ...

Example 2:
S = <...>
S' = map g S
.
.
.
A = reduce f (reduce or false S) S'
B = map f (map f' S')

lazyCodeMotion (B = map f (map f' S'))::(..., S' = map g S, S = <...>) S'
--> (B = map f (map f' S'))::(S' = map g S)::lazyCodeMotion (..., S = <...>) S

Example 3:
S = <...>
.
.
B = map f (map f' S)
a = 1+3;

Example 4:
int x;
int<> A = tabulate f n
x = 1;
return reduce f b A;
-}
