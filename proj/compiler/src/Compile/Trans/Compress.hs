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
-- function does a backwards flow and so is the output
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
-- TODO: Because of the way this is structures, the decls right now only
-- allow compressing stuff within the same scope. There is no movement
-- of stuff across scopes of variables. HOW TO FIX THAT :((
compressStmts (stmt@(Decl t i Nothing scope):prevStmts) =
  (Decl t i Nothing (compressStmts' scope)):compressStmts prevStmts
compressStmts ((Decl t i (Just e) scope):prevStmts) =
  compressStmts $ (Decl t i Nothing $
                  (Assn Common.Set (LIdent i) e):scope):prevStmts
compressStmts (stmt@(Assert e):prevStmts) =
   stmt:(compressStmts $ compressExp e prevStmts)
compressStmts (stmt@(Exp e):prevStmts) =
   stmt:(compressStmts $ compressExp e prevStmts)

-- Looks at an expression and then tells you what possible sequences
-- you can apply lazy code motion on.
-- For eg
--    map f (map f' S) ==> [S]
--    reduce f (reduce f' b S) S' ==> [S, S']
-- NOTE: We try with all idents, but only the ones which have type
-- seq, and therefore appear in seq operations, will change the AST.
-- We know this to be true since this stage runs after typechecking
dependentSeqs :: Exp -> [Common.Ident]
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
   foldl lazyCodeMotionWrapper stmts [i]
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
      res = foldl lazyCodeMotionWrapper stmts potentialSeqs
      stmts' = if length potentialSeqs == 0
                  then stmts
                  else res
    in
       stmts'

lazyCodeMotionWrapper :: [Stmt] -> Common.Ident -> [Stmt]
lazyCodeMotionWrapper stmts s =
  let (res, _) = lazyCodeMotion ([], stmts) s
  in res

-- This function is the one which does the actual code motion downwards
-- Input stmt list is in program order
lazyCodeMotion' :: Stmt -> [Stmt] -> [Stmt]
lazyCodeMotion' s [] = [s]
-- We can move down past assignments asserts
lazyCodeMotion' stmt@(Assn _ (LIdent l) _)
                (nstmt@(Assn _ l' e):afterStmts) =
  if (l `elem` dependentSeqs e)
    then stmt:nstmt:afterStmts
    else nstmt:(lazyCodeMotion' stmt afterStmts)
lazyCodeMotion' stmt@(Assn _ (LIdent l) _)
                (nstmt@(Decl t i Nothing scope):afterStmts) =
  (Decl t i Nothing (lazyCodeMotion' stmt scope)):afterStmts
lazyCodeMotion' stmt@(Assn _ (LIdent l) _)
                (nstmt@(Decl t i (Just e) scope): afterStmts) =
 if (l `elem` dependentSeqs e)
    then stmt:nstmt:afterStmts
    else (Decl t i (Just e) (lazyCodeMotion' stmt scope)):afterStmts
lazyCodeMotion' stmt@(Assn _ (LIdent l) _) (nstmt@(Assert e):afterStmts) =
 if (l `elem` dependentSeqs e)
    then stmt:nstmt:afterStmts
    else nstmt:(lazyCodeMotion' stmt afterStmts)
-- We don't want to move into if statements, while loops or past expression
-- which might be function calls/memory changes
lazyCodeMotion' stmt afterStmts@_ =
    stmt:afterStmts

-- Purpose of function: To move the operations associated with the
-- sequence specified as far down as possible.
-- afterStmts refers to stmts that occur after the one we are about to
-- process in the actual program - in this function, it refers to
-- statements we have already processed while traversing the
-- program backwards

-- NOTE: Input stmt lists are reversed, as are outputs.
lazyCodeMotion :: ([Stmt],[Stmt]) -> Common.Ident -> ([Stmt],[Stmt])
lazyCodeMotion (afterStmts, []) _ = (afterStmts,[])
lazyCodeMotion (afterStmts, stmt@(Assn op (LIdent l) e):prevStmts) s =
  if (l == s && (isSeqOperator e))
    then
      if (isReduceOp e)
        then
          let
           -- 2. We try to move the seq that the reduce op was on,
           -- as far down as possible but only up till before this
           -- stmt
              prevStmts' = compressExp e prevStmts
           -- 1. We keep stmt where it is in the program order.
              res = (afterStmts ++ [stmt], prevStmts')
          in lazyCodeMotion res s
        else
          let
           -- 1. We want to move stmt further down somewhere in
           -- afterStmts
             afterStmts' = reverse $ lazyCodeMotion' stmt $ reverse afterStmts
           -- 2. We want to move dependent seqs as far down as possible
           -- but we are considering the full block of statements
             afterStmts'' = compressExp e (afterStmts' ++ prevStmts)
             res = (afterStmts'', [])
          in
            (afterStmts'', [])
    else
      lazyCodeMotion (afterStmts ++ [stmt], prevStmts) s
lazyCodeMotion (afterStmts, stmt@(Decl t i Nothing scope):prevStmts) s =
  let
    (scope', _) = lazyCodeMotion ([], reverse scope) s
  in
    lazyCodeMotion (afterStmts ++ [Decl t i Nothing (reverse scope')], prevStmts) s
-- Pass through all other cases
lazyCodeMotion (afterStmts, stmt:prevStmts) s =
  lazyCodeMotion (afterStmts ++ [stmt], prevStmts) s

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
