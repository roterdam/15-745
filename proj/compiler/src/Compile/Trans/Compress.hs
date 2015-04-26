module Compile.Trans.Compress (compress) where

import qualified Compile.Types.Common as Common
import Compile.Types.AST
import Compile.Trans.CheckState
import Data.List
import Debug.Trace (trace)

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
compressStmts' body = trace ("Reversed: " ++ (show $ reverse body))
                      reverse $ compressStmts $ reverse body

-- Compress a function body. For each statement, create
-- a list of possible identifiers denoting sequences. For each
-- possible sequence and its dependent sequence, try to pull it down
-- as far as possible within its BASIC BLOCK
-- NOTE: the input is the reverse of the function body
compressStmts :: [Stmt] -> [Stmt]
compressStmts [] = []
compressStmts (stmt@(Assn _ _ e):prevStmts) = trace ("Assn: " ++ show stmt)
  stmt:(compressStmts $ compressExp e prevStmts)
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

extractSeqToCompress :: Exp -> [Common.Ident]
-- NOTE: We try with all idents, but only the ones which have type
-- seq, and therefore appear in seq operations, will change the AST.
-- We know this to be true since this stage runs after typechecking
extractSeqToCompress (Ident i) = [i]
extractSeqToCompress (Binop _ e1 e2) =
    nub $ extractSeqToCompress e1 ++ extractSeqToCompress e2
extractSeqToCompress (Unop _ e) = extractSeqToCompress e
extractSeqToCompress (Cond pred e1 e2) =
    nub $ extractSeqToCompress pred ++ extractSeqToCompress e1 ++
        extractSeqToCompress e2
extractSeqToCompress (Call f args) =
    nub $ concatMap extractSeqToCompress args
extractSeqToCompress (AllocArray _ e) = extractSeqToCompress e
extractSeqToCompress (Index _ i) = extractSeqToCompress i
extractSeqToCompress (Tabulate f n) =
    nub $ extractSeqToCompress f ++ extractSeqToCompress n
extractSeqToCompress (ListSeq args) =
    nub $ concatMap extractSeqToCompress args
extractSeqToCompress (RangeSeq start end) =
    nub $ extractSeqToCompress start ++ extractSeqToCompress end
extractSeqToCompress (Map _ s) = extractSeqToCompress s
extractSeqToCompress (Filter _ s) = extractSeqToCompress s
extractSeqToCompress (Combine _ s1 s2) =
    nub $ extractSeqToCompress s1 ++ extractSeqToCompress s2
extractSeqToCompress (Reduce _ b s) =
    nub $ extractSeqToCompress b ++ extractSeqToCompress s
extractSeqToCompress _ = []

-- Extracts identifiers from an expression and try to move any
-- seq operation as down as possible
-- exp: expression from which to extract all the potential seq
-- identifiers that can be compressed
-- stmts: prevStmts before the stmt with exp
compressExp :: Exp -> [Stmt] -> [Stmt]
compressExp e stmts =
    let
      potentialSeqs = extractSeqToCompress e
      (res, _) = (foldl lazyCodeMotion ([], stmts) potentialSeqs)
    in if length potentialSeqs == 0
          then stmts
          else res

isSeqOperator :: Exp -> Bool
isSeqOperator (Tabulate _ _) = True
isSeqOperator (ListSeq _) = True
isSeqOperator (RangeSeq _ _) = True
isSeqOperator (Map _ _) = True
isSeqOperator (Filter _ _) = True
isSeqOperator (Combine _ _ _) = True
isSeqOperator (Reduce _ _ _) = True
isSeqOperator _ = False

-- Purpose of function: To move the operations associated with the
-- sequence specified as far down as possible.
-- NOTE: Input stmt lists are reversed, as are outputs
lazyCodeMotion :: ([Stmt],[Stmt]) -> Common.Ident -> ([Stmt],[Stmt])
-- Done processing block of statements
lazyCodeMotion (afterStmts, []) _ = (afterStmts, [])
lazyCodeMotion (afterStmts, stmt@(Assn op (LIdent l) e):prevStmts) s =
    if (l == s && (isSeqOperator e))
    -- We found the ident we were looking for and it is a seq
    then
        (let dependentSeqs = extractSeqToCompress e
             (afterStmts', prevStmts') =
              foldl lazyCodeMotion (afterStmts, prevStmts) dependentSeqs
         in (stmt:afterStmts', prevStmts'))
    else lazyCodeMotion (afterStmts ++ [stmt], prevStmts) s
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

lazyCodeMotion (a = 1+3,B = map f (map f' S), ... S = <..>) B
-}
