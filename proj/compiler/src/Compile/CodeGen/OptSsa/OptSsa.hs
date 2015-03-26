{-
  The main file for SSA optimization.
-}

module Compile.CodeGen.OptSsa.OptSsa where

import qualified Compile.CodeGen.OptSsa.Minimize as Minimize
import qualified Compile.CodeGen.OptSsa.Propogate as Propogate
import qualified Compile.CodeGen.OptSsa.Cleanup as Cleanup
import qualified Compile.CodeGen.OptSsa.Inline as Inline
import qualified Compile.CodeGen.OptSsa.Cfg as Cfg

import Compile.Types.Common (TmpIdent, BlockIdent)
import Compile.Types.S3Asm
import qualified Job

import Debug.Trace

{-
  Optimizes the given 3-argument assembly.
  There are a lot of optimizations either here or in the works, so it's worth talking about how
  they're split into groups:
  MINIMIZATION -> ssa minimization
  PROPOGATION -> constant propogation/folding, copy propogation, CSE, arithmetic optimization
  CLEANUP -> dead code/block/block param elimination, extra check/write elimination
  INLINING -> function inlining
  CFG SIMPLIFICATION -> block merging/skipping, return lifting, turning cjmps into jmps

  The groups of optimizations are scheduled as follows:
  1. do {
        run min, prop, post-prop to saturation (but run all at least once)
        run inlining
     } while (inlining did something)
  2. Run CFG simplification.

  TODO:
    implement everything
    see if things can return anything more useful than a Bool
-}
optSsa :: Job.Job -> S3Asm -> S3Asm
optSsa job (Prog fns)
  | (Job.lvl job < 2) = Prog fns
  | otherwise = Prog ({-map simplifyCfg $-} optimizeRec fns)
    where optimizeRec :: [Fn] -> [Fn]
          optimizeRec fns =
            let fns' = map simplifyFn fns
                (fns'', didSomething) = Inline.inline fns'
            in (if (didSomething)
                then optimizeRec fns''
                else fns'')

          simplifyCfg :: Fn -> Fn
          simplifyCfg (Fn name params bMap) = Fn name params (Cfg.simplifyCfg bMap)

          {-
            Runs minimization, propogation, and cleanup to saturation on the
            given function, being sure to run all three at least once.
          -}
          simplifyFn :: Fn -> Fn
          simplifyFn (Fn name params bMap) =
            let (bMap', _) = Minimize.minimize bMap
                (bMap'', _) = Propogate.propogate bMap'
                bMap''' = Cleanup.cleanup bMap''
            in Fn name params (simplifyRec bMap''' 1)

          simplifyRec :: BlockMap -> Integer -> BlockMap
          simplifyRec bMap currRound
            | currRound >= maxSimplifyRounds = bMap
            | otherwise =
              let (bMap', _) = Minimize.minimize bMap
                  (bMap'', didSomething) = Propogate.propogate bMap'
                  bMap''' = Cleanup.cleanup bMap''
              in (if didSomething
                  then simplifyRec bMap''' (currRound + 1)
                  else bMap''')

          maxSimplifyRounds :: Integer
          maxSimplifyRounds = 10
