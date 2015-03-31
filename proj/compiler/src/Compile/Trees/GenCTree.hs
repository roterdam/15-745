{-
  Converts ASTs into CTrees.
-}

module Compile.Trees.GenCTree where

import qualified Compile.Types.Common as Common
import qualified Compile.Types.AST as AST
import qualified Compile.Types.CTree as CTree
import qualified Job as Job

astToCTree :: Job.Job -> AST.AST -> CTree.CTree
astToCTree _ _ = CTree.Prog
