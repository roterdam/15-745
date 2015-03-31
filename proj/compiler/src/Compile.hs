{-
   This file contains code to compile a Job.
-}
module Compile (compile, Job(..), defaultJob, OF(..)) where

import Job
import Error (liftEither)
import Compile.Parse.Parse (parseFiles, readFiles)
import Compile.Trees.GenAST (ptToAST)
import Compile.Trees.GenCTree (astToCTree)
import Compile.Trees.Check (check)

compile :: Job -> IO ()
compile job = do
  files <- readFiles job
  pt <- liftEither job $ parseFiles job files
  let ast = ptToAST job pt
  gs <- liftEither job $ check ast
  case fmt job of
    C0 ->       writeResult pt
    AST ->      writeResult ast
    C ->        writeResult $ astToCTree job ast
  where writeResult :: (Show s) => s -> IO ()
        writeResult = writeFile (dest job) . show
