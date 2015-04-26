{-
   This file contains code to compile a Job.
-}
module Compile (compile, Job(..), defaultJob, OF(..)) where

import Job
import Error (liftEither)
import Compile.Parse.Parse (parseFiles, readFiles)
import Compile.Trans.GenAST (ptToAST)
import Compile.Trans.GenCTree (astToCTree)
import Compile.Trans.Check (check)
import Compile.Trans.Compress (compress)

compile :: Job -> IO ()
compile job = do
  files <- readFiles job
  pt <- liftEither job $ parseFiles job files
  let ast = ptToAST job pt
  gs <- liftEither job $ check ast
  let ast' = compress ast
  case fmt job of
    C0 ->  writeResult pt
    O_AST -> writeResult ast -- Before compression
    AST -> writeResult ast'  -- After compression
    C -> writeResult $ astToCTree gs ast'
  where writeResult :: (Show s) => s -> IO ()
        writeResult = writeFile (dest job) . show
