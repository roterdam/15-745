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
import Compile.Trans.Combine (combine)

compile :: Job -> IO ()
compile job = do
  -- parsing
  files <- readFiles job
  pt <- liftEither job $ parseFiles job files
  -- elaboration
  let ast = ptToAST job pt
  -- typechecking
  gs <- liftEither job $ check ast
  -- combining optimization
  let ast' = (if (lvl job > 0)
                 then combine ast
                 else ast)
  case fmt job of
    C0 ->  writeResult pt
    O_AST -> writeResult ast -- Before compression
    AST -> writeResult ast'  -- After compression
    C -> writeResult $ astToCTree ast'
  where writeResult :: (Show s) => s -> IO ()
        writeResult = writeFile (dest job) . show
