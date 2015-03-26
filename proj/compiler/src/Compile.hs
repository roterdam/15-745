{-
   This file contains code to compile a Job.
-}
module Compile (compile, Job(..), defaultJob, OF(..)) where

import Job
import Error (liftEither)
import Compile.Parse.Parse (parseFiles, readFiles)
import Compile.Trees.GenAST (ptToAST)
import Compile.Trees.Check (check)
import Compile.Trees.GenPCT (astToPCT)
import Compile.Trees.OptPCT (optPCT)
import Compile.Trees.GenIRT (pctToIRT)
import Compile.Trees.OptIRT (optIRT)
import Compile.CodeGen.GenThree (irtToThree)
import Compile.CodeGen.GenSsa (threeToSsa)
import Compile.CodeGen.OptSsa.OptSsa (optSsa)
import Compile.CodeGen.GenTwo (ssaToTwo)
import Compile.CodeGen.GenX86.GenX86 (twoToX86)


-- TODO: doing things partially is a little broken here, since we compute all state before casing.
-- For example, if check fails, and we only want pt, we'll still try to compute check and fail on it.
compile :: Job -> IO ()
compile job = do
  files <- readFiles job
  pt <- liftEither job $ parseFiles job files
  let ast = ptToAST job pt
  gs <- liftEither job $ check ast
  case fmt job of
    C0 ->       writeResult pt
    AST ->      writeResult ast
    PCT ->      writeResult $ toPCT gs ast
    OptPCT ->   writeResult $ toPCT' gs ast
    IRT ->      writeResult $ toIRT gs ast
    OptIRT ->   writeResult $ toIRT' gs ast
    B3Asm ->    writeResult $ toThree gs ast
    SSA3Asm ->  writeResult $ toSsa gs ast
    OptSsa ->   writeResult $ toSsa' gs ast
    B2Asm ->    writeResult $ toTwo gs ast
    XAsm ->     writeResult $ toX86 gs ast
  where writeResult :: (Show s) => s -> IO ()
        writeResult = writeFile (dest job) . show

        -- Result-type helpers.
        toPCT gs ast = astToPCT job gs ast
        toPCT' gs ast = optPCT job $ toPCT gs ast
        toIRT gs ast = pctToIRT job $ toPCT' gs ast
        toIRT' gs ast = optIRT job $ toIRT gs ast
        toThree gs ast = irtToThree job $ toIRT' gs ast
        toSsa gs ast = threeToSsa job $ toThree gs ast
        toSsa' gs ast = optSsa job $ toSsa gs ast
        toTwo gs ast = ssaToTwo job $ toSsa' gs ast
        toX86 gs ast = twoToX86 job $ toTwo gs ast
