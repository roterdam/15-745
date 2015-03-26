{-
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>
                Connor Brem <cbrem@andrew.cmu.edu>

   the entry point to the compiler
-}
import Compile
import Args
import System.Environment
import System.IO
import System.Exit

main :: IO ()
main = do
  prog <- getEnv "COMPILER"
  args <- getArgs
  case parseArgs args of
    Left  err -> do
      let msg = "Compilation failed with error: " ++ err ++ "\n" ++ usage prog
      hPutStr stderr msg
      exitFailure
    Right job -> compile job
  exitSuccess
