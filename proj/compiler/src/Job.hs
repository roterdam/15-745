{-
   Author: Matthew Maurer <mmaurer@andrew.cmu.edu>
   Modified by: Ryan Pearl <rpearl@andrew.cmu.edu>

   Defines a compiler phase or job
-}
module Job where

data Job = Job {
  src :: FilePath,
  dest :: FilePath,
  lib :: Maybe FilePath,
  safe :: Bool,
  lvl :: Integer,
  fmt :: OF,
  verb :: Bool }
  deriving Show

-- Output format.
data OF = C0 | O_AST | AST | C
          deriving (Eq, Show)

defaultJob :: Job
defaultJob = Job {
  src   = "",
  dest  = "",
  lib   = Nothing,
  safe  = True,
  lvl   = 2,
  fmt   = C,
  verb  = False }
