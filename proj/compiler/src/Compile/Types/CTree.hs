{-
  CTree is an AST that corresponds to, and is convertible to, C code, as
  compared to the parseTree and previous AST which corresponded to C0.
-}

module Compile.Types.CTree where

{- TODO: implement -}
data CTree = Prog

instance Show CTree where
  show Prog = "-unimplemented-"
