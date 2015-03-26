{-
  Handles function inlining.
-}

module Compile.CodeGen.OptSsa.Inline (inline) where

import Compile.Types.S3Asm

inline :: [Fn] -> ([Fn], Bool)
inline fns = (fns, False)
