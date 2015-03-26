{-
	Contains common types used by all the parser modules.
-}

module Compile.Parse.Common where

import Text.Parsec.Prim (ParsecT)
import qualified Data.Set as Set
import Control.Monad.Identity (Identity)
import qualified Data.ByteString.Char8 as BS
import Text.Parsec.Prim (Stream(..))

-- The stream type our parsers will use.
type StreamT = BS.ByteString

-- The state type our parsers will use: a set of defined type identifiers.
data StateT = StateT {
	typedefs :: Set.Set String,
	sigdefs :: Set.Set String }

-- A fresh state.
newState :: StateT
newState = StateT {typedefs = Set.empty, sigdefs = Set.empty}
addTypeDef :: String -> StateT -> StateT
addTypeDef typedef state = state {typedefs = Set.insert typedef $ typedefs state}
addSigDef :: String -> StateT -> StateT
addSigDef sigdef state = state {sigdefs = Set.insert sigdef $ sigdefs state}

-- The monad type our parsers will use.
type MonadT = Identity

-- A Parser contains a set of defined types.
type Parser = ParsecT StreamT StateT Identity

-- NOTE: This is necessary to use some of the parsers in Text.Parsec.Char with our Parser type.
instance (Monad m) => Stream StreamT m Char where
  uncons = return . BS.uncons
