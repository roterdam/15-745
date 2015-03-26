{-
  This file contains the top-level function for parsing a job.
-}

module Compile.Parse.Parse (readFiles, parseFiles) where

import Job
import Error (liftEither)
import Compile.Types.ParseTree
import Compile.Parse.Prog (parseProg)
import Compile.Parse.Common (StateT, Parser, StreamT, newState)

import Control.Monad.Error (throwError)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8 as BS
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim (runParser, getState)

type ParseMonad = Either String

-- Given a Job, reads its src file and its lib file (if it exists).
readFiles :: Job -> IO (StreamT, Maybe StreamT)
readFiles job = do
  srcFile <- BS.readFile $ src job
  maybeLibFile <- case lib job of
    Nothing -> return Nothing
    Just lib -> do
      libFile <- BS.readFile lib
      return $ Just libFile
  return (srcFile, maybeLibFile)

{-
  Parses the files from a Job:
    1.  If a library file exists, parses it.
    2.  Parses the source file.
    3.  If a library file is supplied, it checked for illegal constructs (like function defs).
        If there are none, all of its function declarations are tagged as 'external', and we
        concatenate and return the two ParseTrees.
-}
parseFiles :: Job -> (StreamT, Maybe StreamT) -> ParseMonad ParseTree
parseFiles job (srcFile, maybeLibFile) = do
  case maybeLibFile of
    Nothing -> do
      (jobTree, _) <- parseFile job srcFile newState
      return jobTree
    Just libFile -> do
      (libTree, libState) <- parseFile job libFile newState
      (srcTree, _) <- parseFile job srcFile libState
      concatTrees srcTree libTree

-- Combines ParseTrees from the source and library files,
-- transforming external GDecls as necessary.
concatTrees :: ParseTree -> ParseTree -> ParseMonad ParseTree
concatTrees (Prog srcGDecls) (Prog libGDecls) = do
  libGDecls' <- mapM transLibGDecl libGDecls
  return $ Prog $ libGDecls' ++ srcGDecls
  where -- Applies transformations to a GDecl from the library file.
        transLibGDecl :: GDecl -> ParseMonad GDecl
        transLibGDecl gDecl = do
          case gDecl of
            FDecl t ident params -> return $ FExt t ident params
            FDefn _ _ _ _ -> throwError $ "Found function in header file: " ++ show gDecl
            FExt _ _ _ -> error $ "Parsing produced FExt"
            _ -> return gDecl

-- Parses a file, given the filename. Returns the parsed program along with the final user state
-- (a set of all types defined in the program).
parseFile :: Job -> StreamT -> StateT -> ParseMonad (ParseTree, StateT)
parseFile job file state = liftEither job $ runParser parseProgWithState state "" file
  where -- Parses a program, and returns the parsed program along with its final user state.
        parseProgWithState :: Parser (ParseTree, StateT)
        parseProgWithState = do
          prog <- parseProg
          state <- getState
          return (prog, state)
