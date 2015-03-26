{-
  This file contains a parser for parsing a full ParseTree from a C0 source file,
  as well as parsers for parsing many other high-level constructs.

  The parsers in this file cover structures (in Compile.Types.ParseTree) from ParseTree down to
  Stmt sub-types.
-}

module Compile.Parse.Prog where


import Compile.Types.ParseTree
import qualified Compile.Types.Common as Common

import Compile.Parse.Tokens
import Compile.Parse.Helpers (parseLValue, parseExp, parseType)
import Compile.Parse.Common (Parser, addSigDef, addTypeDef)

import qualified Data.Set as Set

import Text.Parsec.Combinator (eof, optionMaybe, endBy, notFollowedBy)
import Text.Parsec.Prim (try, many, (<|>), (<?>), updateState, getPosition)

-- TODO: remove
import Debug.Trace (trace)
traceM msg = trace msg (return ())

{-
  TODO:
  * handle #use directives
-}

-- Parses an entire program into a ParseTree.
parseProg :: Parser ParseTree
parseProg = (do
  -- traceM "parseProg"
  whiteSpace
  gDecls <- many parseGDecl
  whiteSpace
  eof
  return $ Prog gDecls
  ) <?> "program"

-- Parses a global definition/declaration (function def, function decl, or typedef).
-- Note that we don't use ExDecl here, since the parser does not know whether its input is a source
-- or header file.
-- Also, note that we store a set of defined types encountered so far in our state. This is used to
-- resolve ambiguities when parsing types and expressions.
parseGDecl :: Parser GDecl
parseGDecl = (do
  reserved "typedef"
  t <- parseType
  ident <- identifier
  (do
    -- traceM "parseGDecl.Typedef"
    semi
    updateState $ addTypeDef ident
    return $ Typedef t ident
    ) <|> (do
    -- traceM "parseGDecl.Sigdef"
    params <- parens $ commaSep parseParam
    semi
    updateState $ addSigDef ident
    return $ Sigdef t ident params
    )
  ) <|> try (do
  reserved "struct"
  ident <- identifier
  (do
    -- traceM "parseGDecl.SDecl"
    semi
    return $ SDecl ident
    ) <|> (do
    -- traceM "parseGDecl.SDefn"
    fields <- braces $ endBy parseParam semi
    semi
    return $ SDefn ident fields
    )
  ) <|> (do
  t <- parseType
  ident <- identifier
  params <- parseParams
  (do
    -- traceM "parseGDecl.FDecl"
    semi
    return $ FDecl t ident params
    ) <|> (do
    -- traceM "parseGDecl.FDefn"
    block <- parseBlock
    return $ FDefn t ident params block
    )
  ) <?> "global"

-- Parses the param list for a function definition or declaration.
parseParams :: Parser [Common.Param]
parseParams = (do
  -- traceM "parseParams"
  params <- parens $ commaSep parseParam
  return params
  ) <?> "param list"

-- Parses a single function parameter.
parseParam :: Parser Common.Param
parseParam = (do
  -- traceM "parseParam"
  t <- parseType
  ident <- identifier
  return $ Common.Param t ident
  ) <?> "param"

-- Parses one brace-delimited block.
parseBlock :: Parser Block
parseBlock = braces (do
  -- traceM "parseBlock"
  stmts <- many parseStmt
  return $ Stmts stmts
  ) <?> "block"

-- Parses one Stmt.
parseStmt :: Parser Stmt
parseStmt = (do
  -- traceM "parseStmt.Simp"
  simp <- parseSimp
  semi
  return $ Simp simp
  ) <|> (do
  -- traceM "parseStmt.Ctrl"
  ctrl <- parseCtrl
  return $ Ctrl ctrl
  ) <|> (do
  -- traceM "parseStmt.Block"
  block <- parseBlock
  return $ Block block
  ) <?> "stmt"

-- Parses one simple (i.e. one-line) statement.
parseSimp :: Parser Simp
parseSimp = try (do
  -- traceM "parseSimp.Decl"
  t <- parseType
  ident <- identifier
  maybeE <- optionMaybe parseDef
  return $ Decl t ident maybeE
  ) <|> try (do
  -- traceM "parseSimp.Assn"
  lValue <- parseLValue
  asop <- parseAsop
  e <- parseExp
  return $ Assn asop lValue e
  ) <|> try (do
  -- traceM "parseSimp.Post"
  -- NOTE: We explicitly disallow statements of the form *<dest> ++ or *<dest> --.
  notFollowedBy $ reservedOp "*"
  lValue <- parseLValue
  postop <- parsePostop
  return $ Post lValue postop
  ) <|> (do
  -- traceM "parseSimp.Exp"
  e <- parseExp
  return $ Exp e
  ) <?> "simp"

-- Parses the definition portion of a declaration/definition.
parseDef :: Parser Exp
parseDef = (do
  -- traceM "parseDef"
  reservedOp "="
  e <- parseExp
  return e
  ) <?> "def"

-- Parses a control structure.
parseCtrl :: Parser Ctrl
parseCtrl = (do
  -- traceM "parseCtrl.If"
  reserved "if"
  e <- parens parseExp
  stmt <- parseStmt
  maybeElse <- optionMaybe parseElse
  return $ If e stmt maybeElse
  ) <|> (do
  -- traceM "parseCtrl.While"
  reserved "while"
  e <- parens parseExp
  stmt <- parseStmt
  return $ While e stmt
  ) <|> (do
  -- traceM "parseCtrl.For"
  reserved "for"
  (pre, cond, post) <- parens (do
    pre <- optionMaybe parseSimp
    semi
    cond <- parseExp
    semi
    post <- optionMaybe parseSimp
    return (pre, cond, post))
  stmt <- parseStmt
  return $ For pre cond post stmt
  ) <|> (do
  -- traceM "parseCtrl.Assert"
  reserved "assert"
  e <- parens parseExp
  semi
  return $ Assert e
  ) <|> (do
  -- traceM "parseCtrl.Return"
  reserved "return"
  e <- optionMaybe parseExp
  semi
  return $ Return e
  ) <?> "ctrl"

parseElse :: Parser Stmt
parseElse = (do
  -- traceM "parseElse"
  reserved "else"
  stmt <- parseStmt
  return stmt
  ) <?> "else"

-- Parses a postfix operator.
parsePostop :: Parser Postop
parsePostop = (do
  -- traceM "parsePostop.PP"
  reservedOp "++"
  return PP
  ) <|> (do
  -- traceM "parsePostop.MM"
  reservedOp "--"
  return MM
  ) <?> "postop"

-- Parses an assignment operator.
parseAsop :: Parser Common.Asop
parseAsop = (do
  -- traceM "parseAsop.="
  reservedOp "="
  return $ Common.Set
  ) <|> (do
  -- traceM "parseAsop.+="
  reservedOp "+="
  return $ Common.SetOp Common.AAdd
  ) <|> (do
  -- traceM "parseAsop.-="
  reservedOp "-="
  return $ Common.SetOp Common.ASub
  ) <|> (do
  -- traceM "parseAsop.*="
  reservedOp "*="
  return $ Common.SetOp Common.AMul
  ) <|> (do
  -- traceM "parseAsop./="
  reservedOp "/="
  return $ Common.SetOp Common.ADiv
  ) <|> (do
  -- traceM "parseAsop.%="
  reservedOp "%="
  return $ Common.SetOp Common.AMod
  ) <|> (do
  -- traceM "parseAsop.&="
  reservedOp "&="
  return $ Common.SetOp Common.AAnd
  ) <|> (do
  -- traceM "parseAsop.|="
  reservedOp "|="
  return $ Common.SetOp Common.AOr
  ) <|> (do
  -- traceM "parseAsop.^="
  reservedOp "^="
  return $ Common.SetOp Common.AXor
  ) <|> (do
  -- traceM "parseAsop.<<="
  reservedOp "<<="
  return $ Common.SetOp Common.AShL
  ) <|> (do
  -- traceM "parseAsop.>>="
  reservedOp ">>="
  return $ Common.SetOp Common.AShR
  ) <?> "asop"
