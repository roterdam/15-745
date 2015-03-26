{-
  This file contains combinators for parsing C0 tokens.
-}

module Compile.Parse.Tokens
(reserved
, comma
, semi
, identifier
, operator
, braces
, parens
, reservedOp
, natural
, hexadecimal
, decimal
, whiteSpace
, commaSep
, semiSep
, brackets
, charLiteral
, stringLiteral
) where

import Text.Parsec.Token (GenLanguageDef(..))
import Text.Parsec.Prim ((<|>), Stream(..))
import Text.Parsec.Char
import qualified Text.Parsec.Token as Tok

import Compile.Parse.Common (Parser, StreamT, StateT, MonadT)

def :: GenLanguageDef StreamT StateT MonadT
def = LanguageDef {
    commentStart    = string "/*",
    commentEnd      = string "*/",
    commentStartStr = "/*",
    commentEndStr   = "*/",
    commentLine     = string "//",
    nestedComments  = True,
    identStart      = letter <|> char '_',
    identLetter     = alphaNum <|> char '_',

    {-
      NOTE: These are hard-coded so that opStart contains all of the inital characters from ops in
      reservedOpNames. And opLetter contains all of the non-inital characters *except* for + and -.
      This turns out to be very important when parting the unary - operator.
    -}
    opStart         = oneOf "=+-*/%&^|<>!~:?.[]",
    opLetter        = oneOf "=&|<>",
    reservedNames   = ["struct", "typedef", "if", "else", "while", "for", "continue", "break",
                       "return", "assert", "true", "false", "NULL", "alloc", "alloc_array",
                       "int", "bool", "void", "char", "string"],
    reservedOpNames = ["!", "~", "-", "+", "*", "/", "%", "<<", ">>",
                       "<", ">", ">=", "<=", "==", "!=", "&", "^", "|", "&&", "||",
                       "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "|=", "^=",
                       ":", "?", "++", "--", "[", "]", ".", "->"],
    caseSensitive   = True }

lexer :: Tok.GenTokenParser StreamT StateT MonadT
lexer = Tok.makeTokenParser def

reserved      :: String -> Parser ()
reserved      = Tok.reserved lexer
comma         :: Parser ()
comma         = do _ <- Tok.comma lexer; return ()
semi          :: Parser ()
semi          = do _ <- Tok.semi lexer; return ()
identifier    :: Parser String
identifier    = Tok.identifier lexer
operator      :: Parser String
operator      = Tok.operator lexer
braces        :: Parser a -> Parser a
braces        = Tok.braces lexer
parens        :: Parser a -> Parser a
parens        = Tok.parens lexer
reservedOp    :: String -> Parser ()
reservedOp    = Tok.reservedOp lexer
natural       :: Parser Integer
natural       = Tok.natural lexer
hexadecimal   :: Parser Integer
hexadecimal   = Tok.hexadecimal lexer
decimal       :: Parser Integer
decimal       = Tok.decimal lexer
whiteSpace    :: Parser ()
whiteSpace    = Tok.whiteSpace lexer
commaSep      :: Parser a -> Parser [a]
commaSep      = Tok.commaSep lexer
semiSep       :: Parser a -> Parser [a]
semiSep       = Tok.semiSep lexer
brackets      :: Parser a -> Parser a
brackets      = Tok.brackets lexer
stringLiteral :: Parser String
stringLiteral = Tok.stringLiteral lexer
charLiteral   :: Parser Char
charLiteral   = Tok.charLiteral lexer
