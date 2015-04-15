{-
  This file contains parsers for low-level constructs in a C0 source file.
  It contains parsers for Exps, LValues, and Types.
-}

module Compile.Parse.Helpers where

import Compile.Types.ParseTree hiding (tab)
import qualified Compile.Types.Common as Common
import Compile.Parse.Tokens
import Compile.Parse.Common (Parser, StreamT, StateT, MonadT, sigdefs, typedefs)

import Text.Parsec.Combinator (lookAhead, notFollowedBy)
import Text.Parsec.Char (char, oneOf, digit, anyChar)
import Text.Parsec.Expr (Operator(..), Assoc(..), buildExpressionParser)
import Text.Parsec.Prim (getState, try, (<|>), (<?>), getPosition, getInput, many)
import qualified Data.Set as Set
import qualified Data.Map as Map

-- TODO: remove
import Data.ByteString.Char8 (unpack)
import Debug.Trace (trace)
traceM msg = do
  pos <- getPosition
  trace (msg ++ ": " ++ show pos) (return ())

type OpParser a = Operator StreamT StateT MonadT a

-- Parses an LValue.
parseLValue :: Parser LValue
parseLValue = buildExpressionParser table parseTerm
  where table = [ [ prefix "*"  LStar ] ]

        -- Parses a term (i.e. an atom) for an LValue.
        parseTerm :: Parser LValue
        parseTerm = (do
          --traceM "parseLValue.parseTerm"
          base <- parseTermBase
          parseTermTail base
          )

        -- Parses an LValue term without any indices.
        parseTermBase :: Parser LValue
        parseTermBase = (do
          --traceM "parseLValue.parseTermBase.LIdent"
          ident <- identifier
          return $ LIdent ident
          ) <|> (do
          --traceM "parseLValue.parseTermBase.parens"
          parens parseLValue
          )

        -- Parses all trailing indices on an LValue.
        parseTermTail :: LValue -> Parser LValue
        parseTermTail base = (do
          --traceM "parseLValue.parseTermTail.LIndex"
          index <- brackets parseExp
          parseTermTail $ LIndex base index
          ) <|> (do
          --traceM "parseLValue.parseTermTail.LDot"
          reservedOp "."
          field <- identifier
          parseTermTail $ LDot base field
          ) <|> (do
          --traceM "parseLValue.parseTermTail.Arrow"
          reservedOp "->"
          field <- identifier
          parseTermTail $ LArrow base field
          ) <|> (do
          --traceM "parseLValue.parseTermTail.base"
          return base
          )

-- Parses one expression.
-- NOTE: Since -- is used in Postops, not in expressions, we must case on it here and
-- explicitly prevent it from being parsed.
parseExp :: Parser Exp
parseExp = (do
  e1 <- parseExpNonCond
  (do
    --traceM "parseExp.Cond"
    reservedOp "?"
    e2 <- parseExp
    reservedOp ":"
    e3 <- parseExp
    pos <- getPosition
    return $ Cond e1 e2 e3
    ) <|> (do
    --traceM "parseExp.base"
    return e1
    )
  ) <?> "expression"

-- Parses an expression guaranteed to not be a conditional.
parseExpNonCond :: Parser Exp
parseExpNonCond = buildExpressionParser table parseTerm
  where table = [ [ prefix  "--"  reject                                              ,
                    prefix  "!"   (Unop Common.Bang)                                  ,
                    prefix  "~"   (Unop Common.Inv)                                   ,
                    prefix  "-"   (Unop Common.Neg)                                   ,
                    parseCast                                                         ,
                    prefix  "*"   Star                                                ],
                  [ binary  "*"   (Binop $ ArithOp Common.AMul)             AssocLeft ,
                    binary  "/"   (Binop $ ArithOp Common.ADiv)             AssocLeft ,
                    binary  "%"   (Binop $ ArithOp Common.AMod)             AssocLeft ],
                  [ binary  "+"   (Binop $ ArithOp Common.AAdd)             AssocLeft ,
                    binary  "--"  reject                                    AssocLeft ,
                    binary  "-"   (Binop $ ArithOp Common.ASub)             AssocLeft ],
                  [ binary  "<<"  (Binop $ ArithOp Common.AShL)             AssocLeft ,
                    binary  ">>"  (Binop $ ArithOp Common.AShR)             AssocLeft ],
                  [ binary  "<"   (Binop $ CmpOp $ Common.CmpOp Common.L)   AssocLeft ,
                    binary  ">"   (Binop $ CmpOp $ Common.CmpOp Common.G)   AssocLeft ,
                    binary  "<="  (Binop $ CmpOp $ Common.CmpOp Common.LE)  AssocLeft ,
                    binary  ">="  (Binop $ CmpOp $ Common.CmpOp Common.GE)  AssocLeft ],
                  [ binary  "=="  (Binop $ CmpOp $ Common.CmpOp Common.E)   AssocLeft ,
                    binary  "!="  (Binop $ CmpOp $ Common.CmpOp Common.NE)  AssocLeft ],
                  [ binary  "&"   (Binop $ ArithOp Common.AAnd)             AssocLeft ],
                  [ binary  "^"   (Binop $ ArithOp Common.AXor)             AssocLeft ],
                  [ binary  "|"   (Binop $ ArithOp Common.AOr)              AssocLeft ],

                  -- NOTE: Since the way that LAnd and LOr associate does not affect the semantics,
                  -- we parse them as right-associative operator (instead of left-associative, as
                  -- the grammar specifies) to make elaboration cleaner later on.
                  [ binary  "&&"  (Binop $ LogOp Common.LAnd)               AssocRight],
                  [ binary  "||"  (Binop $ LogOp Common.LOr)                AssocRight] ]

        -- Fails when faced with a -- operator in the wrong positions.
        reject = fail "Invalid op"
        parseCast = Prefix $ try (do
          t <- parens parseType
          return $ Cast t
          )

        -- Parses a term.
        parseTerm :: Parser Exp
        parseTerm = (do
          --traceM "parseExpNonCond.parseTerm"
          base <- parseTermBase
          parseTermPost base
          )

        -- Parses a term with no postfix component (a literal, built-in, ident, reference, or cast).
        parseTermBase :: Parser Exp
        parseTermBase = (do
          --traceM "parseExpNonCond.parseTermBase.Tabulate"
          reserved "tabulate"
          f <- parseExp
          n <- parseExp
          return $ Tabulate f n
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.Map"
          reserved "map"
          f <- parseExp
          s <- parseExp
          return $ Map f s
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.Reduce"
          reserved "reduce"
          f <- parseExp
          b <- parseExp
          s <- parseExp
          return $ Reduce f b s 
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.Filter"
          reserved "filter"
          p <- parseExp
          s <- parseExp
          return $ Filter p s
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.Combine"
          reserved "combine"
          f <- parseExp
          a <- parseExp
          b <- parseExp 
          return $ Combine f a b
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.seq"
          reserved "seq"
          parens (do 
            start <- parseExp
            listSeq <- lookAhead ((do
                        comma 
                        return True
                        ) <|> (do
                        reservedOp ".."
                        return False
                        ))
            if listSeq
                then (do
                    comma 
                    rest <- commaSep parseExp 
                    return $ ListSeq (start:rest))
                else (do
                    reservedOp ".."
                    end <- parseExp
                    return $ RangeSeq start end)
            )
          ) <|> (do 
          --traceM "parseExpNonCond.parseTermBase.IntLit"
          n <- parseInt
          return $ IntLit n
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.BoolLit"
          bool <- parseBool
          return $ BoolLit bool
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.CharLit"
          c <- parseChar
          return $ CharLit c
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.StringLit"
          s <- parseString
          return $ StringLit s
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.Null"
          reserved "NULL"
          return Null
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.Alloc"
          reserved "alloc"
          t <- parens parseType
          return $ Alloc t
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.AllocArray"
          reserved "alloc_array"
          (t, e) <- parens (do
            t <- parseType
            comma
            e <- parseExp
            return (t, e)
            )
          return $ AllocArray t e
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.Amp"
          reservedOp "&"
          ident <- identifier
          return $ Amp ident
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.Ident"
          ident <- identifier
          return $ Ident ident
          ) <|> (do
          --traceM "parseExpNonCond.parseTermBase.parens"
          term <- parens parseExp
          return term
          ) 
       
         
        
        -- Parses all postfix operations from a term.
        parseTermPost :: Exp -> Parser Exp
        parseTermPost base = (do
          --traceM "parseExp.parseTermPost.Index"
          index <- brackets parseExp
          parseTermPost $ Index base index
          ) <|> try (do
          --traceM "parseExp.parseTermPost.Dot"
          reservedOp "."
          notFollowedBy $ reservedOp "."
          field <- identifier
          parseTermPost $ Dot base field
          ) <|> (do
          --traceM "parseExp.parseTermPost.Arrow"
          reservedOp "->"
          field <- identifier
          parseTermPost $ Arrow base field
          ) <|> (do
          --traceM "parseExp.parseTermPost.Call"
          args <- parseArgList
          parseTermPost $ Call base args
          ) <|> (do
          --traceM "parseExp.parseTermPost.base"
          return base
          )

        -- Parses the argument list for a function call.
        parseArgList :: Parser [Exp]
        parseArgList = (do
          --traceM "parseExpNonCond.parseArgList"
          args <- parens $ commaSep parseExp
          return args
          ) <?> "arg list"

        -- Parses a decimal or hexadecimal number.
        -- NOTE: we explicitly reject octal numbers (numbers with a leading 0 that are not 0) here.
        parseInt :: Parser Common.IntLit
        parseInt = try (do
          --traceM "parseExpNonCond.parseInt"
          whiteSpace
          char '0'
          x <- hexadecimal
          whiteSpace
          return $ Common.Hex x
          ) <|> (do
          whiteSpace

          -- Do not allow octal.
          lookAhead ((do
              oneOf "123456789"
              return ()
            ) <|> (do
              char '0'
              notFollowedBy digit
            ))

          n <- decimal
          whiteSpace
          return $ Common.Dec n
          ) <?> "number"

        -- Parses a boolean literal.
        parseBool :: Parser Bool
        parseBool = (do
          --traceM "parseExpNonCond.parseBool.True"
          reserved "true"
          return True
          ) <|> (do
          --traceM "parseExpNonCond.parseBool.False"
          reserved "false"
          return False
          ) <?> "bool"

        -- Parses a string literal.
        parseString :: Parser String
        parseString = (do
          char '\"'
          s <- many parseStringChar
          char '\"'
          whiteSpace
          return s
          ) <?> "string"

        -- Parses one valid character in a string.
        parseStringChar :: Parser Char
        parseStringChar = try (do
          char '\\'
          c <- oneOf codes
          return $ codeMap Map.! c
          ) <|> (do
          notFollowedBy $ oneOf "\\\n\""
          anyChar
          )

        -- Parse a valid lone char.
        parseChar :: Parser Char
        parseChar = (do
          char '\''
          c <- try (do
            char '\\'
            c <- oneOf codes
            return $ codeMap Map.! c
            ) <|> (do
            notFollowedBy $ char '\\'
            anyChar
            )
          char '\''
          whiteSpace
          return c
          )

        -- Maps escaped chars (chars after '\' in escape sequences) to the chars they represent.
        codeMap :: Map.Map Char Char
        codeMap = Map.fromList [
          ('a', '\a'),
          ('t', '\t'),
          ('r', '\r'),
          ('f', '\f'),
          ('b', '\b'),
          ('n', '\n'),
          ('v', '\v'),
          ('\'', '\''),
          ('\"', '\"'),
          ('\\', '\\'),
          ('0', '\0') ]
        codes :: [Char]
        codes = Map.keys codeMap

-- Parses a defined or concrete type.
-- To parse defined types, we use the set of defined type idents that is passed around in the
-- parser's state.
parseType :: Parser Common.Type
parseType = (do
  --traceM "parseType"
  base <- parseTypeBase
  parseTypePost base
  ) <?> "type"
  where -- A type base does not contain any []'s or *'s.
        parseTypeBase :: Parser Common.Type
        parseTypeBase = (do
          --traceM "parseTypeBase.IntT" or traceM "parseTypeBase.SeqT IntT"
          reserved "int"
          parseTypeSeq Common.IntT 
          ) <|> (do
          --traceM "parseTypeBase.CharT" or traceM "parseTypeBase.SeqT CharT"
          reserved "char"
          parseTypeSeq Common.CharT
          ) <|> (do
          --traceM "parseTypeBase.StringT" or traceM "parseTypeBase.SeqT StringT"
          reserved "string"
          parseTypeSeq Common.StringT
          ) <|> (do
          --traceM "parseTypeBase.BoolT" or traceM "parseTypeBase.SeqT BoolT"
          reserved "bool"
          parseTypeSeq Common.BoolT
          ) <|> (do
          --traceM "parseTypeBase.VoidT"
          reserved "void"
          return Common.VoidT
          ) <|> (do
          --traceM "parseTypeBase.StructT"
          reserved "struct"
          whiteSpace -- TODO: necessary?
          ident <- identifier
          return $ Common.StructT ident
          ) <|> (do
          -- We only parse idents if they are defined/fn types, in order to resolve ambiguiities.
          ident <- lookAhead identifier
          state <- getState
          if Set.member ident $ typedefs state
            then do
              --traceM "parseTypeBase.DefT"
              ident <- identifier
              return $ Common.DefT ident
            else if Set.member ident $ sigdefs state
              then do
                --traceM "parseTypeBase.FnT"
                ident <- identifier
                return $ Common.FnT ident
              else fail "No defined type with this ident"
          )

        parseTypeSeq :: Common.Type -> Parser Common.Type 
        parseTypeSeq t = try (do
            reservedOp "<>"
            return $ Common.SeqT t
            ) <|> (do
            return $ t
            )

        -- Postfix type modifiers are []'s and *'s.
        parseTypePost :: Common.Type -> Parser Common.Type
        parseTypePost base = (do
          --traceM "parseTypePost.ArrT"
          brackets whiteSpace
          t <- parseTypePost $ Common.ArrT base
          return t
          ) <|> (do
          --traceM "parseTypePost.PtrT"
          reservedOp "*"
          t <- parseTypePost $ Common.PtrT base
          return t
          ) <|> (do
          --traceM "parseTypePost.base"
          return base
          )

-- Helpers for buildExpressionParser.
-- Taken from \url{https://hackage.haskell.org/package/parsec-3.0.0/docs/Text-Parsec-Expr.html}
binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
suffix name fun       = Postfix (do{ reservedOp name; return fun })
