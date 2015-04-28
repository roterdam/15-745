{-
  CTree is an AST that corresponds to, and is convertible to, C code, as
  compared to the parseTree and previous AST which corresponded to C0.
-}

module Compile.Types.CTree where

import qualified Compile.Types.Common as Common
import Data.List (intercalate)
import Data.Char (showLitChar)

{-
  Represents a C program as an AST.
-}
data CTree = Prog [GDecl]

{-
  Same global declarations as an AST, but without FExt because external function
  declarations and local function declarations can be combined at this point.
-}
data GDecl = Include String
           | Typedef CType Common.Ident
           | Sigdef CType Common.Ident [CParam]
           | FDecl CType Common.Ident [CParam]
           | FDefn CType Common.Ident [CParam] [Stmt]
           | SDefn Common.Ident [CParam]

{-
  A statement. Again, note the similarity to AST; the only difference is that
  assert has been treated like a normal function call.
-}
data Stmt = Assn Common.Asop LValue Exp | If Exp [Stmt] [Stmt]
          | While Exp [Stmt] | Return (Maybe Exp)
          | Decl CType Common.Ident (Maybe Exp) [Stmt] | Exp Exp

{-
  An expression. Same as AST, but alloc and alloc_array are treated like normal
  function calls.
-}
data Exp = IntLit Common.IntLit | BoolLit Bool | CharLit Char | StringLit String
         | Ident Common.Ident | Binop Binop Exp Exp | Unop Unop Exp
         | Cond Exp Exp Exp | Call Exp [Exp] | Index Exp Exp | Star Exp
         | Dot Exp Common.Ident | Amp Common.Ident | Cast CType Exp
         | Null | Sizeof CType


{-
  A binary operator. Same as in AST.
-}
data Binop = ArithOp Common.ArithOp | CmpOp Common.CmpOp | LogOp Common.LogOp

{-
  A unary operator. Same as in AST.
-}
type Unop = Common.Unop

{-
  A value we can assign into. Same as in AST.
-}
data LValue = LIdent Common.Ident | LStar LValue | LDot LValue Common.Ident
            | LIndex LValue Exp

{-
  A parameter (typed variable).
-}
data CParam = CParam CType Common.Ident

{-
  A type.
-}
data CType = CDefT Common.Ident | CStructT Common.Ident | CFnT Common.Ident
           | CPtrT CType | CIntT | CBoolT | CVoidT | CCharT deriving Eq


{-
  Implementations of Show grouped at the end for clarity.
-}
joinWith :: String -> [String] -> String
joinWith = intercalate
joinL :: [String] -> String
joinL = joinWith ""

showTuple :: (Show a) => [a] -> String
showTuple xs = joinL ["(", (joinWith ", " $ map show xs), ")"]

{-
  Pretty-prints a list of statements, given a prefix to put at the start of each
  line (used for indenting). There is one newline after each statement,
  including the last one.
-}
showStmts :: String -> [Stmt] -> String
showStmts prefix stmts = joinL $ map (showStmt prefix) stmts

{-
  Pretty-prints a single statement, given a prefix to put at the start of the
  string (used for indenting). The string will end in a newline.
-}
showStmt :: String -> Stmt -> String
showStmt p (Assn op lv e) = joinL [p, show lv, " ", show op, " ", show e, ";\n"]
showStmt p (If e [] []) = joinL [p, "if (", show e, ");\n"]
showStmt p (If e tStmts []) = joinL [p, "if (", show e, ") {\n",
                                     showStmts ("  " ++ p) tStmts,
                                     p, "}\n"]
showStmt p (If e tStmts fStmts) = joinL [p, "if (", show e, ") {\n",
                                         showStmts ("  " ++ p) tStmts,
                                         p, "} else {\n",
                                         showStmts ("  " ++ p) fStmts,
                                         p, "}\n"]
showStmt p (While e []) = joinL [p, "while (", show e, ");\n"]
showStmt p (While e stmts) = joinL [p, "while (", show e, ") {\n",
                                    showStmts ("  " ++ p) stmts,
                                    p, "}\n"]
showStmt p (Return Nothing) = joinL [p, "return;\n"]
showStmt p (Return (Just e)) = joinL [p, "return ", show e, ";\n"]
showStmt p (Decl t ident Nothing stmts) =
  joinL [p, show t, " ", ident, ";\n", showStmts p stmts]
showStmt p (Decl t ident (Just e) stmts) =
  joinL [p, show t, " ", ident, " = ", show e, ";\n", showStmts p stmts]
showStmt p (Exp e) = joinL [p, show e, ";\n"]

{-
  Converts an expression to a string, using the scheme below to determine when
  to wrap subexpressions in parentheses:

  CATEGORIES:
    literals (including sizeof)
    ops (binop, unop, cond)
    calls
    lMems (star and amp)
    rMems (index and dot)
    casts

    literals don't wrap
    ops only wrap each other
    calls wrap everything except literals
    lmems wrap ops, calls, and rmems
    rmems wrap ops, casts, and lmems
    casts wrap everything except literals
-}
data ExpressionCategory = LitCat | OpCat | CallCat | LMemCat | RMemCat | CastCat
showExp :: Exp -> (String, ExpressionCategory)
showExp e =
  let cat = getCategory e
      showNested = showNestedExp cat
      s = (case e of
            (IntLit n) -> show n
            (BoolLit b) -> if b then "true" else "false"
            (CharLit c) -> if (c == '\0') then "'\\0'" else show c
            (StringLit s) -> show s
            (Ident ident) -> ident
            (Binop op e1 e2) ->
              joinL [showNested e1, " ", show op, " ", showNested e2]
            (Unop op e') -> joinL [show op, showNested e']
            (Cond e1 e2 e3) ->
              joinL [showNested e1, " ? ", showNested e2, " : ", showNested e3]
            (Call e' args) -> joinL [showNested e', showTuple args]
            (Index addr idx) ->
              joinL [showNested addr, "[", showNested idx, "]"]
            (Star addr) -> joinL ["*", showNested addr]
            (Dot addr field) -> joinL [showNested addr, ".", field]
            (Amp ident) -> joinL ["&", ident]
            (Cast t e') -> joinL ["(", show t, ")(", show e', ")"]
            (Null) -> "NULL"
            (Sizeof t) -> joinL ["sizeof(", show t, ")"])
  in (s, cat)
  where getCategory :: Exp -> ExpressionCategory
        getCategory (Binop _ _ _) = OpCat
        getCategory (Unop _ _) = OpCat
        getCategory (Cond _ _ _) = OpCat
        getCategory (Call _ _) = CallCat
        getCategory (Index _ _) = RMemCat
        getCategory (Star _) = LMemCat
        getCategory (Dot _ _) = RMemCat
        getCategory (Amp _) = LMemCat
        getCategory (Cast _ _) = CastCat
        getCategory _ = LitCat

        needsWrap :: ExpressionCategory -> ExpressionCategory -> Bool
        needsWrap LitCat _ = error "literal with subexpression"
        needsWrap OpCat OpCat = True
        needsWrap OpCat _ = False
        needsWrap CallCat LitCat = False
        needsWrap CallCat _ = True
        needsWrap LMemCat OpCat = True
        needsWrap LMemCat CallCat = True
        needsWrap LMemCat RMemCat = True
        needsWrap LMemCat _ = False
        needsWrap RMemCat OpCat = True
        needsWrap RMemCat CastCat = True
        needsWrap RMemCat LMemCat = True
        needsWrap RMemCat _ = False
        needsWrap CastCat LitCat = False
        needsWrap CastCat _ = True

        showNestedExp :: ExpressionCategory -> Exp -> String
        showNestedExp parentCat e =
          let (s, cat) = showExp e
          in (if needsWrap parentCat cat
              then joinL ["(", s, ")"]
              else s)


{-
  Turns the given lvalue into a string, adding parentheses as needed to make
  sure the reader doesn't need to know the evaluation semantics of C.
  RULES:
    the toplevel lvalue doesn't need parentheses.
    lIdent, lIndex, and lDot can be nested without parentheses in between.
    lIdent and lStar can be nested without parentheses in between.
-}
showLValue :: LValue -> (String, Maybe Bool)
showLValue (LIdent ident) = (ident, Nothing)
showLValue (LIndex arrLV idx) =
  case (showLValue arrLV) of
    (s, Just False) -> (joinL ["(", s, ")[", show idx, "]"], Just True)
    (s, _) -> (joinL [s, "[", show idx, "]"], Just True)
showLValue (LStar addrLV) =
  case (showLValue addrLV) of
    (s, Just True) -> (joinL ["*(", s, ")"], Just False)
    (s, _) -> (joinL ["*", s], Just False)
showLValue (LDot addrLV field) =
  case (showLValue addrLV) of
    (s, Just False) -> (joinL ["(", s, ").", field], Just True)
    (s, _) -> (joinL [s, ".", field], Just True)




{-
  Turns an CTree into a newline-terminated string.
-}
instance Show CTree where
  show (Prog gdecls) = joinWith "\n" $ map show gdecls

{-
  Turns a GDecl into a newline-terminated string.
-}
instance Show GDecl where
  show (Include s) = joinL ["#include ", s]
  show (Typedef t ident) = joinL ["typedef ", show t, " ", ident, ";\n"]
  show (Sigdef t ident params) =
    joinL ["typedef ", show t, " ", ident, showTuple params, ";\n"]
  show (FDecl t ident params) =
    joinL [show t, " ", ident, showTuple params, ";\n"]
  show (FDefn t ident params stmts) =
    joinL [show t, " ", ident, showTuple params, " {\n",
           showStmts "  " stmts,
           "}\n"]
  show (SDefn ident fields) =
    joinL ["struct ", ident, " {\n",
           joinL [joinL ["  ", show p, ";\n"] | p <- fields],
           "};\n"]

{-
  Turns a statement into a newline-terminated string.
-}
instance Show Stmt where
  show stmt = showStmt "" stmt

{-
  Turns an expression into a string. Note that the string will not have
  parentheses around it, but translated subexpressions might.
-}
instance Show Exp where
  show e = fst $ showExp e

instance Show LValue where
  show lv = fst $ showLValue lv

instance Show Binop where
  show (CmpOp cmpop) = show cmpop
  show (ArithOp arithop) = show arithop
  show (LogOp logop) = show logop

instance Show CParam where
  show (CParam t ident) = show t ++ " " ++ ident

instance Show CType where
  show CIntT = "int"
  show CBoolT = "bool"
  show CVoidT = "void"
  show CCharT = "char"
  show (CDefT ident) = ident
  show (CStructT ident) = "struct " ++ ident
  show (CFnT ident) = ident
  show (CPtrT t) = show t ++ "*"
