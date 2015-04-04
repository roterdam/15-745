{-
  CTree is an AST that corresponds to, and is convertible to, C code, as
  compared to the parseTree and previous AST which corresponded to C0.
-}

module Compile.Types.CTree where

import qualified Compile.Types.Common as Common
import Data.List (intercalate)


{-
  Represents a C program as an AST.
-}
data CTree = Prog [GDecl]

{-
  Same global declarations as an AST, but without FExt because external function
  declarations and local function declarations can be combined at this point.
-}
data GDecl = Typedef Common.Type Common.Ident
           | Sigdef Common.Type Common.Ident [Common.Param]
           | FDecl Common.Type Common.Ident [Common.Param]
           | FDefn Common.Type Common.Ident [Common.Param] [Stmt]
           | SDefn Common.Ident [Common.Param]

{-
  A statement. Again, note the similarity to AST; the only differences are that
  decls that didn't initialize the variable now use a dummy value, and assert
  has been treated like a normal function call.
-}
data Stmt = Assn Common.Asop LValue Exp | If Exp [Stmt] [Stmt]
          | While Exp [Stmt] | Return (Maybe Exp)
          | Decl Common.Type Common.Ident Exp [Stmt] | Exp Exp

{-
  An expression. Same as AST, but alloc and alloc_array are treated like normal
  function calls.
-}
data Exp = IntLit Common.IntLit | BoolLit Bool | CharLit Char | StringLit String
         | Ident Common.Ident | Binop Binop Exp Exp | Unop Unop Exp
         | Cond Exp Exp Exp | Call Exp [Exp] | Index Exp Exp | Star Exp
         | Dot Exp Common.Ident | Amp Common.Ident | Cast Common.Type Exp | Null


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
showStmt p (Assn op lv e) = joinL [p, show lv, " ", show op, " ", show e, "\n"]
showStmt p (If e tStmts []) = joinL [p, "if (", show e, ") {\n",
                                     showStmts ("  " ++ p) tStmts,
                                     p, "}\n"]
showStmt p (If e tStmts fStmts) = joinL [p, "if (", show e, ") {\n",
                                         showStmts ("  " ++ p) tStmts,
                                         p, "} else {\n",
                                         showStmts ("  " ++ p) fStmts,
                                         p, "}\n"]
showStmt p (While e stmts) = joinL [p, "while (", show e, ") {\n",
                                    showStmts ("  " ++ p) stmts,
                                    p, "}\n"]
showStmt p (Return Nothing) = joinL [p, "return;\n"]
showStmt p (Return (Just e)) = joinL [p, "return ", show e, ";\n"]
showStmt p (Decl t ident e stmts) =
  joinL [p, show t, " ", ident, " = ", show e, ";\n", showStmts p stmts]
showStmt p (Exp e) = joinL [p, show e, ";\n"]


{-
  Turns an CTree into a newline-terminated string.
-}
instance Show CTree where
  show (Prog gdecls) = joinWith "\n" $ map show gdecls

{-
  Turns a GDecl into a newline-terminated string.
-}
instance Show GDecl where
  show (Typedef t ident) = joinL ["tyepdef ", show t, " ", ident, ";\n"]
  show (Sigdef t ident params) =
    joinL ["typedef ", show t, " ", ident, showTuple params, "\n"]
  show (FDecl t ident params) =
    joinL [show t, " ", ident, showTuple params, ";\n"]
  show (FDefn t ident params stmts) =
    joinL [show t, " ", ident, showTuple params, " {\n",
           showStmts "  " stmts,
           "}\n"]
  show (SDefn ident fields) =
    joinL ["struct ", ident, " {\n",
           joinL [joinL ["  ", show p, ";\n"] | p <- fields],
           "}\n"]

{-
  Turns a statement into a newline-terminated string.
-}
instance Show Stmt where
  show stmt = showStmt "" stmt

{-
  Turns an expression into a string. Note that the string will not have
  parentheses around it, but translated subexpressions might.
  TODO: make prettier/remove parentheses where possible.
-}
instance Show Exp where
  show (IntLit num) = show num
  show (BoolLit bool) = show bool
  show (CharLit c) = show c
  show (StringLit s) = show s
  show (Ident ident) = ident
  show (Binop op e1 e2) =
    joinL ["(", show e1, ") ", show op, " (", show e2, ")"]
  show (Unop op e) = joinL [show op, "(", show e, ")"]
  show (Cond e1 e2 e3) =
    joinL ["(", show e1, ") ? (", show e2, ") : (", show e3, ")"]
  show (Call (Ident ident) args) = joinL [ident, showTuple args]
  show (Call e args) = joinL ["(", show e, ")", showTuple args]
  show (Index addr idx) = joinL ["(", show addr, ")[", show idx, "]"]
  show (Star addr) = joinL ["*(", show addr, ")"]
  show (Dot addr field) = joinL ["(", show addr, ").", field]
  show Null = "NULL"
  show (Amp ident) = joinL ["&", ident]
  show (Cast t e) = joinL ["(", show t, ")(", show e, ")"]

instance Show LValue where
  show (LIdent ident) = ident
  show (LIndex addr idx) = joinL ["(", show addr, ")[", show idx, "]"]
  show (LStar addr) = joinL ["*(", show addr, ")"]
  show (LDot addr field) = joinL ["(", show addr, ").", field]

instance Show Binop where
  show (CmpOp cmpop) = show cmpop
  show (ArithOp arithop) = show arithop
  show (LogOp logop) = show logop
