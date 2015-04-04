{-
  AST is a more condensed, functional tree representation (compared to ParseTree).
  An AST is easier to navigate than a ParseTree. Therefore, AST will be used
  for type-checking, control-flow checks, and certain optimizations.
-}

module Compile.Types.AST where

import qualified Compile.Types.Common as Common
import Data.List (intercalate)


-- An abstract syntax tree.
data AST = Prog [GDecl]

-- A declaration in the global scope.
-- TODO: should AST and ParseTree differentiate FnName and TypeName from Common.Ident like other types do?
data GDecl = Typedef Common.Type Common.Ident | Sigdef Common.Type Common.Ident [Common.Param]
           | FDecl Common.Type Common.Ident [Common.Param] | FExt Common.Type Common.Ident [Common.Param]
           | FDefn Common.Type Common.Ident [Common.Param] [Stmt]
           | SDefn Common.Ident [Common.Param]

-- A statement. Note that we separate declaration and assignment for easier typechecking.
data Stmt = Assn Common.Asop LValue Exp | If Exp [Stmt] [Stmt] | While Exp [Stmt]
          | Return (Maybe Exp) | Decl Common.Type Common.Ident (Maybe Exp) [Stmt] | Assert Exp | Exp Exp

-- An expression.
data Exp = IntLit Common.IntLit | BoolLit Bool | CharLit Char | StringLit String
         | Ident Common.Ident | Binop Binop Exp Exp | Unop Unop Exp | Cond Exp Exp Exp | Call Exp [Exp]
         | Alloc Common.Type | AllocArray Common.Type Exp | Index Exp Exp | Star Exp | Dot Exp Common.Ident
         | Amp Common.Ident | Cast Common.Type Exp | Null

-- A Binary operator.
data Binop = ArithOp Common.ArithOp | CmpOp Common.CmpOp | LogOp Common.LogOp

-- A unary operator.
type Unop = Common.Unop

-- A value we can assign into.
data LValue = LIdent Common.Ident | LStar LValue | LDot LValue Common.Ident | LIndex LValue Exp

{-
  Show implementations at the end for clarity
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
showStmt p (Decl t ident Nothing stmts) =
  joinL [p, show t, " ", ident, ";\n", showStmts p stmts]
showStmt p (Decl t ident (Just e) stmts) =
  joinL [p, show t, " ", ident, " = ", show e, ";\n", showStmts p stmts]
showStmt p (Assert e) = joinL [p, "assert(", show e, ")\n"]
showStmt p (Exp e) = joinL [p, show e, ";\n"]


{-
  Turns an AST into a newline-terminated string.
-}
instance Show AST where
  show (Prog gdecls) = joinWith "\n" $ map show gdecls

{-
  Turns a GDecl into a newline-terminated string.
-}
instance Show GDecl where
  show (Typedef t ident) = joinL ["tyepdef(", show t, ", ", ident, ")\n"]
  show (Sigdef t ident params) =
    joinL ["sigdef(", show t, ", ", ident, ", ", showTuple params, ")\n"]
  show (FDecl t ident params) =
    joinL ["fdecl(", show t, ", ", ident, ", ", showTuple params, ")\n"]
  show (FExt t ident params) =
    joinL ["fext(", show t, ", ", ident, ", ", showTuple params, ")\n"]
  show (FDefn t ident params stmts) =
    joinL ["fdecl(", show t, ", ", ident, ", ", showTuple params, "):\n",
          showStmts "  " stmts]
  show (SDefn ident fields) =
    joinL ["sDef(", ident, ", ", showTuple fields, ")\n"]

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
  show (Alloc t) = joinL ["alloc(", show t, ")"]
  show (AllocArray t e) = joinL ["alloc_array(", show t, ", ", show e, ")"]
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
