{-
  Contains useful functions for working with ASTs.
-}

module Compile.Types.ASTUtils where

import Compile.Types.AST
import Data.List (mapAccumL)

{-
  Loops backwards through the statements, with a user-supplied function that
  takes an updated statement and list of successor statements, and produces a
  new list of statements.
-}
astUpdateStmtsR :: (Stmt -> [Stmt] -> [Stmt]) -> AST -> AST
astUpdateStmtsR f (Prog gDecls) = Prog $ map (updateGDecl f) gDecls
  where updateGDecl :: (Stmt -> [Stmt] -> [Stmt]) -> GDecl -> GDecl
        updateGDecl f (FDefn t fName params stmts) =
          FDefn t fName params $ updateStmts f stmts
        updateGDecl _ x = x

        updateStmts :: (Stmt -> [Stmt] -> [Stmt]) -> [Stmt] -> [Stmt]
        updateStmts f = foldr (\s ss -> f (updateStmt f s) ss) []

        updateStmt :: (Stmt -> [Stmt] -> [Stmt]) -> Stmt -> Stmt
        updateStmt _ stmt@(Assn _ _ _) = stmt
        updateStmt f (If e stmts1 stmts2) =
          If e (updateStmts f stmts1) (updateStmts f stmts2)
        updateStmt f (While e stmts) =
          While e $ updateStmts f stmts
        updateStmt _ stmt@(Return _) = stmt
        updateStmt f (Decl t v eOpt stmts) =
          Decl t v eOpt $ updateStmts f stmts
        updateStmt _ stmt@(Assert _) = stmt
        updateStmt _ stmt@(Exp _) = stmt


{-
  Loops through the AST, folding over the function bodies to produce new
  function bodies and gather information.
-}
astMapAccumFnStmts :: (a -> LValue -> (a, LValue))
                   -> (a -> Exp -> (a, Exp))
                   -> (a -> a -> a)
                   -> a -> AST -> (a, AST)
astMapAccumFnStmts updateLV updateE join a (Prog gDecls) =
  let (a', gDecls') = mapAccumL (mapAccumFnBody updateLV updateE join) a gDecls
  in (a', Prog gDecls')
  where mapAccumFnBody :: (a -> LValue -> (a, LValue))
                       -> (a -> Exp -> (a, Exp))
                       -> (a -> a -> a)
                       -> a -> GDecl -> (a, GDecl)
        mapAccumFnBody updateLV updateE join a (FDefn t fName params stmts) =
          let (a', stmts') = astMapAccumStmts updateLV updateE join a stmts
          in (a', FDefn t fName params stmts')
        mapAccumFnBody _ _ _ a gDecl = (a, gDecl)


{-
  Loops through a list of statements, accumulating information as well as
  building a new list.
-}
astMapAccumStmts :: (a -> LValue -> (a, LValue))
                 -> (a -> Exp -> (a, Exp))
                 -> (a -> a -> a)
                 -> a -> [Stmt] -> (a, [Stmt])
astMapAccumStmts updateLV updateE join a stmts =
  mapAccumL (mapAccumStmt updateLV updateE join) a stmts
  where mapAccumStmt :: (a -> LValue -> (a, LValue))
                        -> (a -> Exp -> (a, Exp))
                        -> (a -> a -> a)
                        -> a -> Stmt -> (a, Stmt)
        mapAccumStmt updateLV updateE _ a (Assn op lv e) =
          let (a', lv') = updateLV a lv
              (a'', e') = updateE a' e
          in (a'', Assn op lv' e')
        mapAccumStmt updateLV updateE join a (If e stmts1 stmts2) =
          let (a', e') = updateE a e
              (a1, stmts1') = astMapAccumStmts updateLV updateE join a' stmts1
              (a2, stmts2') = astMapAccumStmts updateLV updateE join a' stmts2
          in (join a1 a2, If e' stmts1' stmts2')
        mapAccumStmt updateLV updateE join a (While e stmts) =
          let (a', e') = updateE a e
              (a'', stmts') = astMapAccumStmts updateLV updateE join a' stmts
          in (join a' a'', While e' stmts')
        mapAccumStmt _ updateE _ a (Return eOp) =
          let (a', eOp') = case eOp of
                              Nothing -> (a, Nothing)
                              (Just e) -> let (a', e') = updateE a e
                                          in (a', Just e')
          in (a', Return eOp')
        mapAccumStmt updateLV updateE join a (Decl t v eOp stmts) =
          let (a', eOp') = case eOp of
                              Nothing -> (a, Nothing)
                              (Just e) -> let (a', e') = updateE a e
                                          in (a', Just e')
              (a'', stmts') = astMapAccumStmts updateLV updateE join a' stmts
          in (a'', Decl t v eOp' stmts')
        mapAccumStmt _ updateE _ a (Assert e) =
          let (a', e') = updateE a e
          in (a', Assert e')
        mapAccumStmt _ updateE _ a (Exp e) =
          let (a', e') = updateE a e
          in (a', Exp e')
