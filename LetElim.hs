----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de eliminación de LETs
--
-- Un LET (let x = e1 in e2) es eliminado si e1 es
-- un literal entero o booleano. En ese caso se
-- sustituyen las ocurrencias de x en e2 por e1,
-- o sea, e2[e1/x].
----------------------------------------------------------------------------

module LetElim where

import Syntax

-- e[c/x] sustituye todas las ocurrencias de x en e por c, "sustituye x por c en e"
subst :: Name -> Expr -> Expr -> Expr
subst variable literal expr = case expr of
  Var name ->
    if variable == name then literal else expr
  Infix op left right ->
    Infix op (subst variable literal left) (subst variable literal right)
  If cond thenExpr elseExpr ->
    If (subst variable literal cond) (subst variable literal thenExpr) (subst variable literal elseExpr)
  Let inner@(boundVariable, _) innerExpr outerExpr ->
    if variable == boundVariable
      then Let inner innerExpr outerExpr
      else Let inner (subst variable literal innerExpr) (subst variable literal outerExpr)
  App funcName args -> App funcName (map (subst variable literal) args)
  _ -> expr

letElim :: Expr -> Expr
letElim (Let inner@(boundVariable, _) innerExpr outerExpr) =
  let optInner = letElim innerExpr
      optOuter = letElim outerExpr
   in case optInner of
        IntLit _ -> subst boundVariable optInner optOuter
        BoolLit _ -> subst boundVariable optInner optOuter
        _ -> Let inner optInner optOuter
letElim (Infix op left right) = Infix op (letElim left) (letElim right)
letElim (If cond thenExpr elseExpr) = If (letElim cond) (letElim thenExpr) (letElim elseExpr)
letElim (App name args) = App name (map letElim args)
letElim e = e

letElimP :: Program -> Program
letElimP (Program defs expr) =
  let optDefs = map (\(FunDef t names expr) -> FunDef t names (letElim expr)) defs
      optExpr = letElim expr
   in Program optDefs optExpr
