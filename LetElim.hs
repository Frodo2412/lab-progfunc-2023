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

import Data.List
import Syntax

-- ELIMINACION DE LETs

{-
We want to optimize the AST by eliminating LETs, with the following observations:
Every LET can be eliminated if the expression e1 is a literal integer or boolean, in that case we substitute all the ocurrences of x in e2 by e1, that is e2[e1/x] only for unboud variables
A variable "x" is called unbound in an expression "e" if it doest appear in the body of any LET that contains "x" in its definition, otherwise it is called bound
ex:
 x is unbound in x + 2

 In let x :: Int = 4 * x in x + 2, "x" has one unbound ocurrence (4 * x) and one bound ocurrence (x + 2)

 examples of optimization:
1)      let x = 3 in x + 2 * x ==> 3 + 2 * 3
2)      let x = 3 * 4 in x + 2 ==> let x = 3 * 4 in x + 2 (no optimization)
3)      let x = 3 in let x = 4 in x + 2 ==> let x = 4 in x + 2 ==> 4 + 2
4)      let x = 3 in let y = x in y + 2 ===> let y = 3 in y + 2 ==> 3 + 2
5)      let x = 3 in let x = 4 + x in x + 2 ==> let x = 4 + 3 in x + 2

There are cases where the elimination of an inside let can generate an oportunity to coninue eliminating more lets, for example:
ex:     let x = (let y = 4 in y) in x + 2 ==> let x = 4 in x + 2 ==> 4 + 2
Sometimes the elimination of an inside let cant generate an oportunity to coninue eliminating more lets, for example:
1) let x = 5 + (let y = 4 in y) in x + 2 ==> let x = 5 + 4 in x + 2
2) let x = (let y = 4 in y + 3) in x + 2 ==> let x = 4 + 3 in x + 2

-}

-- subst x e1 e2 is e2[e1/x]
subst :: Name -> Expr -> Expr -> Expr
subst name e1 e2 = case e2 of
  Var name2 -> if name == name2 then e1 else e2
  Infix op e21 e22 -> Infix op (subst name e1 e21) (subst name e1 e22)
  If e21 e22 e23 -> If (subst name e1 e21) (subst name e1 e22) (subst name e1 e23)
  Let (name2, e21) e22 e23 -> if name == name2 then Let (name2, e21) e22 e23 else Let (name2, e21) (subst name e1 e22) (subst name e1 e23)
  App funcName args -> App funcName (map (subst name e1) args)
  IntLit x -> IntLit x
  BoolLit b -> BoolLit b

makePass :: Expr -> (Bool, Expr) -- (changed, expr)
makePass s@(Let (name, t) e1 e2) = case e1 of
  IntLit _ -> (True, subst name e1 e2)
  BoolLit _ -> (True, subst name e1 e2)
  _ ->
    let (changed1, e11) = makePass e1
        (changed2, e22) = makePass e2
     in (changed1 || changed2, Let (name, t) e11 e22)
makePass s@(Infix op e1 e2) =
  let (changed1, e11) = makePass e1
      (changed2, e22) = makePass e2
   in (changed1 || changed2, Infix op e11 e22)
makePass s@(If e1 e2 e3) =
  let (changed1, e11) = makePass e1
      (changed2, e22) = makePass e2
      (changed3, e33) = makePass e3
   in (changed1 || changed2 || changed3, If e11 e22 e33)
makePass s@(App funcName args) =
  let (changed, args1) = mapAccumL (\changed arg -> let (changed1, arg1) = makePass arg in (changed || changed1, arg1)) False args
   in (changed, App funcName args1)
makePass s = (False, s)

letElim :: Expr -> Expr
letElim e =
  let (changed, e1) = makePass e
   in if changed then letElim e1 else e1

letElimP :: Program -> Program
letElimP (Program defs expr) =
  let optDefs = map (\(FunDef t names expr) -> FunDef t names (letElim expr)) defs
      optExpr = letElim expr
   in Program optDefs optExpr
