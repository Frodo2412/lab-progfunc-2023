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
import Data.List


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
subst = undefined

letElimP :: Program -> Program 
letElimP prog = prog


