----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de generación de código C
--
-- Se debe implementar la función genProgram,
-- que dado un AST que representa un programa válido
-- genera el código C correspondiente.
----------------------------------------------------------------------------

module Generator where

-- se pueden agregar mas importaciones
-- en caso de ser necesario

import Control.Monad.State
import Data.List
import Syntax

-- CODE GENERATOR

{-
We want to translate the AST into C code, with the following observations:
- Since C doesnt have boolean values, we will use 0 for False and 1 for True
- for avoiding name conflicts wiht function names and reserved words, we will add a prefix to all the names "_"
- for avoiding name conflicts with variables, we will add a prefix to all the variables "_"
- When an expression of type "let" ocurrs, we do the following:
    - in C translating, let x :: t = e in e' is equivalent to a call to a funcion wiht name _let<number> where <number> is a number that is incremented each time a let ocurrs
     the function only has one parameter and is of type t, and the body of the function is e'

ex:
    input: main = 23 + 4
    output:
        #include <stdio.h>
        int main() {
        printf("%d\n",(23 + 4)); }

    input:
    outut:

    input:
        double :: (Int) -> Int
        double(x) = 2 * x

        main = 23 + double(2)
    outut:
        #include <stdio.h>
        int _double(int _x){
        return ((2 * _x)); };
        int main() {
        printf("%d\n",(23 + _double(2))); }

    input:
        foo :: (Int,Bool) -> Int
        foo(x,b) = if b then x * x else x + 2

        main = 23 + foo(2,True) + foo(3,False)
    outut:
        #include <stdio.h>
        int _foo(int _x,int _b){
        return (_b?(_x * _x):(_x + 2)); };
        int main() {
        printf("%d\n",((23 + _foo(2,1)) + _foo(3,0))); }

    input:
        foo :: (Int,Int) -> Int
        foo(x,y) = (let x :: Int = y in x)
                    + (let y :: Int = x + y in y)

        main = foo(2,4)
    outut:
        #include <stdio.h>
        int _foo(int _x,int _y){
        int _let0(int _x){
        return (_x); };
        int _let1(int _y){
        return (_y); };
        return ((_let0(_y) + _let1((_x + _y)))); };
        int main() {
        printf("%d\n",_foo(2,4)); }
-}

translateName :: Name -> String
translateName name = "_" ++ name

translateType :: Type -> String
translateType TyInt = "int"
translateType TyBool = "int"

-- translateExpr :: Expr -> Int -> (Int, String)
-- translateExpr (Var name) count = (count, translateName name)
-- translateExpr (IntLit value) count = (count, show value)
-- translateExpr (BoolLit value) count = (count, show (if value then 1 else 0))
-- translateExpr (Infix op left right) count =
--   let translatedOp =
--         ( case op of
--             Add -> " + "
--             Sub -> " - "
--             Mult -> " * "
--             Div -> " / "
--             Eq -> " == "
--             NEq -> " != "
--             GTh -> " > "
--             LTh -> " < "
--             GEq -> " >= "
--             LEq -> " <= "
--         )
--       (count1, translatedLeft) = translateExpr left count
--       (count2, translatedRight) = translateExpr right count1
--    in (count2, "(" ++ translatedLeft ++ translatedOp ++ translatedRight ++ ")")
-- translateExpr (If condition thenExpr elseExpr) count =
--   let (count1, translatedCondition) = translateExpr condition count
--       (count2, translatedThen) = translateExpr thenExpr count1
--       (count3, translatedElse) = translateExpr elseExpr count2
--    in (count3, translatedCondition ++ "?" ++ translatedThen ++ ":" ++ translatedElse)
-- translateExpr (Let (name, _type) value body) count =
--   let letName = "_let" ++ show count
--       (count1, translatedValue) = translateExpr value (count + 1)
--       (count2, translatedBody) = translateExpr body count1
--    in (count2, "int " ++ letName ++ "(" ++ translateType _type ++ " " ++ translateName name ++ "){\nreturn (" ++ translatedBody ++ "); };\n" ++ letName ++ "(" ++ translatedValue ++ ")")
-- translateExpr (App name args) count =
--   let translatedName = translateName name
--       (count1, translatedArgs) = mapAccumL (flip translateExpr) count args
--    in (count1, translatedName ++ "(" ++ intercalate "," translatedArgs ++ ")")

translateOperator :: Expr -> String
translateOperator (Infix op _ _) =
  case op of
    Add -> " + "
    Sub -> " - "
    Mult -> " * "
    Div -> " / "
    Eq -> "=="
    NEq -> "!="
    GTh -> ">"
    LTh -> "<"
    GEq -> ">="
    LEq -> "<="

translateExpr :: Expr -> State [String] String -- State (Los lets que tengo que hacer, el codigo que llama a los lets)
translateExpr (Var name) = return $ translateName name
translateExpr (IntLit value) = return $ show value
translateExpr (BoolLit value) = return $ show (if value then 1 else 0)
translateExpr (Infix op e1 e2) =
  do
    translatedLeft <- translateExpr e1
    translatedRight <- translateExpr e2
    let translatedOperator = translateOperator (Infix op e1 e2)
    return ("(" ++ translatedLeft ++ translatedOperator ++ translatedRight ++ ")")
translateExpr (If condition thenExpr elseExpr) =
  do
    translatedCondition <- translateExpr condition
    translatedThen <- translateExpr thenExpr
    translatedElse <- translateExpr elseExpr
    return (translatedCondition ++ "?" ++ translatedThen ++ ":" ++ translatedElse)
translateExpr (App name args) =
  do
    translatedArgs <- mapM translateExpr args
    return (translateName name ++ "(" ++ intercalate "," translatedArgs ++ ")")
translateExpr s@(Let _ e1 _) =
  do
    translatedExpr <- translateExpr e1
    letNumber <- translateLet s
    return ("_let" ++ show letNumber ++ "(" ++ translatedExpr ++ ")")

translateLet :: Expr -> State [String] Int -- State (Los lets que tengo que hacer, el codigo que llama a los lets)
translateLet (Let (varName, _type) expr body) =
  do
    letCount <- gets length
    translatedBody <- translateExpr body
    newCount <- gets length
    let translatedVarName = translateName varName
    let translatedType = translateType _type
    reversedLets <- gets reverse
    let diff = take (newCount - letCount) reversedLets
    let translatedLet = case diff of
          [] -> "int " ++ "_let" ++ show newCount ++ "(" ++ translatedType ++ " " ++ translatedVarName ++ "){\nreturn (" ++ translatedBody ++ "); };\n"
          functions -> "int " ++ "_let" ++ show newCount ++ "(" ++ translatedType ++ " " ++ translatedVarName ++ "){\n" ++ concat functions ++ "return (" ++ translatedBody ++ "); };\n"
    _ <- put (drop (newCount - letCount) reversedLets)
    _ <- modify (\s -> s ++ [translatedLet])
    return newCount

translateFunction :: FunDef -> String
translateFunction (FunDef (name, Sig params returnType) args body) =
  let translatedName = translateName name
      translatedParams = zipWith (\name _y -> translateType _y ++ " " ++ translateName name) args params
      translatedReturnType = translateType returnType
      (translatedBody, localFunctions) = runState (translateExpr body) []
   in translatedReturnType ++ " " ++ translatedName ++ "(" ++ intercalate "," translatedParams ++ "){\n" ++ concat localFunctions ++ "return (" ++ translatedBody ++ "); };\n"

genProgram :: Program -> String
genProgram (Program defs expr) =
  let translatedDefs = map translateFunction defs
      (translatedExpr, mainLets) = runState (translateExpr expr) []
   in "#include <stdio.h>\n" ++ concat translatedDefs ++ "int main() {\n" ++ concat mainLets ++ "printf(\"%d\\n\"," ++ translatedExpr ++ "); }\n"
