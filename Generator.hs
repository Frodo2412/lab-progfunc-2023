----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de generación de código C
--
-- Se debe implementar la función genProgram,
-- que dado un AST que representa un programa válido
-- genera el código C correspondiente.
----------------------------------------------------------------------------

module Generator where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

import Data.List

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

translateExpr :: Expr -> String
translateExpr (Var name) = translateName name
translateExpr (IntLit value) = show value
translateExpr (BoolLit value) = show (if value then 1 else 0)
translateExpr (Infix op left right) =
    let
        translatedOp = case op of
            Add -> " + "
            Sub -> " - "
            Mult -> " * "
            Div -> " / "
            Eq -> " == "
            NEq -> " != "
            GTh -> " > "
            LTh -> " < "
            GEq -> " >= "
            LEq -> " <= "
    in
        "(" ++ translateExpr left ++ translatedOp ++ translateExpr right ++ ")"
translateExpr (If condition thenExpr elseExpr) =
    "(" ++ translateExpr condition ++ "?" ++ translateExpr thenExpr ++ ":" ++ translateExpr elseExpr ++ ")"
translateExpr (Let (name, _type) value body) =
    let
        translatedName = translateName name
        translatedValue = translateExpr value
        translatedBody = translateExpr body
    in
        "int " ++ translatedName ++ " = " ++ translatedValue ++ ";\n" ++ translatedBody
translateExpr (App name args) =
    let
        translatedName = translateName name
        translatedArgs = map translateExpr args
    in
        translatedName ++ "(" ++ intercalate "," translatedArgs ++ ")"

translateFunction :: FunDef -> String
translateFunction (FunDef (name, Sig params returnType) args body) =
    let
        translatedName = translateName name
        translatedParams = zipWith (\ name _y -> translateName name) args params
        translatedReturnType = translateType returnType
        translatedBody = translateExpr body
    in
        translatedReturnType ++ " " ++ translatedName ++ "(" ++ intercalate "," translatedParams ++ ") {\n" ++ translatedBody ++ "};\n"


genProgram :: Program -> String
genProgram (Program defs expr) =
    let
        translatedDefs = map translateFunction defs
        translatedExpr = translateExpr expr
    in
        "#include <stdio.h>" ++ intercalate "\n" translatedDefs ++ "\nint main() {\nprintf(\"%d\\n\"," ++ translatedExpr ++ "); }\n"
    
