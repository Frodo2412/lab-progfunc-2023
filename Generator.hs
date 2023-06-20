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

translateName :: Name -> String
translateName name = "_" ++ name

translateOperator op = case op of
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

translateExpr :: Expr -> State [String] String
translateExpr (Var name) = return $ translateName name
translateExpr (IntLit value) = return $ show value
translateExpr (BoolLit value) = return $ show (if value then 1 else 0)
translateExpr (Infix op e1 e2) =
  do
    translatedLeft <- translateExpr e1
    translatedRight <- translateExpr e2
    let translatedOperator = translateOperator op
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

translateLet :: Expr -> State [String] Int
translateLet (Let (varName, _type) expr body) =
  do
    letCount <- gets length
    translatedBody <- translateExpr body
    newCount <- gets length
    let translatedVarName = translateName varName
    reversedLets <- gets reverse
    let diff = take (newCount - letCount) reversedLets
    let translatedLet = case diff of
          [] -> "int _let" ++ show newCount ++ "(int " ++ translatedVarName ++ "){\nreturn (" ++ translatedBody ++ "); };\n"
          functions -> "int _let" ++ show newCount ++ "(int " ++ translatedVarName ++ "){\n" ++ concat functions ++ "return (" ++ translatedBody ++ "); };\n"
    _ <- put (drop (newCount - letCount) reversedLets)
    _ <- modify (\s -> s ++ [translatedLet])
    return newCount

translateFunction :: FunDef -> String
translateFunction (FunDef (name, Sig params returnType) args body) =
  let translatedName = translateName name
      translatedParams = zipWith (\name _y -> "int " ++ translateName name) args params
      (translatedBody, localFunctions) = runState (translateExpr body) []
   in "int " ++ translatedName ++ "(" ++ intercalate "," translatedParams ++ "){\n" ++ concat localFunctions ++ "return (" ++ translatedBody ++ "); };\n"

genProgram :: Program -> String
genProgram (Program defs expr) =
  let translatedDefs = map translateFunction defs
      (translatedExpr, mainLets) = runState (translateExpr expr) []
   in "#include <stdio.h>\n" ++ concat translatedDefs ++ "int main() {\n" ++ concat mainLets ++ "printf(\"%d\\n\"," ++ translatedExpr ++ "); }\n"
