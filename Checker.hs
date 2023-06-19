----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores,
-- o la lista de errores encontrados en otro caso.
----------------------------------------------------------------------------

module Checker where

-- se pueden agregar mas importaciones
-- en caso de ser necesario

import Control.Concurrent.STM (check)
import Control.Monad.State (MonadState (get), State)
import Data.List
import Data.Maybe
import Foreign.C (errnoToIOError)
import Syntax

-- CHECKER

data Checked = Ok | Wrong [Error]

data Error
  = Duplicated Name
  | Undefined Name
  | ArgNumDef Name Int Int
  | ArgNumApp Name Int Int
  | Expected Type Type

instance Show Error where
  show (Duplicated n) = "Duplicated declaration: " ++ n
  show (Undefined n) = "Undefined: " ++ n
  show (ArgNumDef n s d) =
    "The number of parameters in the definition of "
      ++ n
      ++ " doesn't match the signature ("
      ++ show d
      ++ " vs "
      ++ show s
      ++ ")"
  show (ArgNumApp n s d) =
    "The number of arguments in the application of: "
      ++ n
      ++ " doesn't match the signature ("
      ++ show d
      ++ " vs "
      ++ show s
      ++ ")"
  show (Expected ty ty') =
    "Expected: " ++ show ty ++ " Actual: " ++ show ty'

getDefs :: Program -> Defs
getDefs (Program defs _) = defs

-- 2.1 Repeticion de nombres

checkRepeatedFunctions :: Defs -> Checked
checkRepeatedFunctions defs =
  case foldl
    ( \(found, repeated) (FunDef typedFun _ _) ->
        let functionName = fst typedFun
         in if functionName `elem` found
              then (found, repeated ++ [functionName])
              else (functionName : found, repeated)
    )
    ([], [])
    defs of
    (_, []) -> Ok
    (_, repeated) -> Wrong (map Duplicated repeated)

checkRepeatedArgumentsSingleFunction :: FunDef -> [Name]
checkRepeatedArgumentsSingleFunction (FunDef _ args _) =
  snd
    ( foldl
        ( \(found, repeated) arg ->
            if arg `elem` found
              then (found, repeated ++ [arg])
              else (arg : found, repeated)
        )
        ([], [])
        args
    )

checkRepeatedArguments :: Defs -> Either [Error] ()
checkRepeatedArguments defs =
  case concatMap checkRepeatedArgumentsSingleFunction defs of
    [] -> Right ()
    errors -> Left (map Duplicated errors)

checkDefinitions :: Defs -> Either [Error] ()
checkDefinitions defs = case checkRepeatedFunctions defs of
  Ok -> checkRepeatedArguments defs
  Wrong errors -> case checkRepeatedArguments defs of
    Right _ -> Left errors
    Left errors' -> Left (errors ++ errors')

-- 2.2 Numero de Parametros

checkNumberOfParameters :: [FunDef] -> Either [Error] ()
checkNumberOfParameters defs =
  foldl
    ( \acc res -> case acc of
        Right _ -> case res of
          Right _ -> Right ()
          Left error -> Left [error]
        Left errors -> case res of
          Right _ -> Left errors
          Left error -> Left (errors ++ [error])
    )
    (Right ())
    ( map
        ( \(FunDef (name, Sig types _) args _) ->
            let argsLength = length args
                typesLength = length types
             in if argsLength == typesLength
                  then Right ()
                  else Left (ArgNumDef name typesLength argsLength)
        )
        defs
    )

-- 2.3 Nombres no declarados

checkNamesDefinedExpr :: FunDef -> [Name] -> [Error]
checkNamesDefinedExpr (FunDef _ args expr) defined =
  case expr of
    Infix _ expr1 expr2 ->
      checkNamesDefinedExpr (FunDef undefined args expr1) defined ++ checkNamesDefinedExpr (FunDef undefined args expr2) defined
    If expr1 expr2 expr3 ->
      checkNamesDefinedExpr (FunDef undefined args expr1) defined ++ checkNamesDefinedExpr (FunDef undefined args expr2) defined ++ checkNamesDefinedExpr (FunDef undefined args expr3) defined
    Let typedVar expr1 expr2 ->
      checkNamesDefinedExpr (FunDef undefined args expr1) defined ++ checkNamesDefinedExpr (FunDef undefined (fst typedVar : args) expr2) defined
    App name exprs ->
      ([Undefined name | name `notElem` defined]) ++ concatMap (\expr -> checkNamesDefinedExpr (FunDef undefined args expr) defined) exprs
    Var name -> ([Undefined name | name `notElem` args])
    IntLit _ -> []
    BoolLit _ -> []

checkNamesDefined :: Defs -> ([Error], [Name])
checkNamesDefined =
  foldl
    ( \(errors, defined) (FunDef typedFun args expr) ->
        let newNamesDefined = fst typedFun : defined
         in (errors ++ checkNamesDefinedExpr (FunDef typedFun args expr) newNamesDefined, newNamesDefined)
    )
    ([], [])

checkNamesDefinedExprs :: [Name] -> Expr -> [Error]
checkNamesDefinedExprs defined expr =
  checkNamesDefinedExpr (FunDef undefined defined expr) defined

checkNamesDefinedProgram :: Program -> Either [Error] ()
checkNamesDefinedProgram (Program definitions expression) =
  let (errors, defined) = checkNamesDefined definitions
   in case checkNamesDefinedExprs defined expression of
        [] -> Right ()
        errors' -> Left (errors ++ errors')

-- 2.4 Chequeo de tipos

-- Retorna el tipo de la expresion
typeOf :: Expr -> State Env Type
--
typeOf (Var name) = do
  env <- get
  case lookup name env of
    Just ty -> return ty
    Nothing -> error "Undefined variable"
--
typeOf (IntLit _) = return TyInt
--
typeOf (BoolLit _) = return TyBool
--
typeOf (If _ expr _) = typeOf expr
--
typeOf (Let _ _ expr) = typeOf expr
--
typeOf (App name _) = do
  env <- get
  case lookup name env of
    Just ty -> return ty
    Nothing -> error "Undefined variable"
--
typeOf (Infix op _ _) = return (checkInfixType op)

checkInfixType :: Op -> Type
checkInfixType op = case op of
  Add -> TyInt
  Sub -> TyInt
  Mult -> TyInt
  Div -> TyInt
  Eq -> TyBool
  NEq -> TyBool
  GTh -> TyBool
  LTh -> TyBool
  GEq -> TyBool
  LEq -> TyBool

--

checkProgramTypes = undefined

checkProgram :: Program -> Checked
checkProgram program =
  case do
    _ <- checkDefinitions $ getDefs program
    _ <- checkNumberOfParameters $ getDefs program
    _ <- checkNamesDefinedProgram program
    _ <- checkProgramTypes program
    return () of
    Right _ -> Ok
    Left errors -> Wrong errors
