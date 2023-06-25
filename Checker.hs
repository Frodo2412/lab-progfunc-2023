----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores,
-- o la lista de errores encontrados en otro caso.
----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}

module Checker where

-- se pueden agregar mas importaciones
-- en caso de ser necesario

import Control.Arrow (ArrowChoice (left, right))
import Control.Concurrent.STM (check)
import Control.Monad (zipWithM)
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Maybe
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

-- General utilities
getResult :: [a] -> Either [a] ()
getResult errors = case errors of
  [] -> Right ()
  _ -> Left errors

-- 2.1 Repeated names
checkRepeated = map Duplicated . foldl (\acc name -> if name `elem` acc then acc ++ [name] else acc) []

checkRepeatedNames :: [FunDef] -> Either [Error] ()
checkRepeatedNames funcs =
  getResult $
    let repeatedFunctions = checkRepeated $ map (\(FunDef (name, _) _ _) -> name) funcs
        repeatedArguments = concatMap (\(FunDef _ args _) -> checkRepeated args) funcs
     in repeatedFunctions ++ repeatedArguments

-- 2.2 Number of parameters

checkParameterNumber :: [FunDef] -> Either [Error] ()
checkParameterNumber funcs =
  getResult $
    mapMaybe
      ( \(FunDef (name, Sig argTypes _) argNames _) ->
          let actual = length argNames
              expected = length argTypes
           in if actual /= expected
                then Just $ ArgNumDef name actual expected
                else Nothing
      )
      funcs

-- 2.3 Undeclared names
checkUndeclaredInExpr (Var name) = do
  env <- ask
  if name `elem` env
    then return []
    else return [Undefined name]
checkUndeclaredInExpr (IntLit _) = return []
checkUndeclaredInExpr (BoolLit _) = return []
checkUndeclaredInExpr (Infix _ e1 e2) = do
  errors1 <- checkUndeclaredInExpr e1
  errors2 <- checkUndeclaredInExpr e2
  return $ errors1 ++ errors2
checkUndeclaredInExpr (If e1 e2 e3) = do
  errors1 <- checkUndeclaredInExpr e1
  errors2 <- checkUndeclaredInExpr e2
  errors3 <- checkUndeclaredInExpr e3
  return $ errors1 ++ errors2 ++ errors3
checkUndeclaredInExpr (Let (name, _) e1 e2) = do
  errors1 <- checkUndeclaredInExpr e1
  errors2 <- local (name :) $ checkUndeclaredInExpr e2
  return $ errors1 ++ errors2
checkUndeclaredInExpr (App name args) = do
  env <- ask
  errors <- mapM checkUndeclaredInExpr args
  if name `elem` env
    then return $ concat errors
    else return $ Undefined name : concat errors

checkUndeclaredNames :: Program -> Either [Error] ()
checkUndeclaredNames (Program funcs main) =
  getResult $
    concatMap
      ( \(FunDef _ names expr) ->
          runReader (checkUndeclaredInExpr expr) names
      )
      funcs
      ++ runReader (checkUndeclaredInExpr main) []

-- 2.4 Type Checking
checkProgramType :: Program -> Either [Error] ()
checkProgramType (Program defs main) = undefined

checkExprType (IntLit _) = lift $ checkLiteral TyInt
checkExprType (BoolLit _) = lift $ checkLiteral TyBool
checkExprType (Var name) =
  do
    vars <- ask
    let actual = name `lookup` vars
    _ <- maybe get put actual
    return []
checkExprType (Infix op left right) =
  ( case op of
      Add -> checkArithmeticOperator
      Sub -> checkArithmeticOperator
      Mult -> checkArithmeticOperator
      Div -> checkArithmeticOperator
      Eq -> checkEqualityOperator
      NEq -> checkEqualityOperator
      GTh -> checkEqualityOperator
      LTh -> checkEqualityOperator
      GEq -> checkArithmeticOperator
      LEq -> checkEqualityOperator
  )
    op
    left
    right
checkExprType (If cond thenExpr elseExpr) =
  do
    thenErrors <- checkExprType thenExpr
    thenType <- get
    elseErrors <- checkExprType elseExpr
    elseType <- get
    condErrors <- checkExprType cond
    condType <- get
    _ <- put thenType
    let localErrors = [Expected TyBool condType | condType /= TyBool] ++ [Expected thenType elseType | thenType /= elseType]
    let nestedErrors = condErrors ++ thenErrors ++ elseErrors
    return $ localErrors ++ nestedErrors
checkExprType (Let var inner outer) = undefined
checkExprType App = undefined

checkLiteral :: Type -> State Type [Error]
checkLiteral actual =
  do
    _ <- put actual
    return []

checkArithmeticOperator :: Op -> Expr -> Expr -> ReaderT Env (State Type) [Error]
checkEqualityOperator :: Op -> Expr -> Expr -> ReaderT Env (State Type) [Error]
--
checkProgram :: Program -> Checked
checkProgram prog@(Program defs main) = case do
  checkRepeatedNames defs
  checkParameterNumber defs
  checkUndeclaredNames prog of
  Right _ -> Ok
  Left errors -> Wrong errors
