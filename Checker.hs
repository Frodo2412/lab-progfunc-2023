----------------------------------------------------------------------------
-- LABORATORIO DE PROGRAMACION FUNCIONAL 2023
-- Módulo de chequeo
--
-- Se debe implementar la funcion checkProgram que, dado un AST
-- que representa un programa, retorna Ok en caso de no encontrar errores,
-- o la lista de errores encontrados en otro caso.
----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}

module Checker where

-- se pueden agregar mas importaciones
-- en caso de ser necesario

import Control.Monad (zipWithM)
import Control.Monad.Reader
import Control.Monad.Writer
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

-- 2.4 Type Checking (Ahora que lo veo esto podria ser un writer monad)
checkProgramType :: Program -> Either [Error] ()
checkProgramType (Program defs main) = undefined

checkExprType :: Expr -> ReaderT (Env, [FunDef]) (Writer [Error]) Type
checkExprType (IntLit _) = return TyInt
checkExprType (BoolLit _) = return TyBool
checkExprType (Var name) =
  do
    varType <- asks $ (name `lookup`) . fst
    return $ fromMaybe (error ("Undeclared variable " ++ name)) varType
checkExprType (Infix op left right) =
  ( case op of
      Add -> checkArithmeticOperator
      Sub -> checkArithmeticOperator
      Mult -> checkArithmeticOperator
      Div -> checkArithmeticOperator
      Eq -> checkComparissonOperator
      NEq -> checkComparissonOperator
      GTh -> checkComparissonOperator
      LTh -> checkComparissonOperator
      GEq -> checkComparissonOperator
      LEq -> checkComparissonOperator
  )
    left
    right
checkExprType (If cond thenExpr elseExpr) =
  do
    elseType <- checkExprType elseExpr
    thenType <- checkExprType thenExpr
    condType <- checkExprType cond
    let localErrors = [Expected TyBool condType | condType /= TyBool] ++ [Expected thenType elseType | thenType /= elseType]
    tell localErrors
    return thenType
checkExprType (Let var@(name, _type) inner outer) =
  do
    outerType <- local (\(vars, defs) -> (var : vars, defs)) $ checkExprType outer
    innerType <- checkExprType inner
    tell ([Expected _type innerType | _type /= innerType])
    return outerType
checkExprType (App name args) = undefined

-- do
--   actualTypes <- mapM checkExprType args
--   defs <- asks snd
--   let (Sig expectedTypes returnType) = fromMaybe (error "Function not defined " ++ name) (lookup name defs)
--   let argErrors = checkArguments expectedTypes actualTypes
--   let lengthError =
--         let actual = length args
--             expected = length argTypes
--          in [ArgNumApp actual expected | actual /= expected]
--   _ <- modify ((lengthError ++ argErrors) ++)
--   return returnType

checkArithmeticOperator left right =
  do
    rightType <- checkExprType right
    leftType <- checkExprType left
    let localErrors = [Expected TyInt leftType | leftType /= TyInt] ++ [Expected TyInt rightType | rightType /= TyInt]
    tell localErrors
    return TyInt

checkComparissonOperator left right =
  do
    rightType <- checkExprType right
    leftType <- checkExprType left
    tell [Expected leftType rightType | leftType /= rightType]
    return TyBool

checkArguments expected actual = concat $ zipWithM (\a e -> [Expected e a | e /= a]) expected actual

checkProgram :: Program -> Checked
checkProgram prog@(Program defs main) = case do
  checkRepeatedNames defs
  checkParameterNumber defs
  checkUndeclaredNames prog of
  Right _ -> Ok
  Left errors -> Wrong errors
