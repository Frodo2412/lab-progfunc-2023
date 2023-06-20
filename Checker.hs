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
typeOf :: Expr -> State (Env, [TypedFun]) Type
--
typeOf (Var name) = do
  (variables, _) <- get
  case lookup name variables of
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
  (_, functions) <- get
  case lookup name functions of
    Just (Sig _ ret) -> return ret
    Nothing -> error ("Undefined variable " ++ name)
--
typeOf (Infix op _ _) = return (typeOfOp op)

-- Operator utilities
typeOfOp :: Op -> Type
typeOfOp op = case op of
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

-- Function utilities
typeOfFunc :: TypedFun -> Type
typeOfFunc (_, Sig _ ret) = ret

-- Chequea el tipo de la expresion
checkType :: Expr -> Type -> State (Env, [TypedFun]) [Error]
--
checkType (Infix op left right) expected = case op of
  Add -> checkArithmeticOperator left right expected
  Sub -> checkArithmeticOperator left right expected
  Mult -> checkArithmeticOperator left right expected
  Div -> checkArithmeticOperator left right expected
  Eq -> checkComparisonOperator left right expected
  NEq -> checkComparisonOperator left right expected
  GTh -> checkComparisonOperator left right expected
  LTh -> checkComparisonOperator left right expected
  GEq -> checkComparisonOperator left right expected
  LEq -> checkComparisonOperator left right expected
--
checkType (If cond thenExpr elseExpr) expected =
  do
    trueType <- typeOf thenExpr
    condErrors <- checkType cond TyBool
    thenErrors <- checkType thenExpr trueType
    elseErrors <- checkType elseExpr trueType
    return ([Expected expected trueType | expected /= trueType] ++ condErrors ++ thenErrors ++ elseErrors)
--
checkType (Let inner@(name, innerType) innerExpr outerExpr) expected =
  do
    trueType <- typeOf outerExpr
    innerErrors <- checkType innerExpr innerType
    _ <- modify (\(v, f) -> (inner : v, f))
    outerErrors <- checkType outerExpr trueType
    return ([Expected expected trueType | expected /= trueType] ++ innerErrors ++ outerErrors)
--
checkType func@(App name args) expected =
  do
    actual <- typeOf func
    args <- getArguments name
    let argNumbAppError = checkNumberOfArguments func args
    argTypeErrors <- checkArgumentTypes func args
    return ([Expected expected actual | expected /= actual] ++ argNumbAppError ++ argTypeErrors)
-- Covers var and literals
checkType expr expected =
  do
    actual <- typeOf expr
    return ([Expected expected actual | expected /= actual])

-- Operator utility functions
checkArithmeticOperator :: Expr -> Expr -> Type -> State (Env, [TypedFun]) [Error]
checkArithmeticOperator left right expected =
  do
    leftErrors <- checkType left TyInt
    rightErrors <- checkType right TyInt
    return ([Expected expected TyInt | expected /= TyInt] ++ leftErrors ++ rightErrors)

checkComparisonOperator :: Expr -> Expr -> Type -> State (Env, [TypedFun]) [Error]
checkComparisonOperator left right expected =
  do
    trueType <- typeOf left
    leftErrors <- checkType left trueType
    rightErrors <- checkType right trueType
    return ([Expected expected TyBool | expected /= TyBool] ++ leftErrors ++ rightErrors)

-- Function utility functions
checkNumberOfArguments (App name exprs) args =
  let actualLength = length exprs
      expectedLength = length args
   in [ArgNumApp name actualLength expectedLength | actualLength /= expectedLength]

checkArgumentTypes (App name exprs) args =
  do
    errors <- zipWithM checkType exprs args
    return (concat errors)

getArguments :: Name -> State (Env, [TypedFun]) [Type]
getArguments name =
  do
    (_, functions) <- get
    return
      ( case lookup name functions of
          Just (Sig args _) -> args
          Nothing -> []
      )

--
checkFunction (FunDef (name, Sig argTypes retType) argNames expr) =
  do
    _ <- modify (\(vars, funcs) -> (vars ++ zip argNames argTypes, funcs))
    checkType expr retType

checkMain expr =
  do
    trueType <- typeOf expr
    checkType expr trueType

--
checkProgramTypes :: Program -> Either [Error] ()
checkProgramTypes (Program defs expr) =
  let functions = map (\(FunDef def _ _) -> def) defs
      vars = []
      initialState = (vars, functions)
      functionErrors = concatMap (fst . flip runState initialState . checkFunction) defs
      mainErrors = evalState (checkMain expr) initialState
   in case functionErrors ++ mainErrors of
        [] -> Right ()
        errors -> Left errors

checkProgram :: Program -> Checked
checkProgram program@(Program defs _) =
  case do
    _ <- checkDefinitions defs
    _ <- checkNumberOfParameters defs
    _ <- checkNamesDefinedProgram program
    _ <- checkProgramTypes program
    return () of
    Right _ -> Ok
    Left errors -> Wrong errors
