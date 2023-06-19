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

setType :: Env -> TypedVar -> Env
setType env typedVar = typedVar : env

getType :: Env -> Name -> Maybe Type
getType [] _ = Nothing
getType ((name, type') : env) name' =
  if name == name'
    then Just type'
    else getType env name'

type FuncEnv = [(Name, [Type])]

getFuncTypes :: FuncEnv -> Name -> Maybe [Type]
getFuncTypes [] _ = Nothing
getFuncTypes ((name, types) : funcEnv) name' =
  if name == name'
    then Just types
    else getFuncTypes funcEnv name'

obtainType :: Expr -> Env -> Type
obtainType (Var name) env =
  fromMaybe TyInt (getType env name)
obtainType (IntLit _) _ = TyInt
obtainType (BoolLit _) _ = TyBool
obtainType (Infix op expr1 expr2) env =
  case op of
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
obtainType (If expr1 expr2 expr3) env = obtainType expr2 env
obtainType (Let typedVar expr1 expr2) env = obtainType expr2 (setType env typedVar)
obtainType (App name exprs) env =
  fromMaybe TyInt (getType env name)

checkMathOperator :: Expr -> Env -> FuncEnv -> Maybe Type -> Type -> (Type, [Error])
checkMathOperator (Infix op expr1 expr2) env functionEnv expectedType mathType =
  let (type', errors) = checkExprType expr1 env functionEnv (Just TyInt)
      (type'', errors') = checkExprType expr2 env functionEnv (Just TyInt)
   in case expectedType of
        Nothing -> (mathType, errors ++ errors')
        Just expectedType ->
          if expectedType == mathType
            then (mathType, errors ++ errors')
            else (mathType, errors ++ errors' ++ [Expected expectedType mathType])

checkOrderOperator :: Expr -> Env -> FuncEnv -> Maybe Type -> (Type, [Error])
checkOrderOperator (Infix op expr1 expr2) env functionEnv expectedType =
  checkMathOperator (Infix op expr1 expr2) env functionEnv expectedType TyBool

checkEqualityOperator :: Expr -> Env -> FuncEnv -> Maybe Type -> (Type, [Error])
checkEqualityOperator (Infix op expr1 expr2) env functionEnv expectedType =
  let firstArgType = obtainType expr1 env
      secondArgType = obtainType expr2 env
      (type', errors) = checkExprType expr1 env functionEnv (Just firstArgType)
      (type'', errors') = checkExprType expr2 env functionEnv (Just secondArgType)
      errorsExpr =
        if firstArgType == secondArgType
          then errors ++ errors'
          else [Expected firstArgType secondArgType] ++ errors ++ errors'
   in case expectedType of
        Nothing -> (TyBool, errorsExpr)
        Just expectedType ->
          if expectedType == TyBool
            then (TyBool, errorsExpr)
            else (TyBool, errorsExpr ++ [Expected expectedType TyBool])

checkArithemticOperator :: Expr -> Env -> FuncEnv -> Maybe Type -> (Type, [Error])
checkArithemticOperator (Infix op expr1 expr2) env functionEnv expectedType =
  checkMathOperator (Infix op expr1 expr2) env functionEnv expectedType TyInt

auxCheckApplicationSigType :: Maybe Type -> Type -> [Error]
auxCheckApplicationSigType expectedType returnType =
  case expectedType of
    Nothing -> []
    Just expectedType -> ([Expected expectedType returnType | expectedType /= returnType])

auxCheckApplicationLength :: Name -> Int -> Int -> [Error]
auxCheckApplicationLength name expectedLength actualLength =
  [ArgNumApp name expectedLength actualLength | expectedLength /= actualLength]

auxCheckApplicationTypes :: [Type] -> [Type] -> Int -> [Error]
auxCheckApplicationTypes expectedTypes actualTypes amountToCheck =
  let expectedTypes' = take amountToCheck expectedTypes
      actualTypes' = take amountToCheck actualTypes
   in [Expected expectedType actualType | (expectedType, actualType) <- zip expectedTypes' actualTypes', expectedType /= actualType]

checkExprType :: Expr -> Env -> FuncEnv -> Maybe Type -> (Type, [Error])
checkExprType (Var name) env functionEnv expectedType =
  case getType env name of
    Nothing -> (TyInt, [Undefined name])
    Just type' -> case expectedType of
      Nothing -> (type', [])
      Just expectedType ->
        if type' == expectedType
          then (type', [])
          else (type', [Expected expectedType type'])
checkExprType (IntLit _) _ _ expectedType =
  case expectedType of
    Nothing -> (TyInt, [])
    Just expectedType ->
      if expectedType == TyInt
        then (TyInt, [])
        else (TyInt, [Expected expectedType TyInt])
checkExprType (BoolLit _) _ _ expectedType =
  case expectedType of
    Nothing -> (TyBool, [])
    Just expectedType ->
      if expectedType == TyBool
        then (TyBool, [])
        else (TyBool, [Expected expectedType TyBool])
checkExprType (Infix op expr1 expr2) env functionEnv expectedType =
  case op of
    Add -> checkArithemticOperator (Infix op expr1 expr2) env functionEnv expectedType
    Sub -> checkArithemticOperator (Infix op expr1 expr2) env functionEnv expectedType
    Mult -> checkArithemticOperator (Infix op expr1 expr2) env functionEnv expectedType
    Div -> checkArithemticOperator (Infix op expr1 expr2) env functionEnv expectedType
    Eq -> checkEqualityOperator (Infix op expr1 expr2) env functionEnv expectedType
    NEq -> checkEqualityOperator (Infix op expr1 expr2) env functionEnv expectedType
    GTh -> checkOrderOperator (Infix op expr1 expr2) env functionEnv expectedType
    LTh -> checkOrderOperator (Infix op expr1 expr2) env functionEnv expectedType
    GEq -> checkOrderOperator (Infix op expr1 expr2) env functionEnv expectedType
    LEq -> checkOrderOperator (Infix op expr1 expr2) env functionEnv expectedType
checkExprType (If expr1 expr2 expr3) env functionEnv expectedType =
  let (type', errors) = checkExprType expr1 env functionEnv (Just TyBool)
      (type'', errors') = checkExprType expr2 env functionEnv expectedType
      (type''', errors'') = checkExprType expr3 env functionEnv (Just type'')
   in (type'', errors ++ errors' ++ errors'')
checkExprType (Let typedVar expr1 expr2) env functionEnv expectedType =
  let retType = obtainType expr2 env
      (type', errors) = checkExprType expr1 env functionEnv (Just (snd typedVar))
      (type'', errors') = checkExprType expr2 (setType env typedVar) functionEnv (Just retType)
   in case expectedType of
        Nothing -> (type'', errors ++ errors')
        Just someType ->
          let allErrors =
                if Just retType == expectedType
                  then errors ++ errors'
                  else Expected someType retType : (errors ++ errors')
           in (someType, allErrors)
checkExprType (App name exprs) env functionEnv expectedType =
  let argumentsTypes = fromMaybe [TyInt] (getFuncTypes functionEnv name)
      parameterTypes = map (`obtainType` env) exprs
      functionReturnType = fromMaybe TyInt (getType env name)
      lenghtArguments = length argumentsTypes
      lenghtExprs = length exprs
      minLenght = min lenghtArguments lenghtExprs
      numberOfParameterErrors = auxCheckApplicationLength name lenghtArguments lenghtExprs
      argumentErrors = concatMap (\expr -> snd (checkExprType expr env functionEnv Nothing)) exprs
      errors = auxCheckApplicationSigType expectedType functionReturnType ++ numberOfParameterErrors ++ auxCheckApplicationTypes argumentsTypes parameterTypes minLenght ++ argumentErrors
   in (functionReturnType, errors)

envTuple :: FunDef -> (Name, Type)
envTuple (FunDef (name, Sig argumentsTypes returnType) arguments expr) = (name, returnType)

funcEnvTuple :: FunDef -> (Name, [Type])
funcEnvTuple (FunDef (name, Sig argumentsTypes returnType) arguments expr) = (name, argumentsTypes)

loadEnv :: FunDef -> Env -> Env
loadEnv funcDef env = envTuple funcDef : env

loadEnvs :: Defs -> Env -> FuncEnv -> (Env, FuncEnv)
loadEnvs [] env functionEnv = (env, functionEnv)
loadEnvs (funcDef : funcDefs) env functionEnv =
  let env' = loadEnv funcDef env
      functionEnv' = funcEnvTuple funcDef : functionEnv
   in loadEnvs funcDefs env' functionEnv'

getExpr :: FunDef -> Expr
getExpr (FunDef (name, Sig argumentsTypes returnType) arguments expr) = expr

getReturnType :: FunDef -> Type
getReturnType (FunDef (name, Sig argumentsTypes returnType) arguments expr) = returnType

loadArgs :: FunDef -> Env -> Env
loadArgs (FunDef (name, Sig argumentsTypes returnType) arguments expr) env = zip arguments argumentsTypes ++ env

checkDefsTypes :: Defs -> Env -> FuncEnv -> [Error]
checkDefsTypes [] env functionEnv = []
checkDefsTypes (funcDef : funcDefs) env functionEnv =
  let (type', errors) = checkExprType (getExpr funcDef) (loadArgs funcDef env) functionEnv (Just (getReturnType funcDef))
   in errors ++ checkDefsTypes funcDefs env functionEnv

checkMain :: Expr -> Env -> FuncEnv -> [Error]
checkMain expr env functionEnv =
  let (type', errors) = checkExprType expr env functionEnv Nothing
   in errors

checkProgramTypes :: Program -> Either [Error] ()
checkProgramTypes (Program defs expr) =
  let (env, functionEnv) = loadEnvs defs [] []
      errors = checkDefsTypes defs env functionEnv ++ checkMain expr env functionEnv
   in if null errors
        then Right ()
        else Left errors