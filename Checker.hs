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

{-
-- Given a Program, check if it is well typed this consists of several phases
If any of the phases fails it doesn't continue to the next one and returns de list of errors foud up to that point
If all phases are successful it returns Ok
the phases are:
1. Check that there are no names repeated, that includes function names and argument names
2. Check that the number of arguments in the definition of a function matches the signature
  ex:
    f :: ( Int ) -> Int
    f (x , y ) = x + y
3. Check that all names are defined
  ex:
    f :: ( Int ) -> Int
    f ( x ) = x + z + g ( s )

    main = h ( x )

  output:
    Undefined: z
    Undefined: g
    Undefined: s
    Undefined: h
    Undefined: x
4. Check that the types of the arguments in the application match the signature
This means the following:
 * A boolean literal is of type Bool
  * An integer literal is of type Int
  * A infix expression e op e' depends of operator op
    * If op is a relational operator then the subexpressions e and e' must be of the same type and the result is of type Bool
    * If op is a arithmetic operator then the subexpressions e and e' must be of type Int and the result is of type Int
  * The type of an if b then e else e' expression is the type of e and e' and b must be of type Bool
  * The type of an expression let x :: t = e in e' is t if e is of type t and e' is of type t
  * The type of an application of f (e1,..., ek) is t if f :: (t1,...,tn) -> t. It must be checked that the amount of
    k arguments provided in the application matches the amount of arguments in the signature (independently that k=n)
    and that each ei is of type ti. We have to check the type of min(k,n) arguments. Meaning that if k > n
    then only the first n arguments are checked and if k <= n then only the first k arguments are checked.
    ex:
      f :: ( Int , Int ) -> Int
      f (x , y ) = if x then x + y else x == True

      main = False == f ( True ,2+( True *4) ,5)

      output:
        Expected: Bool Actual: Int
        Expected: Int Actual: Bool
        Expected: Int Actual: Bool
        Expected: Bool Actual: Int
        The number of arguments in the application of: f doesn’t match the signature (3 vs 2)
        Expected: Int Actual: Bool
        Expected: Int Actual: Bool
  -}

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

-- Checks that there are no repeted function names in a list of definitions
checkRepeatedFunctions :: Defs -> Checked
checkRepeatedFunctions defs =
  case foldl
    ( \(found, repeated) (FunDef typedFun _ _) ->
        let functionName = fst typedFun
         in if functionName `elem` found
              then (found, functionName : repeated)
              else (functionName : found, repeated)
    )
    ([], [])
    defs of
    (_, []) -> Ok
    (_, repeated) -> Wrong (map Duplicated repeated)

-- Checks for repeted names in a signle function
checkRepeatedArgumentsSingleFunction :: FunDef -> [Name]
checkRepeatedArgumentsSingleFunction (FunDef _ args _) =
  snd
    ( foldl
        ( \(found, repeated) arg ->
            if arg `elem` found
              then (found, arg : repeated)
              else (arg : found, repeated)
        )
        ([], [])
        args
    )

-- Checks for repeated arguments in a list of definitions
checkRepeatedArguments :: Defs -> Either [Error] ()
checkRepeatedArguments defs =
  case concatMap checkRepeatedArgumentsSingleFunction defs of
    [] -> Right ()
    errors -> Left (map Duplicated errors)

-- 1) Checks that defenitions have no repeted fucntion names and no repeted argument names
checkDefinitions :: Defs -> Either [Error] ()
checkDefinitions defs = case checkRepeatedFunctions defs of
  Ok -> checkRepeatedArguments defs
  Wrong errors -> case checkRepeatedArguments defs of
    Right _ -> Left errors
    Left errors' -> Left (errors ++ errors')

-- 2) Checks that the number of arguments in the application of a function matches the signature
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
                  else Left (ArgNumDef name argsLength typesLength)
        )
        defs
    )

-- Checks that all names are defined in an Expr
-- Takes a FunDef and a list of names found up to that point and returns a list of errors
-- If given a operator expression it checks that both subexpressions are defined and if not returns a list of errors, if given the case, returned by both subexpressions, is concatenated
-- If given a if expression it checks that all subexpressions are defined and if not returns a list of errors, if given the case, returned by all subexpressions, is concatenated
-- If given a let expression it checks that the expression is defined and if not returns a list of errors, if given the case, returned by the expression, is concatenated
-- If given a application expression it checks that all subexpressions are defined and if not returns a list of errors, if given the case, returned by all subexpressions, is concatenated
-- If given a variable expression it checks that the variable is defined and if not returns a list of errors, if given the case, returned by the variable, is concatenated
-- If given a integer literal expression it returns []
-- If given a boolean literal expression it returns []
-- The names declared can be found in the FunDef as an argument or a recursive call to this function and the function names names found up to FunDef declaration
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
      concatMap (\expr -> checkNamesDefinedExpr (FunDef undefined args expr) defined) exprs ++ ([Undefined name | name `notElem` defined])
    Var name -> ([Undefined name | name `notElem` args])
    IntLit _ -> []
    BoolLit _ -> []

-- Checks that all names are defined in definitions
-- Returns a (list of errors or ok, list of function names defined) tuple
-- Go through all definitions and check that all names are defined in each definition
-- Keeps in a list the names defined in each definition for the next definition so each iteration
-- keeps the names defined in the previous definitions plus the name of the fuction in the step
checkNamesDefined :: Defs -> ([Error], [Name])
checkNamesDefined =
  foldl
    ( \(errors, defined) (FunDef typedFun args expr) ->
        let newNamesDefined = fst typedFun : defined
         in (errors ++ checkNamesDefinedExpr (FunDef typedFun args expr) newNamesDefined, newNamesDefined)
    )
    ([], [])

-- Checks that all names are defined in an Expr of Program given a list of function names defined
-- Return a list of errors or ok
checkNamesDefinedExprs :: [Name] -> Expr -> [Error]
checkNamesDefinedExprs defined expr =
  checkNamesDefinedExpr (FunDef undefined defined expr) defined

-- 3) Checks that all names are defined
-- First checks that all names are defined in definitions
-- Then checks that all names are defined in main expression
-- In any case resuls of ok or concatenation of errors of both phases
checkNamesDefinedProgram :: Program -> Either [Error] ()
checkNamesDefinedProgram (Program definitions expression) =
  let (errors, defined) = checkNamesDefined definitions
   in case checkNamesDefinedExprs defined expression of
        [] -> Right ()
        errors' -> Left (errors ++ errors')

-- --------------------------------------------------

{-
4. Check that the types of the arguments in the application match the signature
This means the following:
 * A boolean literal is of type Bool
  * An integer literal is of type Int
  * A infix expression e op e' depends of operator op
    * If op is a relational operator then the subexpressions e and e' must be of the same type and the result is of type Bool
    * If op is a arithmetic operator then the subexpressions e and e' must be of type Int and the result is of type Int
  * The type of an if b then e else e' expression is the type of e and e' and b must be of type Bool
  * The type of an expression let x :: t = e in e' is t if e is of type t and e' is of type t
  * The type of an application of f (e1,..., ek) is t if f :: (t1,...,tn) -> t. It must be checked that the amount of
    k arguments provided in the application matches the amount of arguments in the signature (independently that k=n)
    and that each ei is of type ti. We have to check the type of min(k,n) arguments. Meaning that if k > n
    then only the first n arguments are checked and if k <= n then only the first k arguments are checked.
    ex:
      f :: ( Int , Int ) -> Int
      f (x , y ) = if x then x + y else x == True

      main = False == f ( True ,2+( True *4) ,5)

      output:
        Expected: Bool Actual: Int
        Expected: Int Actual: Bool
        Expected: Int Actual: Bool
        Expected: Bool Actual: Int
        The number of arguments in the application of: f doesn’t match the signature (3 vs 2)
        Expected: Int Actual: Bool
        Expected: Int Actual: Bool
-}
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
            else (mathType, [Expected expectedType mathType] ++ errors ++ errors')

checkOrderOperator :: Expr -> Env -> FuncEnv -> Maybe Type -> (Type, [Error])
checkOrderOperator (Infix op expr1 expr2) env functionEnv expectedType =
  checkMathOperator (Infix op expr1 expr2) env functionEnv expectedType TyBool

checkEqualityOperator :: Expr -> Env -> FuncEnv -> Maybe Type -> (Type, [Error])
checkEqualityOperator (Infix op expr1 expr2) env functionEnv _ =
  let firstArgType = obtainType expr1 env
      secondArgType = obtainType expr2 env
      (type', errors) = checkExprType expr1 env functionEnv (Just firstArgType)
      (type'', errors') = checkExprType expr2 env functionEnv (Just secondArgType)
   in if firstArgType == secondArgType
        then (TyBool, errors ++ errors')
        else (TyBool, [Expected firstArgType secondArgType] ++ errors ++ errors')

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
      (type''', errors'') = checkExprType expr3 env functionEnv expectedType
   in (type'', errors ++ errors' ++ errors'')
checkExprType (Let typedVar expr1 expr2) env functionEnv expectedType =
  let (type', errors) = checkExprType expr1 env functionEnv (Just (snd typedVar))
      (type'', errors') = checkExprType expr2 (setType env typedVar) functionEnv expectedType
   in (type'', errors ++ errors')
checkExprType (App name exprs) env functionEnv expectedType =
  let argumentsTypes = fromMaybe [TyInt] (getFuncTypes functionEnv name)
      parameterTypes = map (`obtainType` env) exprs
      functionReturnType = fromMaybe TyInt (getType env name)
      lenghtArguments = length argumentsTypes
      lenghtExprs = length exprs
      minLenght = min lenghtArguments lenghtExprs
   in (functionReturnType, auxCheckApplicationSigType expectedType functionReturnType ++ auxCheckApplicationLength name lenghtArguments lenghtExprs ++ auxCheckApplicationTypes argumentsTypes parameterTypes minLenght)

envTuple :: FunDef -> (Name, Type)
envTuple (FunDef (name, Sig argumentsTypes returnType) arguments expr) = (name, returnType)

funcEnvTuple :: FunDef -> (Name, [Type])
funcEnvTuple (FunDef (name, Sig argumentsTypes returnType) arguments expr) = (name, argumentsTypes)

{-
Function that takes a FunDef, Env and returns Env loaded with the function
-}
loadEnv :: FunDef -> Env -> Env
loadEnv funcDef env = envTuple funcDef : env

{-
Function iterates over the list of FunDefs and does the following:
1. Loads the function into the Env
2. Loads the function into the FuncEnv
-}
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

{-
Function, given Defs and loaded Env and FuncEnv with function signatures, that iterates over the list of FunDefs and does the following:
Checks types of the function using checkExprType
-}
checkDefsTypes :: Defs -> Env -> FuncEnv -> [Error]
checkDefsTypes [] env functionEnv = []
checkDefsTypes (funcDef : funcDefs) env functionEnv =
  let (type', errors) = checkExprType (getExpr funcDef) (loadArgs funcDef env) functionEnv (Just (getReturnType funcDef))
   in errors ++ checkDefsTypes funcDefs env functionEnv

checkMain :: Expr -> Env -> FuncEnv -> [Error]
checkMain expr env functionEnv =
  let (type', errors) = checkExprType expr env functionEnv Nothing
   in errors

{-
4. Check that the types of the arguments in the application match the signature
This means the following:
 * A boolean literal is of type Bool
  * An integer literal is of type Int
  * A infix expression e op e' depends of operator op
    * If op is a relational operator then the subexpressions e and e' must be of the same type and the result is of type Bool
    * If op is a arithmetic operator then the subexpressions e and e' must be of type Int and the result is of type Int
  * The type of an if b then e else e' expression is the type of e and e' and b must be of type Bool
  * The type of an expression let x :: t = e in e' is t if e is of type t and e' is of type t
  * The type of an application of f (e1,..., ek) is t if f :: (t1,...,tn) -> t. It must be checked that the amount of
    k arguments provided in the application matches the amount of arguments in the signature (independently that k=n)
    and that each ei is of type ti. We have to check the type of min(k,n) arguments. Meaning that if k > n
    then only the first n arguments are checked and if k <= n then only the first k arguments are checked.
    ex:
      f :: ( Int , Int ) -> Int
      f (x , y ) = if x then x + y else x == True

      main = False == f ( True ,2+( True *4) ,5)

      output:
        Expected: Bool Actual: Int
        Expected: Int Actual: Bool
        Expected: Int Actual: Bool
        Expected: Bool Actual: Int
        The number of arguments in the application of: f doesn’t match the signature (3 vs 2)
        Expected: Int Actual: Bool
        Expected: Int Actual: Bool
-}   
checkProgramTypes :: Program -> Either [Error] ()
checkProgramTypes (Program defs expr) =
  let (env, functionEnv) = loadEnvs defs [] []
      errors = checkDefsTypes defs env functionEnv ++ checkMain expr env functionEnv
   in if null errors
        then Right ()
        else Left errors