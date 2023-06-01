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
                  else Left (ArgNumDef name argsLength typesLength)
        )
        defs
    )



checkProgram :: Program -> Checked
checkProgram (Program definitions expressions) =
  case do
    _ <- checkDefinitions definitions
    _ <- checkNumberOfParameters definitions
    return () of
    Right _ -> Ok
    Left errors -> Wrong errors