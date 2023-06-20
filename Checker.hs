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

import Control.Monad (zipWithM)
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

-- 2.1 Repeticion de nombres
checkRepeated = map Duplicated . foldl (\acc name -> if name `elem` acc then acc ++ [name] else acc) []

checkRepeatedNames :: [FunDef] -> Either [Error] ()
checkRepeatedNames funcs =
  let repeatedFunctions = checkRepeated $ map (\(FunDef (name, _) _ _) -> name) funcs
      repeatedArguments = concatMap (\(FunDef _ args _) -> checkRepeated args) funcs
   in case repeatedFunctions ++ repeatedArguments of
        [] -> Right ()
        errors -> Left errors

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

checkProgram :: Program -> Checked
checkProgram prog@(Program defs main) = case do
  checkRepeatedNames defs
  checkParameterNumber defs of
  Right _ -> Ok
  Left errors -> Wrong errors
