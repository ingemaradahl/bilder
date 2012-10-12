{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.Types where

import CompilerTypes
import TypeChecker.Utils (paramType)
import FrontEnd.AbsGrammar

import Data.Map (Map)

class Global a where
  ident ∷ a → String
  location ∷ a → Location
  position ∷ a → Position
  position = snd . location

data Function = Function {
    functionName ∷ String
  , functionLocation ∷ Location
  , retType ∷ Type
  , parameters ∷ [Param]
  , statements ∷ [Stm]
}

instance Global Function where
  ident = functionName
  location = functionLocation

instance Eq Function where
  fa == fb = ident fa == ident fb &&
             retType fa == retType fb &&
             map paramType (parameters fa) == map paramType (parameters fb)

data Struct = Struct {
    structName ∷ String
  , structLocation ∷ Location
  , declarations ∷ [StructVarDecl]
}

instance Global Struct where
  ident = structName
  location = structLocation

data Variable = Variable {
    variableName ∷ String
  , variableLocation ∷ Location
  , varType ∷ Type
}

instance Global Variable where
  ident = variableName
  location = variableLocation


data Program = Program {
    functions ∷ Map String Function
  , structs ∷ Map String Struct
  , variables ∷ Map String Variable
}

