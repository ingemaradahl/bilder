{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.Types where

import CompilerTypes
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
  , paramVars ∷ [Variable]
  , parameters ∷ [Param]
  , statements ∷ [Stm]
} | Null
 deriving (Show)

instance Global Function where
  ident = functionName
  location = functionLocation

instance Eq Function where
  fa == fb = ident fa == ident fb &&
             retType fa == retType fb &&
             map varType (paramVars fa) == map varType (paramVars fb)

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
 deriving (Show)

data Typedef = Typedef {
    typedefName ∷ String
  , typedefLocation ∷ Location
  , typedefType ∷ Type
}
 deriving (Show)

instance Global Variable where
  ident = variableName
  location = variableLocation


data Blob = Blob {
  filename ∷ FilePath,
  functions ∷ Map String [Function],
  typedefs ∷ Map String Typedef,
  variables ∷ Map String Variable
}
