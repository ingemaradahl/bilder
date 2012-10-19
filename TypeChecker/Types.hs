{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.Types where

import CompilerTypes
import FrontEnd.AbsGrammar

import Data.Map (Map, toList)
import Data.List (intercalate)
import Text.Printf

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

instance Global Function where
  ident = functionName
  location = functionLocation

instance Eq Function where
  fa == fb = ident fa == ident fb &&
             retType fa == retType fb &&
             map varType (paramVars fa) == map varType (paramVars fb)

instance Show Function where
  show (TypeChecker.Types.Function name _ ret params _ _) = printf "%s :: %s -> %s"
    name
    (intercalate " -> " $ map (show . varType) params)
    (show ret)

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

instance Show Variable where
 show (Variable name _ typ) = printf "%s ∷ %s"
  name
  (show typ)

data Typedef = Typedef {
    typedefName ∷ String
  , typedefLocation ∷ Location
  , typedefType ∷ Type
}
 deriving (Show)

instance Global Typedef where
  ident = typedefName
  location = typedefLocation

data Blob = Blob {
  filename ∷ FilePath,
  functions ∷ Map String [Function],
  typedefs ∷ Map String Typedef,
  variables ∷ Map String Variable
}

instance Show Blob where
  show (Blob file funs types vars) = printf (
    "# %s ###########\n" ++
    "Typedefs:\n%s" ++
    "Functions:\n%s" ++
    "Variables:\n%s")
    file
    ((concatMap (\(k,v) → printf "  %s: %s\n" k (show $ typedefType v)) $ toList types) :: String)
    ((concatMap (\(_,v) → printf "  %s\n" $ intercalate "\n  " $ map show v) $ toList funs) :: String)
    ((concatMap (\(k,v) → printf "  %s: %s\n" k (show v)) $ toList vars) :: String)
