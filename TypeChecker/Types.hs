{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.Types where

import CompilerTypes
import FrontEnd.AbsGrammar
import FrontEnd.PrintGrammar

import Data.Map (Map, empty, union, toList)
import Data.List (intercalate)
import Data.Monoid
import Text.Printf

class Global a where
  ident ∷ a → String
  location ∷ a → Location
  position ∷ a → Position
  position = snd . location
  locfile ∷ a → FilePath
  locfile = fst . location

data Function = Function {
    functionName ∷ String
  , alias ∷ String
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
  show (TypeChecker.Types.Function name _ _ ret params pars ss) = printf "%s :: %s -> %s\n -- Parameters ------------------\n%s\n -- Function body --------------\n%s\n\n"
    name
    (intercalate " -> " $ map (show . varType) params)
    (show ret)
    (intercalate ", " $ map printTree pars)
    (concatMap printTree ss)

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

data Source = Source {
  functions ∷ Map String Function,
  variables ∷ Map String Variable
}

instance Show Source where
  show (Source funs vars) = printf "Functions:\n%s\n\nVariables:\n%s\n\n"
    (indentMapValue funs)
    (indentMapValue vars)
   where
    indentMapValue m = intercalate "\n" $ map (unlines . map (\l → "    " ++ l) . lines . show . snd) (toList m)

emptySource ∷ Source
emptySource = Source empty empty

instance Monoid Source where
  l `mappend` r = Source {
    functions = functions l `union` functions r,
    variables = variables l `union` variables r
  }
  mempty = emptySource

