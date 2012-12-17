{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Simple.Types where

import qualified Data.Map as Map
import Compiler.Simple.AbsSimple

data Shader = Shader {
    functions ∷ Map.Map String Function
  , variables ∷ Map.Map String Variable
  --, structs ∷ Map.Map String Struct
  , output ∷ Variable
  , inputs ∷ Map.Map String Variable
}
 deriving (Show)
