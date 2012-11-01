{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.Types.Blob where

import Data.Map (Map, toList)
import Data.List (intercalate)
import Text.Printf

import TypeChecker.Types

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

