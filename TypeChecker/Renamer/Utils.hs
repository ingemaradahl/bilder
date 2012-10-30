{-# Language UnicodeSyntax #-}

module TypeChecker.Renamer.Utils where

import Control.Applicative
import Control.Monad.State hiding (mapM)

import Data.Map as Map (insert, lookup)

import TypeChecker.TCM
import TypeChecker.TCM.Errors
import TypeChecker.Environment

import Text.Printf

popId ∷ TCM ()
popId = modify (\st → st { freeAliases = tail (freeAliases st)})

newId ∷ TCM String
newId = gets (head . freeAliases) >>= (\i → popId >> return (printf "_%03d" i))

newIdSuffix ∷ String → TCM String
newIdSuffix s = (++) <$> newId <*> pure s

newAlias ∷ String → TCM String
newAlias s = do
  s' ← newIdSuffix s
  modify (\st → st { aliases = insert s s' (aliases st)})
  return s'

lookupAlias ∷ String → TCM String
lookupAlias s = do
  as ← gets aliases
  case Map.lookup s as of
    Just alias → return alias
    Nothing → debugError "ALIAS NOT FOUND"
