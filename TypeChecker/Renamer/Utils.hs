{-# Language UnicodeSyntax #-}

module TypeChecker.Renamer.Utils where

import Control.Applicative
import Control.Monad.State hiding (mapM)

import Data.Map as Map (insert, lookup)

import TypeChecker.TCM
import TypeChecker.TCM.Utils
import TypeChecker.TCM.Errors

import TypeChecker.Types
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
  as ← gets aliases
  modify (\st → st { aliases = insert s s' (head as):tail as})
  return s'

lookupAlias ∷ String → TCM String
lookupAlias s = do
  as ← gets aliases
  case lookupAlias' as of
    Just alias' → return alias'
    Nothing → debugError "ALIAS NOT FOUND"
 where
  lookupAlias' ∷ [Aliases] → Maybe String
  lookupAlias' [] = Nothing
  lookupAlias' (a:as) =
    case Map.lookup s a of
      Just alias' → Just alias'
      Nothing → lookupAlias' as

addSource ∷ Source → TCM ()
addSource src = do
  mergeFunctions' (functions src)
  mergeVariables (variables src)
  mergeTypedefs  (typedefs src)

renameVariable ∷ Variable → TCM Variable
renameVariable (Variable name loc typ) = Variable <$> newAlias name <*>
  pure loc <*> pure typ

annotateFunction ∷ Function → TCM Function
annotateFunction f = do
  ident' ← newAlias (ident f)
  return $ f { alias = ident' }
