{-# Language UnicodeSyntax #-}

module TypeChecker.Renamer.Utils where

import Control.Applicative
import Control.Monad.State hiding (mapM)

import Data.Map as Map (insert, lookup, empty)

import TypeChecker.TCM
import TypeChecker.TCM.Utils
import TypeChecker.TCM.Errors

import TypeChecker.Types
import TypeChecker.Environment hiding (pushScope, popScope)

import CompilerTypes

import FrontEnd.AbsGrammar

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

newCIdAlias ∷ CIdent → TCM CIdent
newCIdAlias (CIdent (pos,n)) = CIdent <$> ((,) <$> pure pos <*> newAlias n)

pushAlias ∷ TCM ()
pushAlias = modify (\st → st { aliases = Map.empty:aliases st})

popAlias ∷ TCM ()
popAlias = modify (\st → st { aliases = tail (aliases st)})

lookupAlias'' ∷ String → Position → TCM String
lookupAlias'' s pos = do
  as ← gets aliases
  case lookupAlias' as of
    Just alias' → return alias'
    Nothing → typeError pos $ "ALIAS " ++ s ++ "NOT FOUND"
 where
  lookupAlias' ∷ [Aliases] → Maybe String
  lookupAlias' [] = Nothing
  lookupAlias' (a:as) =
    case Map.lookup s a of
      Just alias' → Just alias'
      Nothing → lookupAlias' as

lookupAlias ∷ String → TCM String
lookupAlias s = do
  as ← gets aliases
  case lookupAlias' as of
    Just alias' → return alias'
    Nothing → debugError $ "ALIAS " ++ s ++ "NOT FOUND"
 where
  lookupAlias' ∷ [Aliases] → Maybe String
  lookupAlias' [] = Nothing
  lookupAlias' (a:as) =
    case Map.lookup s a of
      Just alias' → Just alias'
      Nothing → lookupAlias' as

lookupAliasMaybe ∷ String → TCM (Maybe String)
lookupAliasMaybe s = liftM lookupAlias' $ gets aliases
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

annotateFunction ∷ Function → TCM Function
annotateFunction f = do
  ident' ← if ident f == "main" then return "main" else newAlias (ident f)
  return $ f { alias = ident' }

flushAliases ∷ TCM ()
flushAliases = undefined

