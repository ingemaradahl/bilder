{-# Language UnicodeSyntax #-}

module TypeChecker.Renamer.Utils where

import Control.Applicative
import Control.Monad.State hiding (mapM)

import Data.Map as Map (insert, lookup, empty)

import TypeChecker.TCM
import TypeChecker.TCM.Utils
import TypeChecker.TCM.Errors

import TypeChecker.Types
import TypeChecker.Environment

import Compiler.Utils

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

pushAlias ∷ TCM ()
pushAlias = modify (\st → st { aliases = Map.empty:aliases st})

popAlias ∷ TCM ()
popAlias = modify (\st → st { aliases = tail (aliases st)})

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

renameParam ∷ Param → TCM Param
renameParam (ParamDec qs cid) = ParamDec <$> pure qs <*> renameCIdent cid
renameParam (ParamDefault qs cid tk e) = ParamDefault <$> pure qs <*>
  renameCIdent cid <*> pure tk <*> renameExp e

renameCIdent ∷ CIdent → TCM CIdent
renameCIdent (CIdent (pos, s)) = CIdent <$> ((,) <$> pure pos <*> lookupAlias s)

renameStm ∷ Stm → TCM Stm
renameStm = undefined

renameExp ∷ Exp → TCM Exp
renameExp (EVar cid) = EVar <$> renameCIdent cid
renameExp (ECall cid e) = ECall <$> renameCIdent cid <*> mapM renameExp e
renameExp e =  mapExpM renameExp e

