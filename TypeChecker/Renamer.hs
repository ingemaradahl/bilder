{-# Language UnicodeSyntax #-}

module TypeChecker.Renamer where

import Control.Monad.State hiding (mapM)

import Data.Tree
import Data.Map as Map hiding (fold)
import Data.Monoid

import TypeChecker.TCM
import TypeChecker.TCM.Utils hiding (initScope)

import TypeChecker.Environment hiding (pushScope, popScope)
import TypeChecker.Renamer.Utils

import TypeChecker.Types
import TypeChecker.Types.Blob (Blob, Blob (Blob), filename)
import qualified TypeChecker.Types.Blob as Blob (functions, typedefs, variables)
import qualified TypeChecker.Scope as Scope

rename ∷ Blob → [Tree (Source, Aliases)] → TCM (Source, Aliases)
rename b ss = do
  done ← gets renamed
  if filename b `elem` done
    then return (mempty :: Source, Map.empty)
    else renameBlob b ss

renameBlob ∷ Blob → [Tree (Source, Aliases)] → TCM (Source, Aliases)
renameBlob blob children = do
  clearScope

  -- Insert aliases from children
  modify (\st → st { aliases = [Map.unions $ Prelude.map (snd . rootLabel) children] })

  -- Populate Scope
  mapM_ (addSource . fst . rootLabel) children
  annotFuns ← mapM annotateFunction $ concat $ elems (Blob.functions blob)
  mapM_ addFunction annotFuns

  variables' ← renameVariables $ Blob.variables blob
  functions' ← mapM renameFunction annotFuns

  aliases' ← gets (head . aliases)

  return (Source
    (fromList (Prelude.map (\f → (ident f, f)) functions'))
    Map.empty
    variables'

    , aliases')


renameFunction ∷ Function → TCM Function
renameFunction fun = do
  pushScope
  pushAlias

  paramVars' ← mapM renameVariable (paramVars fun)
  parameters' ← mapM renameParam (parameters fun)
  statements' ← mapM renameStm (statements fun)

  popAlias
  popScope

  return fun {
      functionName = alias fun
    , paramVars = paramVars'
    , parameters = parameters'
    , statements = statements'
  }


renameBody ∷ String → [Function] → TCM [Function]
renameBody = undefined

renameVariables ∷ Map String Variable → TCM (Map String Variable)
renameVariables vars = do
  vars' ← mapM (renameVariable . snd) (toList vars)
  return $ fromList $ Prelude.map (\v → (ident v, v)) vars'

