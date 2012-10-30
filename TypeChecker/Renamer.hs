{-# Language UnicodeSyntax #-}

module TypeChecker.Renamer where

import Control.Applicative
import Control.Monad.State hiding (mapM)
import Control.Monad.Error


import Data.Tree
import Data.Map as Map hiding (fold)

import Data.Foldable hiding (elem)
import Data.Monoid

import TypeChecker.TCM
import TypeChecker.Environment
import TypeChecker.Renamer.Utils

import TypeChecker.Types
import TypeChecker.Types.Blob (Blob, Blob (Blob), filename)
import qualified TypeChecker.Types.Blob as Blob (functions, typedefs, variables)
import qualified TypeChecker.Scope as Scope

import CompilerError
import CompilerTypes

import Utils

import FrontEnd.AbsGrammar

import Text.Printf


{-rename ∷ Tree Blob → CError Source-}
{-rename bs = liftM fold $ evalStateT (traverse renameBlob bs) emptyIds-}

rename ∷ Blob → [Tree Source] → TCM Source
rename b ss = do
  done ← gets renamed
  if filename b `elem` done
    then return (mempty :: Source)
    else undefined --renameBlob' b ss

{-renameBlob' ∷ Blob → [Tree Source] → Renamer Source-}
{-renameBlob' blob children = do-}
  {-variables' ← renameVariables $ Blob.variables blob-}

  {-
   -functions' ← liftM (mapWithKey renameFunction) (Blob.functions blob) >>= liftM elems >>=
   -  mapM renameBody
   -}

  return undefined



{-
 -renameFunction ∷ Map String [Function] → Renamer (Map String Function)
 -renameFunction funs = undefined
 -}




{-
 -renameBody ∷ String → [Function] → Renamer [Function]
 -renameBody = undefined
 -}

{-
 -renameVariables ∷ Map String Variable → Renamer (Map String Variable)
 -renameVariables = return
 -}

