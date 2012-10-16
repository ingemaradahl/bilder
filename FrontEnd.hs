{-# LANGUAGE UnicodeSyntax #-}

module FrontEnd where

import System.FilePath

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State

import Data.Tree
import Data.List ((\\))

import FrontEnd.AbsGrammar

import Compiler hiding (options, buildEnv)
import CompilerError

import Parser
import Preprocessor

liftCError ∷ CError a → PM a
liftCError m = StateT (\s → case m of { Fail f → CErrorT $ return $ Fail f; Pass a → return (a,s) })

-- | Add a file to the state, and return the list of imported files
addFile ∷ FilePath → PM [FilePath]
addFile f = do
  fs ← gets filepaths
  modify (\s → s { filepaths = f : fs })
  return $ f : fs

-- | Parse the tree of files
parseHead ∷ Options → IO (CError (Tree (FilePath, AbsTree)))
parseHead os = runCErrorT $ evalStateT (parseWithPrelude (inputFile os)) (buildEnv os)

-- | Filter out the import statements from the syntax tree
filterImports ∷ AbsTree → [FilePath]
filterImports (AbsTree ts) = [ f | Import _ f ← ts ]

-- | Parses tree and adds prelude at the top level
parseWithPrelude ∷ FilePath → PM (Tree (FilePath, AbsTree))
parseWithPrelude f = do
  -- TODO: Add PATH-like stuff for include/.
  p ← parseTree "include/Prelude.fl"
  t ← parseTree f
  return $ Node (rootLabel t) $ p:subForest t

-- | Recursively parse the files to be imported
parseTree ∷ FilePath → PM (Tree (FilePath, AbsTree))
parseTree f = do
  liftIO . putStrLn $ "reading " ++ f
  imported ← addFile f
  t ← readAndProcessFile f >>= parse
  liftM (Node (f, t)) (mapM parseTree (map (dir </>) (filterImports t) \\ imported))
 where
  dir = takeDirectory f
