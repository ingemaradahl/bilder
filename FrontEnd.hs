{-# LANGUAGE UnicodeSyntax #-}

module FrontEnd where

import System.FilePath

import Control.Monad
import Control.Monad.Trans.State

import Data.Tree
import Data.List ((\\))

import FrontEnd.AbsGrammar

import Compiler hiding (options, buildEnv)
import CompilerError

import Parser
import Preprocessor

import NetTypes
import TypeChecker.Utils (cIdentToString)

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

-- | Parses a list of NetFiles.
parseNetHead ∷ Options → [NetFile] → IO (CError (Tree (FilePath, AbsTree)))
parseNetHead os fs = runCErrorT $ evalStateT (parseNet fs) (buildEnv os)

-- | Parses net tree and adds prelude at the top level
parseNetWithPrelude ∷ (String, AbsTree) → [(String, AbsTree)] → PM (Tree (FilePath, AbsTree))
parseNetWithPrelude f rest = do
  -- TODO: Add PATH-like stuff for include/.
  p ← parseTree "include/Prelude.fl"
  t ← parseNetTree f rest
  return $ Node (rootLabel t) $ p:subForest t

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

hasMain ∷ AbsTree → Bool
hasMain (AbsTree ts) = True `elem` map isMain ts

isMain ∷ Toplevel → Bool
isMain (Function _ cid _ _) = cIdentToString cid == "main"
isMain _ = False

preprocessNetFile ∷ NetFile → PM String
preprocessNetFile f = do
  modify (\s → s {currentFile = name f})
  preprocess (contents f)

parseNet ∷ [NetFile] → PM (Tree (FilePath, AbsTree))
parseNet fs = do
  -- parse all the files
  pfs ← sequence [ do
    pf ← preprocessNetFile f >>= parse
    return (name f, pf)
    | f ← fs ]

  -- find what file cointains the main-function.
  let mfs = filter (hasMain . snd) pfs
  -- TODO: Throw a real CompilerError.
  unless (length mfs == 1) $ error "unable to locate function `main'."
  let main = head mfs

  parseNetWithPrelude main pfs

findTree ∷ FilePath → [(String, AbsTree)] → (String, AbsTree)
findTree f fs =
  case filter ((==f). fst) fs of
    [] → error "no such file found."
    rs → head rs

parseNetTree ∷ (String, AbsTree) → [(String, AbsTree)] → PM (Tree (FilePath, AbsTree))
parseNetTree (f, t) rest = do
  imported ← addFile f

  let imports = map (`findTree` rest) (filterImports t \\ imported)
  liftM (Node (f, t)) (mapM (`parseNetWithPrelude` rest) imports)

-- | Recursively parse the files to be imported
parseTree ∷ FilePath → PM (Tree (FilePath, AbsTree))
parseTree f = do
  imported ← addFile f
  t ← readAndProcessFile f >>= parse
  liftM (Node (f, t)) (mapM parseWithPrelude (map (dir </>) (filterImports t) \\ imported))
 where
  dir = takeDirectory f
