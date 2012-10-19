{-# LANGUAGE UnicodeSyntax #-}

module Compiler where

import Control.Monad.State

import Data.Tree

import CompilerError
import CompilerTypes

import TypeChecker.Types

import FrontEnd.AbsGrammar

data Options = Options {
  inputFile ∷ FilePath
}
  deriving (Show)

data Environment = Env {
  blobs ∷ Tree Blob,
  source ∷ String,
  options ∷ Options
}

buildEnv ∷ Options → Tree Blob → Environment
buildEnv opts bs = Env {
  blobs = bs,
  source = "",
  options = opts
}

-- CPM - CompilerMonad: Alias for both Error and State monad
type CPM a = StateT Environment CError a

compileTree ∷ Options → Tree Blob → CError [String]
compileTree opts bs = evalStateT compile' (buildEnv opts bs)

compile' ∷ CPM [String]
compile' = do
  bs ← gets blobs
  return [drawTree $ fmap show bs]
--compile t@(Tree toplevels) = do
  --return [printTree t]
