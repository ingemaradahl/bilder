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
  source ∷ String,
  options ∷ Options
}

buildEnv ∷ Options → Environment
buildEnv opts = Env {
  source = "",
  options = opts
}

-- CPM - CompilerMonad: Alias for both Error and State monad
type CPM a = StateT Environment CError a

compileTree ∷ Options → Tree Blob → CError [String]
compileTree _ b = Pass [drawTree $ fmap show b]
{-
 -compileTree ∷ Options → Tree (FilePath,AbsTree) → CError [String]
 -compileTree opts tree = evalStateT (compile tree) (buildEnv opts)
 -
 -compile ∷ Tree (FilePath,AbsTree) → CPM [String]
 -compile _ = return ["tree"]
 -}
--compile t@(Tree toplevels) = do
  --return [printTree t]
