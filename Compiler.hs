{-# LANGUAGE UnicodeSyntax #-}

module Compiler where

import Control.Monad.State hiding (mapM)

import CompilerError

import Compiler.Lifter hiding (Environment, source, buildEnv)
import qualified Compiler.Lifter as L

import TypeChecker.Types

data Options = Options {
  inputFile ∷ FilePath
}
  deriving (Show)

data Environment = Env {
  source ∷ Source,
  options ∷ Options
}

buildEnv ∷ Options → Source → Environment
buildEnv opts src= Env {
  source = src,
  options = opts
}

-- CPM - CompilerMonad: Alias for both Error and State monad
type CPM a = StateT Environment CError a

compileTree ∷ Options → Source → CError [String]
compileTree opts src = evalStateT compile' (buildEnv opts src)

compile' ∷ CPM [String]
compile' = do
  src ← gets source
  return [show src]
--compile t@(Tree toplevels) = do
  --return [printTree t]
