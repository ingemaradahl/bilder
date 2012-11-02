{-# LANGUAGE UnicodeSyntax #-}

module Compiler where

import Control.Monad.State hiding (mapM)

import CompilerError
import Compiler.Utils

import qualified Compiler.Lifter as L

import TypeChecker.Types

data Options = Options {
  inputFile ∷ FilePath
}
  deriving (Show)

data Environment = Env {
  source ∷ Source,
  options ∷ Options,
  warnings ∷ [String]
}

buildEnv ∷ Options → Source → Environment
buildEnv opts src = Env {
  source = src,
  options = opts,
  warnings = []
}

-- CPM - CompilerMonad: Alias for both Error and State monad
type CPM a = StateT Environment CError a

compileTree ∷ Options → Source → CError [String]
compileTree opts src = evalStateT compile' (buildEnv opts src)

lambdaLift ∷ CPM ()
lambdaLift = do
  src ← gets source
  env ← liftCError $ execStateT L.lambdaLift (L.buildEnv src)
  modify (\s → s {
      warnings = L.warnings env ++ warnings s,
      source = L.source env
    })

compile' ∷ CPM [String]
compile' = do
  lambdaLift
  src ← gets source
  return [show src]
