{-# LANGUAGE UnicodeSyntax #-}

module Compiler where

import Control.Monad.State hiding (mapM)

import Data.Tree

import CompilerError

import TypeChecker.Types


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

compileTree ∷ Options → Source → CError [String]
compileTree opts src = undefined -- evalStateT compile' (buildEnv opts)

{-compile' ∷ CPM [String]-}
{-compile' = do-}
  {-bs ← gets blobs-}
  {-return [drawTree $ fmap show bs]-}
--compile t@(Tree toplevels) = do
  --return [printTree t]
