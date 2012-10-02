{-# LANGUAGE UnicodeSyntax #-}

module Compiler where

import Control.Monad.State

import CompilerError
import FrontEnd.AbsGrammar

data Options = Options {
  inputFile ∷ FilePath
}

data Environment = Env {
  source ∷ String,
  position ∷ Position,
  options ∷ Options
}

buildEnv ∷ Options → Environment
buildEnv opts = Env {
  source = "",
  position = (0,0),
  options = opts
}

-- CPM - CompilerMonad: Alias for both Error and State monad
type CPM a = StateT Environment CErr a


compileTree ∷ Options → Program → CErr [String]
compileTree opts tree = evalStateT (compile tree) (buildEnv opts)

compile ∷ Program → CPM [String]
compile (Prog functions) = do
  return ["\\o/"]

typeCheck ∷ Options → Program → CErr Program
typeCheck = undefined
