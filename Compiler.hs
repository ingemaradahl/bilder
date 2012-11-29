{-# LANGUAGE UnicodeSyntax #-}

module Compiler where

import Control.Monad.State hiding (mapM)

import CompilerError
import Compiler.Utils

import qualified Compiler.Lifter as L
import Compiler.Desugar (desugar)
import Compiler.Split (splitSource)
import Compiler.Simple (absToSimple, simpleToGLSL)

-- TEMP
import Compiler.GLSL.Utils


import FrontEnd.AbsGLSL as G

import TypeChecker.Types

data Options = Options {
  inputFile ∷ FilePath
}
  deriving (Show)

data Environment = Env {
  options ∷ Options,
  warnings ∷ [String]
}

buildEnv ∷ Options → Environment
buildEnv opts = Env {
  options = opts,
  warnings = []
}

-- CPM - CompilerMonad: Alias for both Error and State monad
type CPM a = StateT Environment CError a

compileTree ∷ Options → Source → CError [G.Tree]
compileTree opts src = evalStateT (compile src) (buildEnv opts)

lambdaLift ∷ Source → CPM Source
lambdaLift src = do
  env ← liftCError $ execStateT L.lambdaLift (L.buildEnv src)
  modify (\s → s {
      warnings = L.warnings env ++ warnings s
    })
  return $ L.source env

compile ∷ Source → CPM [G.Tree]
compile src =
  liftM (simpleToGLSL . map finalizeMain . absToSimple .splitSource . desugar) $ lambdaLift src
