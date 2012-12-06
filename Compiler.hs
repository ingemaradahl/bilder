{-# LANGUAGE UnicodeSyntax #-}

module Compiler where

import Control.Monad.State hiding (mapM)

import Control.Arrow
import qualified Data.Map as Map

import TypeChecker.Types (Source)

import Compiler.Simple.AbsSimple
import Compiler.Simple.Types
import Compiler.Simple.Utils
import Compiler.GLSL.Utils
import Compiler.Clean (clean)

import CompilerError
import Compiler.Utils (liftCError)

import qualified Compiler.Lifter as L
import Compiler.Desugar (desugar)
import Compiler.Split (splitSource)
import Compiler.Simple (absToSimple, simpleToGLSL)

import qualified FrontEnd.AbsGLSL as G


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
  liftM (
        desugar
    >>> splitSource       -- Split to [Shader] with AbsGrammar
    >>> absToSimple       -- Convert to SimpleGrammar
    >>> map finalizeMain  -- Replace main-function with GLSL-variant
    >>> map clean         -- Removes unnecessary statements
    >>> simpleToGLSL      -- Translate to GLSL
  ) $ lambdaLift src




finalizeMain ∷ Shader → Shader
finalizeMain shd =
  shd { functions = Map.adjust reworkMain "main" (functions shd) }

reworkMain ∷ Function → Function
reworkMain (Function name _ [x, y] stms) = Function name TVoid [] (d:stms')
 where
  p = Variable "p" TVec2 True
  d = SDeclAss p (EDiv gl_FragCoord_xy fl_Resolution)
  x' = EMember (EVar (variableName p)) "x"
  y' = EMember (EVar (variableName p)) "y"
  stms' = map (subReturn . mapStmExp subXY) stms
  subXY ∷ Exp → Exp
  subXY (EVar s) | s == variableName x = x'
  subXY (EVar s) | s == variableName y = y'
  subXY e = mapExp subXY e
  subReturn ∷ Stm → Stm
  subReturn (SReturn e) = SExp $ EAss gl_FragColor e
  subReturn s = mapStm subReturn s

