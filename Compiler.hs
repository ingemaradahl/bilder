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

import CompilerError
import Compiler.Utils (liftCError)

import qualified Compiler.Lifter as L
import Compiler.Desugar (desugar)
import Compiler.Split (splitSource)
import Compiler.Merge (mergeShaders)
import Compiler.Clean (clean)
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
    >>> mergeShaders      -- Try to merge shaders based on sample count
    >>> map finalizeMain  -- Replace main-function with GLSL-variant
    >>> map clean         -- Removes unnecessary statements
    >>> map renameBuiltin -- Re-renames built in functions
    >>> simpleToGLSL      -- Translate to GLSL
  ) $ lambdaLift src




finalizeMain ∷ Shader → Shader
finalizeMain shd =
  shd { functions = Map.adjust reworkMain "main" (functions shd) }

reworkMain ∷ Function → Function
reworkMain (Function name _ [x, y] stms) = Function name TVoid [] (d:stms')
 where
  p = Variable "p" TVec2 True Nothing
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

renameBuiltin ∷ Shader → Shader
renameBuiltin sh = sh { functions =
    Map.map (\f → f { statements = map (mapStmExp cleanBuiltin) (statements f)})
    (functions sh)
  }

-- Worlds ugliest hack below, don't read
cleanBuiltin ∷ Exp → Exp
cleanBuiltin (ECall s es) | matchesBuiltin s =
  ECall (drop 4 s) $ map (mapExp cleanBuiltin) es
cleanBuiltin e = mapExp cleanBuiltin e

matchesBuiltin ∷ String → Bool
matchesBuiltin ('_':'x':_) = True
matchesBuiltin _ = False
