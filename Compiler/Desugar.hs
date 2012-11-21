{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Desugar (desugar) where

import TypeChecker.Types
import Compiler.Desugar.Uncurry (uncurrySource)
import Compiler.Desugar.SimpleDecs (simpleDecs)
import Compiler.Desugar.Extract (expandSource)

desugar ∷ Source → Source
desugar = expandSource . simpleDecs . uncurrySource

