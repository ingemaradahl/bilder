{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Desugar (desugar) where

import TypeChecker.Types
import Compiler.Desugar.Uncurry (uncurrySource)
import Compiler.Desugar.SimpleDecs (simpleDecs)

desugar ∷ Source → Source
desugar = simpleDecs . uncurrySource

