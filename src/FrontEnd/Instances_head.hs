{-# LANGUAGE UnicodeSyntax #-}

module FrontEnd.Instances where

import FrontEnd.AbsGrammar
import CompilerTypes

class Token a where
  tkpos ∷ a → Position
  tkident ∷ a → String

