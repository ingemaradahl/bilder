{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker where

import Compiler
import FrontEnd.AbsGrammar
import CompilerError

typeCheck ∷ Options → Program → CErr Program
typeCheck o p = Pass p
