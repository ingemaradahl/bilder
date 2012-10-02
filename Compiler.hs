{-# LANGUAGE UnicodeSyntax #-}

module Compiler where

import CompilerError
import FrontEnd.AbsGrammar

data Options = Options {
  inputFile ∷ FilePath
}
compileTree ∷ Options → Program → CErr [String]
compileTree o tree = undefined

typeCheck ∷ Options → Program → CErr Program
typeCheck = undefined
