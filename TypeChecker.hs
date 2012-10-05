{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker where

import Data.Map (Map)
import Data.Tree

import Compiler hiding (Environment)
import FrontEnd.AbsGrammar
import CompilerError

data Environment = Env {
  functions ∷ [CIdent]
}

-- | Typechecks the given abstract source and annotates the syntax tree
typeCheck ∷ Options → Tree (FilePath, AbsTree) → CError (Tree (FilePath, AbsTree))
typeCheck _ = Pass
