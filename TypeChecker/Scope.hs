{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module TypeChecker.Scope where

import TypeChecker.Types hiding (functions, variables)

import Data.Map

data Scope = Scope {
  functions ∷ Map String [Function],
  variables ∷ Map String Variable
}

emptyScope ∷ Scope
emptyScope = Scope { functions = empty, variables = empty }

-- | Add a function to a scope
addFunction ∷ Function → Scope → Scope
addFunction fun scope = scope { functions = fs' }
 where
  name = ident fun
  fs = functions scope
  fs' = if member name fs
          then adjust (fun:) name fs
          else insert name [fun] fs

addVariable ∷ Variable → Scope → Scope
addVariable v scope = scope { variables = vs }
 where
  name = ident v
  vs = insert name v $ variables scope


