{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module TypeChecker.Scope where

import TypeChecker.Types

import Data.Map

data Scope = Scope {
  functions ∷ Map String [Function],
  variables ∷ Map String Variable
}
 deriving (Show)


emptyScope ∷ Scope
emptyScope = Scope { functions = empty, variables = empty }

-- | Add a function to a scope
saddFunction ∷ String → Function → Scope → Scope
saddFunction n t s = s { functions = fs' }
 where
  fs = functions s
  fs' = if member n fs
          then adjust (t:) n fs
          else insert n [t] fs

saddVariable ∷ String → Variable → Scope → Scope
saddVariable n v s = s { variables = vs }
 where
  vs = insert n v $ variables s


