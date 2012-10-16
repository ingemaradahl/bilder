{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module TypeChecker.Scope where

import TypeChecker.Types hiding (functions, variables, typedefs)
import FrontEnd.AbsGrammar

import Data.Map

data Scope = Scope {
  functions ∷ Map String [Function],
  typedefs ∷ Map String Type,
  variables ∷ Map String Variable
}

emptyScope ∷ Scope
emptyScope = Scope { functions = empty, typedefs = empty, variables = empty }

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

addTypedef ∷ String → Type → Scope → Scope
addTypedef n t s = s { typedefs = typedefs' }
 where
  typedefs' = insert n t $ typedefs s

lookupTypedef ∷ String → [Scope] → Maybe Type
lookupTypedef n (s:ss) =
  case Data.Map.lookup n (typedefs s) of
    Just t  → Just t
    Nothing → lookupTypedef n ss
lookupTypedef _ [] = Nothing

