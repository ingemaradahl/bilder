{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module TypeChecker.TCM where

import TypeChecker.Environment as Env
import TypeChecker.Scope
import TypeChecker.Types
import TypeChecker.Utils

import Compiler hiding (Environment, buildEnv)
import CompilerError

import FrontEnd.AbsGrammar

import Control.Monad.Trans.State
import Control.Monad.Error

import Prelude hiding (lookup)
import Data.Map

import Text.Printf

-- | TypeCheckerMonad - Wrapping up a state around CError
type TCM a = StateT Environment CError a

-- | Only used during interpretation runs
instance (Show a) => Show (TCM a) where
  show res = case runStateT res $ buildEnv Options { inputFile = "DEBUG" } of
    Pass (v,s) → printf "%s\n%s" (show v) (show s)
    Fail e → show e

-- | Adds a function to the environment, and makes sure there are no duplicates
addFunction ∷ String → Function → TCM ()
addFunction name fun@(_,pos,_,_) = do
  (s:ss) ← gets scopes
  let a = lookup name (functions s) >>= (\fs → if fun `exists` fs then Just fs; else Nothing)
  case a of
    Just _ → typeError pos $ printf "function '%s' with type %s already defined" name (showFunctionType fun)
    Nothing → modify (\st → st { scopes = saddFunction name fun s : ss })

-- | Adds a variable to the current scope, making sure there are no duplicates
addVariable ∷ String → Variable → TCM ()
addVariable name v@(_,pos,_) = do
  (s:ss) ← gets scopes
  case lookup name (variables s) of
    Just _ → typeError pos $ printf "variable '%s' already defined" name
    Nothing → modify (\st → st { scopes = saddVariable name v s : ss } )

-- | Add a variable to the current scope, using currentFile from the state
addVariable' ∷ String → Position → Type → TCM ()
addVariable' n p t = do
  file ← gets currentFile
  addVariable n (file, p, t)

-- | Lookup a variable denoted by it's name. May throw error
lookupVar ∷ String → TCM Variable
lookupVar name = gets scopes >>= lookupVar'
 where
  lookupVar' ∷ [Scope] → TCM Variable
  lookupVar'     [] = typeError (-1,-1) $ "Cannot find variable " ++ name
  lookupVar' (s:ss) = case lookup name (variables s) of
    Just v  → return v
    Nothing → lookupVar' ss

-- | Sets which file is currently checked
updateFile ∷ FilePath → TCM ()
updateFile f = modify (\st → st { currentFile = f })

-- | Pushes a new scope to the environment
pushScope ∷ TCM ()
pushScope = modify Env.pushScope

popScope ∷ TCM ()
popScope = modify Env.popScope

-- | Throw a type error
typeError ∷ Position → String → TCM a
typeError = absError TypeError

compileError ∷ Position → String → TCM a
compileError = absError CompileError

absError ∷ (Position → FilePath → String → CompilerError) → Position → String → TCM a
absError e p s = do
  f ← gets currentFile
  throwError $ e p f s
