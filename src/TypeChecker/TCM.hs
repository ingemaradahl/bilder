{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module TypeChecker.TCM where

import TypeChecker.Environment as Env

import Compiler hiding (Environment, buildEnv)
import CompilerError

import Control.Monad.Trans.State
import Text.Printf

-- | TypeCheckerMonad - Wrapping up a state around CError
type TCM a = StateT Environment CError a

-- | Only used during interpretation runs
instance (Show a) => Show (TCM a) where
  show res = case runStateT res $ buildEnv Options { inputFile = "DEBUG" } of
    Pass (v,s) → printf "%s\n%s" (show v) (show s)
    Fail e → show e

