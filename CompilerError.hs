{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses #-}

module CompilerError where

import Control.Applicative
import Control.Monad

import Control.Monad.Error

type Position = (Int, Int)

data CompilerError =
    SyntaxError Position String
  | TypeError Position String
  | CompileError Position String
  | UnknownError String
  deriving (Read, Show, Eq, Ord)

instance Error CompilerError where
  noMsg    = UnknownError "Unknown error"
  strMsg s = UnknownError s

data EitherError a = Pass a | Fail CompilerError
  deriving (Read, Show, Eq, Ord)

instance Functor EitherError where
  fmap f (Pass a) = Pass (f a)
  fmap _ (Fail e) = Fail e

instance Applicative EitherError where
  pure = Pass
  (Pass f) <*> a = fmap f a
  (Fail e) <*> _ = Fail e

instance Monad EitherError where
  return       = Pass
  fail s       = Fail $ UnknownError s
  Pass a >>= f = f a
  Fail e >>= f = Fail e

instance MonadError CompilerError EitherError where
    throwError             = Fail
    Pass  a `catchError` _ = Pass a
    Fail  f `catchError` h = h f

-- CompilerError alias
type CErr = EitherError

