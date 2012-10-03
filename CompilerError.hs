{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses  #-}

module CompilerError where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity

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

data CError a = Pass a | Fail CompilerError
  deriving (Read, Show, Eq, Ord)

newtype CErrorT m a = CErrorT { runCErrorT ∷ m (CError a) }

instance Monad m ⇒ Monad (CErrorT m) where
  return = (CErrorT . return . Pass)
  CErrorT v >>= f = CErrorT $ do
    result ← v
    case result of
      Pass a → runCErrorT $ f a
      Fail e → return $ Fail e

instance MonadTrans CErrorT where
  lift m = CErrorT $ do
    a ← m
    return $ Pass a

instance Functor CError where
  fmap f (Pass a) = Pass (f a)
  fmap _ (Fail e) = Fail e

instance Applicative CError where
  pure = Pass
  (Pass f) <*> a = fmap f a
  (Fail e) <*> _ = Fail e

instance Monad CError where
  return       = Pass
  fail s       = Fail $ UnknownError s
  Pass a >>= f = f a
  Fail e >>= f = Fail e

instance MonadError CompilerError CError where
    throwError             = Fail
    Pass  a `catchError` _ = Pass a
    Fail  f `catchError` h = h f

