{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, UndecidableInstances  #-}

module CompilerError where

import CompilerTypes

import Control.Applicative
import Control.Monad.Trans

import Control.Monad.Error

import Text.Printf

data CompilerError =
    SyntaxError Position String
  | TypeError Position FilePath String
  | CompileError Position FilePath String
  | UnknownError String
  deriving (Read, Eq, Ord)

instance Show CompilerError where
  show (SyntaxError _ s) = printf "SYNTAX ERROR: %s" s -- TODO
  show (CompileError _ _ s) = printf "COMPILE ERROR: %s" s -- TODO
  show (TypeError (l,c) f e) = printf "Type error in %s:%d, column %d\n%s" f l c e
  show (UnknownError s) = s

instance Error CompilerError where
  noMsg  = UnknownError "Unknown error"
  strMsg = UnknownError

data CError a = Pass a | Fail CompilerError
  deriving (Read, Show, Eq, Ord)

newtype CErrorT m a = CErrorT { runCErrorT ∷ m (CError a) }

instance Monad m ⇒ Monad (CErrorT m) where
  return = CErrorT . return . Pass
  CErrorT v >>= f = CErrorT $ do
    result ← v
    case result of
      Pass a → runCErrorT $ f a -- TODO verify
      Fail e → return $ Fail e  -- TODO   "

instance MonadIO m ⇒ MonadIO (CErrorT m) where
  liftIO = lift . liftIO

instance MonadTrans CErrorT where
  lift m = CErrorT $ do
    a ← m
    return $ Pass a

instance (Monad a) ⇒ MonadError CompilerError (CErrorT a) where
  throwError e     = CErrorT $ return (Fail e)
  m `catchError` h = CErrorT $ do
    a ← runCErrorT m
    case a of
      Fail e → runCErrorT (h e)
      Pass f → return (Pass f)

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
  Fail e >>= _ = Fail e

instance MonadError CompilerError CError where
    throwError             = Fail
    Pass  a `catchError` _ = Pass a
    Fail  f `catchError` h = h f

