{-
 -      This file is part of Bilder.
 -
 -   Bilder is free software: you can redistribute it and/or modify
 -   it under the terms of the GNU Lesser General Public License as published by
 -   the Free Software Foundation, either version 3 of the License, or
 -   (at your option) any later version.
 -
 -   Bilder is distributed in the hope that it will be useful,
 -   but WITHOUT ANY WARRANTY; without even the implied warranty of
 -   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -   GNU Lesser General Public License for more details.
 -
 -   You should have received a copy of the GNU Lesser General Public License
 -   along with Bilder.  If not, see <http://www.gnu.org/licenses/>.
 -
 -   Copyright © 2012-2013 Filip Lundborg
 -   Copyright © 2012-2013 Ingemar Ådahl
 -
 -}
{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, UndecidableInstances #-}

module CompilerError where

import CompilerTypes

import Control.Applicative
import Control.Monad.Trans

import Control.Monad.Error

import Text.Printf

data CompilerError =
    SyntaxError Position FilePath String
  | TypeError Position FilePath String
  | CompileError Position FilePath String
  | UnknownError String
  deriving (Read, Eq, Ord)

instance Show CompilerError where
  show (SyntaxError (l,c) f s) = printf "SYNTAX ERROR in %s:%d, column %d\n%s" f l c s
  show (CompileError (l,c) f s) = printf "COMPILE ERROR in %s:%d, column: %d\n%s" f l c s
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
      Pass a → runCErrorT $ f a
      Fail e → return $ Fail e

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

instance Functor a ⇒ Functor (CErrorT a) where
  fmap f (CErrorT m) = CErrorT $ fmap (fmap f) m

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

