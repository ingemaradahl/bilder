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
  show res = case runStateT res $ buildEnv Options { inputFile = "DEBUG", preludeFile = "include/Prelude.bild" } of
    Pass (v,s) → printf "%s\n%s" (show v) (show s)
    Fail e → show e

