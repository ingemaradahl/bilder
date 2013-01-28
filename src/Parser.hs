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
{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Control.Monad.Trans.State
import Control.Monad.Error

import qualified Data.Map as Map

import Text.Regex.Posix

import FrontEnd.AbsGrammar
import FrontEnd.ParGrammar
import FrontEnd.ErrM

import Compiler hiding (options, buildEnv, warnings)
import CompilerError
import CompilerTypes

-- | PM - A ParserMonad, keeps a state of which file has been imported
type PM a = StateT PPEnv (CErrorT IO) a

-- | Preprocessor stuff
data PPEnv = PPEnv {
  defines ∷ Map.Map String ([String], String),
  ifStack ∷ [Bool],
  children ∷ Map.Map String String,
  warnings ∷ [(Position, String)],
  filepaths ∷ [FilePath],
  currentFile ∷ FilePath,
  currentLine ∷ Int,
  options ∷ Options
}

buildEnv ∷ Options → PPEnv
buildEnv os = PPEnv {
  ifStack = [],
  defines = Map.empty,
  children = Map.empty,
  warnings = [],
  filepaths = [],
  currentFile = "",
  currentLine = 0,
  options = os
}

-- | Parse the given source code
parse ∷ String → PM AbsTree
parse src =
  case (pAbsTree . myLexer) src of
    Bad e → do
      cf ← gets currentFile
      throwError $ parseErrM cf e
    Ok tree → return tree
  where
    parseErrM ∷ String → String → CompilerError
    parseErrM f s | s =~ "line" ∷ Bool = SyntaxError (lineErr s,0) f s
                  | otherwise = UnknownError s
    lineErr ∷ String → Int
    lineErr s = read (s =~ "[1-9]+" ∷ String) ∷ Int
