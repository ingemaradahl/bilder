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
