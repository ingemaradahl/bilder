{-# LANGUAGE UnicodeSyntax #-}

module Parser where

import Control.Monad.Trans.State

import qualified Data.Map as Map

import Text.Regex.Posix

import FrontEnd.AbsGrammar
import FrontEnd.ParGrammar
import FrontEnd.ErrM

import Compiler hiding (options, buildEnv)
import CompilerError
import CompilerTypes

-- | PM - A ParserMonad, keeps a state of which file has been imported
type PM a = StateT PPEnv (CErrorT IO) a

-- | Preprocessor stuff
data PPEnv = PPEnv {
	defines ∷ Map.Map String String,
	ifStack ∷ [Bool],
  warnings ∷ [(Position, String)],
  filepaths ∷ [FilePath],
  currentFile ∷ FilePath,
  currentLine ∷ Int,
  options ∷ Options
}
 deriving (Show)

buildEnv ∷ Options → PPEnv
buildEnv os = PPEnv {
  ifStack = [],
  defines = Map.empty,
  warnings = [],
  filepaths = [],
  currentFile = "",
  currentLine = 0,
  options = os
}

-- | Parse the given source code
parse ∷ String → CError AbsTree
parse src =
  case (pAbsTree . myLexer) src of
    Bad e → Fail $ parseErrM e
    Ok tree → return tree
  where
    parseErrM ∷ String → CompilerError
    parseErrM s | s =~ "line" ∷ Bool = SyntaxError (lineErr s,0) "file" s
                | otherwise = UnknownError s
    lineErr ∷ String → Int
    lineErr s = read (s =~ "[1-9]+" ∷ String) ∷ Int
