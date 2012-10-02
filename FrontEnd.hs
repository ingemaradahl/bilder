{-# LANGUAGE UnicodeSyntax #-}

module FrontEnd where

import Text.Regex.Posix

import FrontEnd.AbsGrammar
import FrontEnd.ParGrammar
import FrontEnd.LexGrammar

import FrontEnd.ErrM

import CompilerError

parse ∷ String → CErr Program
parse src =
  case (pProgram . myLexer) src of
    Bad e → Fail $ parseErrM e
    Ok tree → return tree
  where
    parseErrM ∷ String → CompilerError
    parseErrM s | s =~ "line" ∷ Bool = SyntaxError (lineErr s,0) s
                | otherwise = UnknownError s
    lineErr ∷ String → Int
    lineErr s = read (s =~ "[1-9]+" ∷ String) ∷ Int

