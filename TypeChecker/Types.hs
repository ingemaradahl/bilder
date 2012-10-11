{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.Types where

import CompilerError
import FrontEnd.AbsGrammar

-- | Type of functions, where first value is return type, second is arguments
type Function = (FilePath, Position, Type, [Type])
type Variable = (FilePath, Position, Type)

