{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.Conversion where

import TypeChecker.Types
import TypeChecker.Utils
import qualified FrontEnd.AbsGrammar as Abs

--paramToVar ∷ FilePath → Abs.Param → Variable
--paramToVar f p = Variable (paramToString p) (f,paramToPos p) (paramType p)
