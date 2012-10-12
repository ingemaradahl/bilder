{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.Conversion where

import TypeChecker.Types
import TypeChecker.Utils
import qualified FrontEnd.AbsGrammar as Abs

tcFun ∷ FilePath → Abs.Toplevel → Function
tcFun file (Abs.Function t cident params stms) =
  Function {
    functionName = cIdentToString cident,
    functionLocation = (file, cIdentToPos cident),
    retType = t,
    parameters = params,
    statements = stms
  }
