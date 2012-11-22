{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Split where

import Compiler.Utils

import TypeChecker.Types
import FrontEnd.AbsGrammar


split ∷ Function → [Source]
split = undefined

createsImg ∷ Stm → Bool
createsImg (SDecl (Dec qs (DecAss _ _ (EPartCall {})))) = qualsToType qs == imgType
createsImg (SExp (EAss _ _ (EPartCall {}))) = True
createsImg (SType t s) = t == imgType && createsImg s
createsImg _ = False

imgType ∷ Type
imgType = TFun TVec4 [TFloat, TFloat]

