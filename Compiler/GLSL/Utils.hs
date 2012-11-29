{-# LANGUAGE UnicodeSyntax #-}

module Compiler.GLSL.Utils where

import qualified Data.Map as Map

import Compiler.Simple.AbsSimple
import Compiler.Simple.Types
import Compiler.Simple.Utils

gl_FragCoord, gl_FragColor, gl_FragCoord_xy, fl_Resolution ∷ Exp
gl_FragCoord = EVar "gl_FragCoord"
gl_FragColor = EVar "gl_FragColor"
fl_Resolution = EVar "fl_Resolution"
gl_FragCoord_xy = EMember gl_FragCoord "xy"

finalizeMain ∷ Shader → Shader
finalizeMain shd =
  shd { functions = Map.adjust reworkMain "main" (functions shd) }

reworkMain ∷ Function → Function
reworkMain (Function name _ [x, y] stms) = Function name TVoid [] (d:stms')
 where
  p = Variable "p" TVec2 True
  d = SDeclAss p (EDiv gl_FragCoord_xy fl_Resolution)
  x' = EMember (EVar (variableName p)) "x"
  y' = EMember (EVar (variableName p)) "y"
  stms' = map (subReturn . mapStmExp subXY) stms
  subXY ∷ Exp → Exp
  subXY (EVar s) | s == variableName x = x'
  subXY (EVar s) | s == variableName y = y'
  subXY e = mapExp subXY e
  subReturn ∷ Stm → Stm
  subReturn (SReturn e) = SExp $ EAss gl_FragColor e
  subReturn s = mapStm subReturn s


