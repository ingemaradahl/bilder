{-# LANGUAGE UnicodeSyntax #-}

module Compiler.GLSL.Utils where

import Compiler.Simple.AbsSimple

gl_FragCoord, gl_FragColor, gl_FragCoord_xy, fl_Resolution ∷ Exp
gl_FragCoord = EVar "gl_FragCoord"
gl_FragColor = EVar "gl_FragColor"
fl_Resolution = EVar "fl_Resolution"
gl_FragCoord_xy = EMember gl_FragCoord "xy"


