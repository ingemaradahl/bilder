{-# LANGUAGE UnicodeSyntax #-}

module Compiler.GLSL.Utils where

import Compiler.Simple.AbsSimple

gl_FragCoord, gl_FragColor, gl_FragCoord_xy, gl_FragCoord_x, gl_FragCoord_y, fl_Resolution, fl_Resolution_y ∷ Exp
gl_FragCoord = EVar "gl_FragCoord"
gl_FragColor = EVar "gl_FragColor"
fl_Resolution = EVar "fl_Resolution"
fl_Resolution_y = EMember fl_Resolution "y"
gl_FragCoord_xy = EMember gl_FragCoord "xy"
gl_FragCoord_x = EMember gl_FragCoord "x"
gl_FragCoord_y = EMember gl_FragCoord "y"


