{-
 -      This file is part of Bilder.
 -
 -   Bilder is free software: you can redistribute it and/or modify
 -   it under the terms of the GNU Lesser General Public License as published by
 -   the Free Software Foundation, either version 3 of the License, or
 -   (at your option) any later version.
 -
 -   Bilder is distributed in the hope that it will be useful,
 -   but WITHOUT ANY WARRANTY; without even the implied warranty of
 -   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -   GNU Lesser General Public License for more details.
 -
 -   You should have received a copy of the GNU Lesser General Public License
 -   along with Bilder.  If not, see <http://www.gnu.org/licenses/>.
 -
 -   Copyright © 2012-2013 Filip Lundborg
 -   Copyright © 2012-2013 Ingemar Ådahl
 -
 -}
{-# LANGUAGE UnicodeSyntax #-}

module Compiler.GLSL.Utils where

import Compiler.Simple.AbsSimple

gl_FragCoord, gl_FragColor, gl_FragCoord_xy, gl_FragCoord_x, gl_FragCoord_y, fl_Resolution, fl_Resolution_x, fl_Resolution_y ∷ Exp
gl_FragCoord = EVar "gl_FragCoord"
gl_FragColor = EVar "gl_FragColor"
fl_Resolution = EVar "fl_Resolution"
fl_Resolution_x = EMember fl_Resolution "x"
fl_Resolution_y = EMember fl_Resolution "y"
gl_FragCoord_xy = EMember gl_FragCoord "xy"
gl_FragCoord_x = EMember gl_FragCoord "x"
gl_FragCoord_y = EMember gl_FragCoord "y"


