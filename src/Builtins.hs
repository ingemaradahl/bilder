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

module Builtins where

import FrontEnd.AbsGrammar

typeConstuctors ∷ Type → [[Type]]
typeConstuctors TVec2 = [[TFloat],[TVec2], [TFloat,TFloat]]
typeConstuctors TVec3 = [[TFloat],[TVec3]] ++
  map (++ [TFloat]) (tail $ typeConstuctors TVec2)
typeConstuctors TVec4 = [[TFloat],[TVec4],[TVec2, TVec2]] ++
  map (++ [TFloat]) (tail $ typeConstuctors TVec3)
typeConstuctors TMat2 = [[TFloat], replicate 2 TVec2, replicate 4 TFloat]
typeConstuctors TMat3 = [[TFloat], replicate 3 TVec3, replicate 9 TFloat]
typeConstuctors TMat4 = [[TFloat], replicate 4 TVec4, replicate 16 TFloat]
typeConstuctors _ = []

-- | Member-functions (e.g. .map)
--   Returns (Returntype, [Arg type], [Translated expression])
componentFunc ∷ String → Type → [Exp] → Maybe (Type, [Type], [Exp])
-- Image.map(Color → Color) → Color
componentFunc "map" (TFun TVec4 [TFloat,TFloat]) [EVar cid] = Just (TVec4, [TFun TVec4 [TVec4]], [ECall cid []])
-- Image.size() → Vec2
componentFunc "size" (TFun TVec4 [TFloat,TFloat]) [] = Just (TVec2, [], [])
-- Image.index(Int) → TFloat
componentFunc "index" (TFun TVec4 [TFloat,TFloat]) es = Just (TFloat, [TInt], es)
componentFunc _ _ _ = Nothing
