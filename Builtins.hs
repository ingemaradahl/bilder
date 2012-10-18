{-# LANGUAGE UnicodeSyntax #-}

module Builtins where

import FrontEnd.AbsGrammar

typeConstuctors ∷ Type → [[Type]]
typeConstuctors TVec2 = [[TFloat],[TVec2], [TFloat,TFloat]]
typeConstuctors TVec3 = [[TFloat],[TVec3]] ++
  map (++ [TFloat]) (tail $ typeConstuctors TVec2)
typeConstuctors TVec4 = [[TFloat],[TVec4],[TVec2, TVec2]] ++
  map (++ [TFloat]) (tail $ typeConstuctors TVec3)
typeConstuctors _ = []

-- | Member-functions (e.g. .map)
componentFunc ∷ Type → String → [Exp] → Maybe (Type, [Exp])
componentFunc (TFun TVec4 [TFloat,TFloat]) "map" [EVar cid] = Just (TFun TVec4 [TVec4], [ECall cid []])
componentFunc _ _ _ = Nothing
