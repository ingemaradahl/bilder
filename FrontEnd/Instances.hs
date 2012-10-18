{-# LANGUAGE UnicodeSyntax #-}

module FrontEnd.Instances where

import FrontEnd.AbsGrammar
import CompilerTypes

class Token a where
  tkpos ∷ a → Position
  tkident ∷ a → String
{-
instance Token aa where
  tkpos (aa (p,_)) = p
  tkident (aa (_,s)) = s
-}

instance Token TkPlus where
  tkpos (TkPlus (p,_)) = p
  tkident (TkPlus (_,s)) = s

instance Token TkMinus where
  tkpos (TkMinus (p,_)) = p
  tkident (TkMinus (_,s)) = s

instance Token TkMul where
  tkpos (TkMul (p,_)) = p
  tkident (TkMul (_,s)) = s

instance Token TkDiv where
  tkpos (TkDiv (p,_)) = p
  tkident (TkDiv (_,s)) = s

instance Token TkLt where
  tkpos (TkLt (p,_)) = p
  tkident (TkLt (_,s)) = s
