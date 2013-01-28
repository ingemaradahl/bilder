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

module FrontEnd.Instances where

import FrontEnd.AbsGrammar
import CompilerTypes

class Token a where
  tkpos ∷ a → Position
  tkident ∷ a → String

instance Token TkImport where
  tkpos (TkImport (p,_)) = p
  tkident (TkImport (_,s)) = s

instance Token TkReturn where
  tkpos (TkReturn (p,_)) = p
  tkident (TkReturn (_,s)) = s

instance Token TkFor where
  tkpos (TkFor (p,_)) = p
  tkident (TkFor (_,s)) = s

instance Token TkWhile where
  tkpos (TkWhile (p,_)) = p
  tkident (TkWhile (_,s)) = s

instance Token TkDo where
  tkpos (TkDo (p,_)) = p
  tkident (TkDo (_,s)) = s

instance Token TkIf where
  tkpos (TkIf (p,_)) = p
  tkident (TkIf (_,s)) = s

instance Token TkElse where
  tkpos (TkElse (p,_)) = p
  tkident (TkElse (_,s)) = s

instance Token TkStruct where
  tkpos (TkStruct (p,_)) = p
  tkident (TkStruct (_,s)) = s

instance Token TkBreak where
  tkpos (TkBreak (p,_)) = p
  tkident (TkBreak (_,s)) = s

instance Token TkContinue where
  tkpos (TkContinue (p,_)) = p
  tkident (TkContinue (_,s)) = s

instance Token TkDiscard where
  tkpos (TkDiscard (p,_)) = p
  tkident (TkDiscard (_,s)) = s

instance Token TkConst where
  tkpos (TkConst (p,_)) = p
  tkident (TkConst (_,s)) = s

instance Token TkExternal where
  tkpos (TkExternal (p,_)) = p
  tkident (TkExternal (_,s)) = s

instance Token TkPixelwise where
  tkpos (TkPixelwise (p,_)) = p
  tkident (TkPixelwise (_,s)) = s

instance Token TkBounded where
  tkpos (TkBounded (p,_)) = p
  tkident (TkBounded (_,s)) = s

instance Token TkTypeDef where
  tkpos (TkTypeDef (p,_)) = p
  tkident (TkTypeDef (_,s)) = s

instance Token TkOR where
  tkpos (TkOR (p,_)) = p
  tkident (TkOR (_,s)) = s

instance Token TkXOR where
  tkpos (TkXOR (p,_)) = p
  tkident (TkXOR (_,s)) = s

instance Token TkAnd where
  tkpos (TkAnd (p,_)) = p
  tkident (TkAnd (_,s)) = s

instance Token TkEqual where
  tkpos (TkEqual (p,_)) = p
  tkident (TkEqual (_,s)) = s

instance Token TkNEqual where
  tkpos (TkNEqual (p,_)) = p
  tkident (TkNEqual (_,s)) = s

instance Token TkArrow where
  tkpos (TkArrow (p,_)) = p
  tkident (TkArrow (_,s)) = s

instance Token TkPlusPlus where
  tkpos (TkPlusPlus (p,_)) = p
  tkident (TkPlusPlus (_,s)) = s

instance Token TkMinusMinus where
  tkpos (TkMinusMinus (p,_)) = p
  tkident (TkMinusMinus (_,s)) = s

instance Token TkAssAdd where
  tkpos (TkAssAdd (p,_)) = p
  tkident (TkAssAdd (_,s)) = s

instance Token TkAssSub where
  tkpos (TkAssSub (p,_)) = p
  tkident (TkAssSub (_,s)) = s

instance Token TkAssMul where
  tkpos (TkAssMul (p,_)) = p
  tkident (TkAssMul (_,s)) = s

instance Token TkBWShiftLeft where
  tkpos (TkBWShiftLeft (p,_)) = p
  tkident (TkBWShiftLeft (_,s)) = s

instance Token TkBWShiftRight where
  tkpos (TkBWShiftRight (p,_)) = p
  tkident (TkBWShiftRight (_,s)) = s

instance Token TkLEt where
  tkpos (TkLEt (p,_)) = p
  tkident (TkLEt (_,s)) = s

instance Token TkGEt where
  tkpos (TkGEt (p,_)) = p
  tkident (TkGEt (_,s)) = s

instance Token TkAssDiv where
  tkpos (TkAssDiv (p,_)) = p
  tkident (TkAssDiv (_,s)) = s

instance Token TkAssMod where
  tkpos (TkAssMod (p,_)) = p
  tkident (TkAssMod (_,s)) = s

instance Token TkAssBWAnd where
  tkpos (TkAssBWAnd (p,_)) = p
  tkident (TkAssBWAnd (_,s)) = s

instance Token TkAssBWXOR where
  tkpos (TkAssBWXOR (p,_)) = p
  tkident (TkAssBWXOR (_,s)) = s

instance Token TkAssBWOR where
  tkpos (TkAssBWOR (p,_)) = p
  tkident (TkAssBWOR (_,s)) = s

instance Token TkQuest where
  tkpos (TkQuest (p,_)) = p
  tkident (TkQuest (_,s)) = s

instance Token TkColon where
  tkpos (TkColon (p,_)) = p
  tkident (TkColon (_,s)) = s

instance Token TkAss where
  tkpos (TkAss (p,_)) = p
  tkident (TkAss (_,s)) = s

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

instance Token TkMod where
  tkpos (TkMod (p,_)) = p
  tkident (TkMod (_,s)) = s

instance Token TkGt where
  tkpos (TkGt (p,_)) = p
  tkident (TkGt (_,s)) = s

instance Token TkLt where
  tkpos (TkLt (p,_)) = p
  tkident (TkLt (_,s)) = s

instance Token TkNegSign where
  tkpos (TkNegSign (p,_)) = p
  tkident (TkNegSign (_,s)) = s

instance Token TkComplement where
  tkpos (TkComplement (p,_)) = p
  tkident (TkComplement (_,s)) = s

instance Token TkBWOR where
  tkpos (TkBWOR (p,_)) = p
  tkident (TkBWOR (_,s)) = s

instance Token TkBWXOR where
  tkpos (TkBWXOR (p,_)) = p
  tkident (TkBWXOR (_,s)) = s

instance Token TkBWAnd where
  tkpos (TkBWAnd (p,_)) = p
  tkident (TkBWAnd (_,s)) = s

instance Token CIdent where
  tkpos (CIdent (p,_)) = p
  tkident (CIdent (_,s)) = s

instance Token TypeIdent where
  tkpos (TypeIdent (p,_)) = p
  tkident (TypeIdent (_,s)) = s

