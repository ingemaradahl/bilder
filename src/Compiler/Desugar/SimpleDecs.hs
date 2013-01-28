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

module Compiler.Desugar.SimpleDecs where

import qualified Data.Map as Map (map)

import Compiler.Utils

import TypeChecker.Types
import FrontEnd.AbsGrammar

simpleDecs ∷ Source → Source
simpleDecs src = src { functions = Map.map simplifyFun (functions src)}

simplifyFun ∷ Function → Function
simplifyFun fun = fun { statements = simpleDec (statements fun )}

simpleDec ∷ [Stm] → [Stm]
simpleDec (SDecl (Dec qs ps):ss) = newDec qs ps ++ simpleDec ss
simpleDec ss = expandStm simpleDec ss

newDec ∷ [Qualifier] → DeclPost → [Stm]
newDec qs (Vars cids) = map (makeDec qs) cids
newDec qs (DecAss cids tk ex) = map (makeDec qs) (init cids) ++ [makeDecAss qs tk ex (last cids)]

makeDec ∷ [Qualifier] → CIdent → Stm
makeDec qs cid = SDecl (Dec qs (Vars [cid]))

makeDecAss ∷ [Qualifier] → TkAss → Exp → CIdent → Stm
makeDecAss qs tk ex cid = SDecl (Dec qs (DecAss [cid] tk ex))
