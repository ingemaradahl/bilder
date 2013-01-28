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

module Compiler.AvoidAlias where

import qualified Data.Map as Map

import Compiler.Simple.AbsSimple
import Compiler.Simple.Types
import Compiler.Simple.Utils

-- | Strips the code of unneccecary variable aliases
avoidAlias ∷ Shader → Shader
avoidAlias sh = sh { functions = Map.map replaceFun (functions sh) }

replaceFun ∷ Function → Function
replaceFun fun = fun { statements = replaceStm (statements fun)}

replaceStm ∷ [Stm] → [Stm]
replaceStm (SDecl v:ss) | variableName v `notElem` usedVars ss =
  expandStm replaceStm ss
replaceStm (SDeclAss vl (EVar vr):ss) | vr `notElem` usedVars ss =
  let ss' = replaceVar (variableName vl) vr ss in
  expandStm replaceStm ss'
replaceStm (s:ss) = s:replaceStm ss
replaceStm [] = []

replaceVar ∷ String → String → [Stm] → [Stm]
replaceVar find new = map (mapStmExp replace)
 where
  replace ∷ Exp → Exp
  replace (EVar s) | s == find = EVar new
  replace (ECall s es) | s == find = ECall new $ map replace es
  replace e = mapExp replace e
