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

module Compiler.SimpleInliner where

import Prelude hiding (mapM)

import Control.Monad.State hiding (mapM)

import Compiler.Simple.AbsSimple
import Compiler.Simple.Types
import Compiler.Simple.Utils
import Utils

import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable


-- Inlines function only concisting of SReturn exp
simpleInline ∷ Shader → Shader
simpleInline ss = execState (mapM simpleInlineFun (functions ss)) ss

simpleInlineFun ∷ Function → State Shader ()
simpleInlineFun f = do
  stms ← mapM (mapStmExpM (simpleInlineExp (pixelwise f))) $ statements f
  modify (\st → st { functions = Map.insert name (f { statements = stms}) (functions st) })
 where
  name = functionName f

simpleInlineExp ∷ Bool → Exp → State Shader Exp
simpleInlineExp px e@(ECall f es) = do
  es' ← mapM (simpleInlineExp px) es
  funs ← gets functions
  case Map.lookup f funs >>= (\x → mayhaps (simpleEnough x px) x) of
    Just fun → do
      let (SReturn ex) = head (statements fun)
      return $ replaceVars
                  (Map.fromList $ zip (map variableName (parameters fun)) es')
                  ex
    Nothing → return e
simpleInlineExp px e = mapExpM (simpleInlineExp px) e

replaceVars ∷ Map.Map String Exp → Exp → Exp
replaceVars trans = replace
 where
  replace (EVar v) = fromMaybe (EVar v) (Map.lookup v trans)
  replace (ECall n es) = fromMaybe (ECall n (map (replaceVars trans) es)) (Map.lookup n trans >>= (\(EVar v) → Just $ ECall v (map (replaceVars trans) es)))
  replace e = mapExp replace e

-- Sanity check
simpleEnough ∷ Function → Bool → Bool
simpleEnough f px | countStms f == 1 && pixelwise f == px =
  case head (statements f) of
    SReturn _ → True
    _ → False
simpleEnough _ _ = False

countStms ∷ Function → Int
countStms = length . statements
