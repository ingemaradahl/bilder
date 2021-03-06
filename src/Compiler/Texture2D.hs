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

module Compiler.Texture2D (
    texture2D
  ) where

import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import Compiler.Simple.AbsSimple
import Compiler.Simple.Types
import Compiler.Simple.Utils

import Control.Arrow


-- Texture state - Map of Variable to Type
type TEX a = State (Map.Map String Type) a

-- | Replaces all Image function calls with texture lookups.
texture2D ∷ Shader → Shader
texture2D sh = sh { functions = sh' }
 where
  st = Map.fromList $ map (variableName &&& variableType) (Map.elems (inputs sh) ++ Map.elems (variables sh))
  sh' = Map.map (\f → evalState (replaceFun f) st) (functions sh)

addVar ∷ String → Type → TEX ()
addVar n t = get >>= \m → put $ Map.insert n t m

addVariable ∷ Variable → TEX ()
addVariable v = addVar (variableName v) (variableType v)

replaceFun ∷ Function → TEX Function
replaceFun fun = do
  mapM_ addVariable (parameters fun)
  stms' ← mapM replaceStm (statements fun)
  return fun { statements = stms' }

replaceStm ∷ Stm → TEX Stm
replaceStm s@(SDecl v) = addVariable v >> mapStmExpM replaceExp s
replaceStm (SDeclAss v e) = addVariable v >> SDeclAss v <$> replaceExp e
replaceStm s = mapStmM replaceStm s >>= mapStmExpM replaceExp

replaceExp ∷ Exp → TEX Exp
replaceExp (ECall s es) = do
  m ← get
  case Map.lookup s m of
    Just TSampler  → do
      es' ← mapM replaceExp es
      return $ ECall "texture2D" [EVar s, ETypeCall TVec2 es']
    _ → ECall s <$> mapM replaceExp es
replaceExp e = mapExpM replaceExp e
