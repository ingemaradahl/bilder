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

module Compiler.Simple where

import Text.JSON

import Control.Arrow
import qualified Data.Map as Map

import Compiler.Simple.Types as Simple
import qualified Compiler.Split as Split

import Compiler.Simple.AbsSimple
import qualified FrontEnd.AbsGLSL as G
import qualified FrontEnd.PrintGLSL as PG
import Compiler.Simple.FromAbsGrammar

import Compiler.Graph (makeGraph, graphToJSON)

import Compiler.Simple.ToGLSL (
    funToPrototype
  , funToGLSL
  , varToGLSLDecl
  , typeToGLSL
  )

absToSimple ∷ [Split.Shader] → [Simple.Shader]
absToSimple = map splitShaderToSimple

splitShaderToSimple ∷ Split.Shader → Simple.Shader
splitShaderToSimple shd = Shader {
      functions = Map.map slimFunToSimple (Split.funs shd)
    , variables = Map.map slimVarToSimple (Split.vars shd)
    , output = Variable (Split.output shd) TSampler False Nothing
    , inputs = Map.fromList $ map (Split.varName &&& slimVarToSimple) (Split.inputs shd)
  }

slimFunToSimple ∷ Split.SlimFun → Function
slimFunToSimple (Split.SlimFun name ret px args stms) =
  Function name (translate ret) px (map slimVarToSimple args) (map translate stms)

slimVarToSimple ∷ Split.SlimVar → Variable
slimVarToSimple (Split.SlimVar name typ e) =
  Variable name (translate typ) False (fmap translate e)

-- | Translates Simple to GLSL tree.
simpleToGLSL ∷ [Shader] → JSValue
simpleToGLSL ss = graphToJSON $ makeGraph $ map (\s → (s, PG.printTree $ simpleToGLSLShader s)) ss

simpleToGLSLShader ∷ Shader → G.Tree
simpleToGLSLShader (Shader funs vars _ ins) = G.Tree $
  --map structToGLSL (structs blob) ++
  map (G.TopDecl . uniform) (Map.elems ins) ++
  map (G.TopDecl . varToGLSLDecl) (Map.elems vars) ++
  map funToPrototype (Map.elems funs) ++
  map funToGLSL (Map.elems funs)

uniform ∷ Variable → G.Decl
uniform v = G.Declaration
  [G.DQStorage G.QUniform]
  (typeToGLSL $ variableType v)
  [G.Ident $ variableName v]
