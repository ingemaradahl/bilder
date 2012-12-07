{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Simple where

import Control.Arrow
import qualified Data.Map as Map

import Compiler.Simple.Types as Simple
import qualified Compiler.Split as Split

import Compiler.Simple.AbsSimple
import qualified FrontEnd.AbsGLSL as G
import Compiler.Simple.FromAbsGrammar

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
slimFunToSimple (Split.SlimFun name ret args stms) =
  Function name (translate ret) (map slimVarToSimple args) (map translate stms)

slimVarToSimple ∷ Split.SlimVar → Variable
slimVarToSimple (Split.SlimVar name typ e) =
  Variable name (translate typ) False (fmap translate e)

-- | Translates Simple to GLSL tree.
simpleToGLSL ∷ [Shader] → [G.Tree]
simpleToGLSL = map simpleToGLSLShader

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


