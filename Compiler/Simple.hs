{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Simple where

import Control.Arrow
import qualified Data.Map as Map

import Compiler.Simple.Types as Simple
import qualified Compiler.Split as Split

import Compiler.Simple.AbsSimple
--import qualified FrontEnd.AbsGrammar as G
import Compiler.Simple.FromAbsGrammar

{-
 -import Compiler.Simple.ToGLSL (funToPrototype, funToGLSL)
 -}

absToSimple ∷ [Split.Shader] → [Simple.Shader]
absToSimple = map splitShaderToSimple

splitShaderToSimple ∷ Split.Shader → Simple.Shader
splitShaderToSimple shd = Shader {
      functions = Map.map slimFunToSimple (Split.funs shd)
    , variables = Map.map slimVarToSimple (Split.vars shd)
    , output = Variable (Split.output shd) TSampler False
    , inputs = Map.fromList $ map (Split.varName &&& slimVarToSimple) (Split.inputs shd)
  }

slimFunToSimple ∷ Split.SlimFun → Function
slimFunToSimple (Split.SlimFun name ret args stms) =
  Function name (translate ret) (map slimVarToSimple args) (map translate stms)

slimVarToSimple ∷ Split.SlimVar → Variable
slimVarToSimple (Split.SlimVar name typ) =
  Variable name (translate typ) False

{-
 --- | Translates Simple to GLSL tree.
 -simpleToGLSL ∷ SimpleSource → G.Tree
 -simpleToGLSL blob = G.Tree $
 -  map structToGLSL (structs blob) ++
 -  map (G.TopDecl . varToGLSLDecl) (variables blob) ++
 -  map funToPrototype (functions blob) ++
 -  map funToGLSL (functions blob)
 -
 --- | Translates FL tree to Simple.
 -flToSimple ∷ F.AbsTree → SimpleSource
 -flToSimple _ = undefined
 -}

