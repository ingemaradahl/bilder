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

module Compiler.Graph where

import Compiler.Simple.Types
import Compiler.Simple.AbsSimple

import Control.Monad.State
import Control.Applicative

import Text.Printf

import Text.JSON

import Data.Hashable
import qualified Data.Map as Map

type Output = String
type Hash = String

type ShaderName = String
type TranslatedShader = String

data UniformType =
    Texture
  | Float
  | Integer
  | Boolean
  | Vector Int
  | Matrix Int Int
 deriving (Show)

data Input =
    NodeInput String Node
  | Uniform UniformType String
 deriving (Show)

data Node = Node ShaderName Hash [Input]

instance Show Node where
  show (Node name h ins) =
    printf "node %s (%s)\ninputs:\n%s"
      name
      h
      (unlines $ map (\l → "    " ++ l) $ lines $ concatMap ((++"\n") . show) ins)

instance JSON Node where
  showJSON (Node n h is) = makeObj
    [ ("filename", showJSON n)
    , ("hash", showJSON h)
    , ("inputs", showJSON is)
    ]
  readJSON _ = error "one way only, mange."

instance JSON Input where
  showJSON (NodeInput o node) = makeObj
    [ ("name", showJSON o)
    , ("type", showJSON "node")
    , ("node", showJSON node)
    ]
  showJSON (Uniform t n) = makeObj
    [ ("name", showJSON n)
    , ("type", showJSON t)
    ]
  readJSON _ = error "one way only, mange."

instance JSON UniformType where
  showJSON (Texture) = showJSON "texture"
  showJSON (Float) = showJSON "float"
  showJSON (Integer) = showJSON "int"
  showJSON (Boolean) = showJSON "bool"
  showJSON (Vector i) = showJSON ("vec" ++ show i)
  showJSON (Matrix i j) = showJSON (show i ++ "x" ++ show j)
  readJSON _ = error "one way only, mange."

data Shaders = Shaders {
      shaders ∷ Map.Map Hash (ShaderName, TranslatedShader)
    , shaderNames ∷ Map.Map Hash ShaderName
    , oldShaders ∷ [(Shader, TranslatedShader)]
    , freeNums ∷ [Int]
  }
 deriving (Show)

makeGraph ∷ [(Shader, TranslatedShader)] → (Node, [(ShaderName, TranslatedShader)])
makeGraph ss = (node, Map.elems $ shaders ss')
 where
  root = head $ filter (\s → (variableName . output . fst) s == "result_image") ss
  start = Shaders Map.empty Map.empty ss [1..]
  (node, ss') = runState (shaderToNode root) start

shaderToNode ∷ (Shader, TranslatedShader) → State Shaders Node
shaderToNode (s, t) =
  Node <$> addShader hashed t <*> pure hashed <*> mapM findInput ins
 where
  hashed = hashShader t
  ins = Map.elems $ inputs s

newName ∷ State Shaders ShaderName
newName = do
  n ← gets freeNums
  modify (\st → st { freeNums = tail n })
  return $ printf "shader%d.fs" (head n)

addShader ∷ Hash → TranslatedShader → State Shaders ShaderName
addShader h sh = do
  ss ← gets shaders
  case Map.lookup h ss of
    Nothing → do
      name ← newName
      modify (\st → st { shaders = Map.insert h (name, sh) ss })
      return name
    Just (name, _) → return name

findInput ∷ Variable → State Shaders Input
findInput v = do
  ss ← gets oldShaders
  -- find the shader with output `i'.
  case filter (\sh → (variableName . output . fst) sh == i) ss of
    []  → return $ Uniform (typeToUniformType t) i
    ss' → NodeInput i <$> shaderToNode (head ss')
 where
  t = variableType v
  i = variableName v

typeToUniformType ∷ Type → UniformType
--typeToUniformType (TVoid) = Void
typeToUniformType (TFloat) = Float
typeToUniformType (TBool) = Boolean
typeToUniformType (TInt) = Integer
typeToUniformType (TVec2) = Vector 2
typeToUniformType (TVec3) = Vector 3
typeToUniformType (TVec4) = Vector 4
typeToUniformType (TMat2) = Matrix 2 2
typeToUniformType (TMat3) = Matrix 3 3
typeToUniformType (TMat4) = Matrix 4 4
typeToUniformType (TSampler) = Texture
typeToUniformType (TStruct {}) = undefined

hashShader ∷ TranslatedShader → Hash
hashShader s = show $ hash s

graphToJSON ∷ (Node, [(ShaderName, TranslatedShader)]) → JSValue
graphToJSON (g, ss) = makeObj [
    ("graph", showJSON g)
  , ("shaders", JSArray $ map mkShader ss)
  ]
 where
  mkShader (n, s) = JSObject $ toJSObject [
      ("name", showJSON n)
    , ("shader", showJSON s)
    ]

graphToXML ∷ Shaders → String
graphToXML _ = undefined
