{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Graph where

import Compiler.Simple.Types
import Compiler.Simple.AbsSimple

import Control.Monad.State
import Control.Applicative

import Control.Arrow

import Text.Printf

import Data.Hashable
import qualified Data.Map as Map

type Output = String
type Hash = String

type ShaderName = String
type TranslatedShader = String

data UniformType =
    Texture
  | Float
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
  Node <$> addShader hashed t <*> pure hashed <*> mapM (findInput s) ins
 where
  hashed = hashShader t
  ins = map variableName $ Map.elems $ inputs s

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

findInput ∷ Shader → String → State Shaders Input
findInput s i = do
  ss ← gets oldShaders
  -- find the shader with output `i'.
  case filter (\sh → (variableName . output . fst) sh == i) ss of
    []  → return $ Uniform Texture i -- TODO: Look up the actual type.
    ss' → NodeInput i <$> shaderToNode (head ss')

hashShader ∷ TranslatedShader → Hash
hashShader s = show $ hash s

graphToJSON ∷ (Node, [(ShaderName, TranslatedShader)]) → (String, [(String, String)])
graphToJSON = first show

graphToXML ∷ Shaders → String
graphToXML _ = undefined
