{-# LANGUAGE UnicodeSyntax, TupleSections #-}

module Compiler.Merge where

import Control.Monad.State

import Data.Maybe
import qualified Data.Map as Map

import Compiler.Simple.Utils
import Compiler.Simple.Types
import Compiler.Simple.AbsSimple

data Count = Count {
  images ∷ [Map.Map String String], -- Variables containing sampler alias
  samples ∷ Map.Map String Int,
  shader ∷ Shader
}
 deriving (Show)

emptyState ∷ Shader → Count
emptyState shd = Count
  [Map.empty]
  (Map.fromList (("unknown",0):map ((,0) . variableName)
    (filter (\v → variableType v == TSampler) $ Map.elems (inputs shd))))
  shd

setAlias ∷ String → String → State Count ()
setAlias variable target = modify (\st → st {
  images = Map.insert variable target (head (images st)):tail (images st)})

increment ∷ String → State Count ()
increment s = modify (\st → st { samples = Map.insertWith (+) s 1 (samples st)}) 

mergeShaders ∷ [Shader] → [Shader]
mergeShaders s = s

sampleCount ∷ Shader → Int
sampleCount shd = evalState (countSamples mainFun) (emptyState shd)
 where
  mainFun = fromJust $ Map.lookup "main" (functions shd)

countSamples ∷ Function → State Count Int
countSamples (Function _ _ _ stms) = do
  c ← mapM countStm stms
  return $ sum c

countStm ∷ Stm → State Count Int
countStm (SDecl v) | variableType v == TSampler =
  setAlias (variableName v) "unknown" >> return 0
countStm (SDeclAss v (EVar s)) | variableType v == TSampler =
  setAlias (variableName v) s >> return 0
countStm (SDeclAss v _) | variableType v == TSampler =
  setAlias (variableName v) "unknown" >> -- Find out which sampler from exp?
  return 0
countStm (SExp (EAss (EVar var) (EVar samp))) = do
  aliases ← gets (head . images)
  case Map.lookup var aliases of
    Just s  → setAlias s samp >> return 0
    Nothing → return 0
countStm s = foldStmExpM (\p e → liftM (+p) (countExp e)) 0 s


countExp ∷ Exp → State Count Int
countExp (ECall name [_, _]) = do
  aliases ← gets (head . images)
  ins ← gets (inputs . shader)
  case Map.lookup name aliases `mplus` fmap variableName (Map.lookup name ins) of
    Just s  → increment s >> return 1
    Nothing → do
      funs ← gets (functions . shader)
      case Map.lookup name funs of
        Just f  → countSamples f
        Nothing → return 0
countExp (ECall name es) = do
  funs ← gets (functions . shader)
  case Map.lookup name funs of
    Just f  → branchFun f es
    Nothing → return 0
countExp _ = return 0

branchFun ∷ Function → [Exp] → State Count Int
branchFun fun args = do
  modify (\st → st { images = Map.empty:images st})

  -- Match arguments with expressions, and set aliases where applicable
  mapM_ (\(v,e) → case e of
                    EVar n → setAlias (variableName v) n
                    _ → setAlias (variableName v) "unknown") $
        filter (\(v, _) → variableType v == TSampler) $ zip (parameters fun) args
  c ← countSamples fun
  modify (\st → st { images = tail (images st)})
  return c

