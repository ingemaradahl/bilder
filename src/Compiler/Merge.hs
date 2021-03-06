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
{-# LANGUAGE UnicodeSyntax, TupleSections #-}

module Compiler.Merge where

import Control.Arrow
import Control.Monad.State

import Data.Maybe
import Data.List
import qualified Data.Map as Map

import Utils
import Compiler.Simple.Utils
import Compiler.Simple.Types
import Compiler.Simple.AbsSimple



data Count = Count {
  images ∷ [Map.Map String String], -- Variables containing sampler alias
  samples ∷ Map.Map String Int,
  shader ∷ Shader
}
 deriving (Show)

type Functions = Map.Map String Function

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

sampleCount ∷ Shader → Int
sampleCount shd = evalState (countSamples mainFun) (emptyState shd)
 where
  mainFun = fromJust $ Map.lookup "main" (functions shd)

countSamples ∷ Function → State Count Int
countSamples (Function _ _ _ _ stms) = do
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
countStm (SFor {}) = return 9001
countStm (SDoWhile {}) = return 9001
countStm (SWhile {}) = return 9001
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




mergeShaders ∷ [Shader] → [Shader]
mergeShaders ss = dropShaders $ if clean then ss' else mergeShaders ss'
 where
  (clean, ss') = ((not . dirty) &&& (Map.elems . shaders)) $ execState (startMerge mainShd) (freshState mainShd ss)
  mainShd = head $ filter isMain ss

-- Drop shaders not needed any more due to them being merged in to another shader
dropShaders ∷ [Shader] → [Shader]
dropShaders ss = filter needed ss
 where
  ins = nub $ "result_image":concatMap (map variableName . Map.elems . inputs) ss
  needed ∷ Shader → Bool
  needed s = variableName (output s) `elem` ins

isMain ∷ Shader → Bool
isMain = (==) "result_image" . variableName . output

data Merge = Merge {
  currentShader ∷ Shader,
  currentFunction ∷ Function,
  shaders ∷ Map.Map String Shader,
  samplers ∷ Map.Map String Variable,
  dirty ∷ Bool
}
  deriving (Show)

updateSampler ∷ String → String → State Merge ()
updateSampler var samp = do
  smps ← gets samplers
  case Map.lookup samp smps of
    Just s  → modify (\st → st { samplers = Map.insert var s smps})
    Nothing → return () -- Sampler wasn't referencing anything useful :(


startMerge ∷ Shader → State Merge ()
startMerge shd = do
  let f = fromJust $ Map.lookup "main" (functions shd)
  stms ← mapM mergeStm (statements f)

  -- copypasta:
  -- Update state
  currShader ← gets currentShader
  let shader' = currShader {
      functions = Map.insert
                    (functionName f)
                    f { statements = stms}
                    (functions currShader)
    }

  modify (\st → st {
        currentShader = shader'
      , shaders = Map.insert (variableName (output shader')) shader' (shaders st)
    })

freshState ∷ Shader → [Shader] → Merge
freshState shd shds = Merge
  shd
  (fromJust $ Map.lookup "main" $ functions shd)
  (Map.fromList $ map ((variableName . output) &&& id) shds)
  (Map.filter ((==) TSampler . variableType) (inputs shd))
  False


-- Finds which shader is referred to with the string s in the current context
resolveShader ∷ String → State Merge (Maybe String)
resolveShader s = gets (fmap variableName . Map.lookup s . samplers)


mergeFun ∷ Function → [Exp] → State Merge (String, [Exp])
mergeFun f args = do
  -- Add sampler aliases to state
  modify (\st → st {
    samplers = Map.union (samplers st) $ Map.fromList
      [ (alias, let Just v = (Map.lookup sampler (samplers st)) in v)
        | (Variable alias TSampler _ _, EVar sampler)
        ← zip (parameters f) args
      ]
    })

  stms ← mapM mergeStm (statements f)
  if stms == statements f
    then return (functionName f, args)
    else do -- Stuff has changed, we might not be dependent on samplers as args any more
      newName ← iterateFun (functionName f)

      let references = usedVars stms
      let (params', args') = unzip $ filter ((`elem` references) . variableName . fst) $ zip (parameters f) args

      -- Update state
      currShader ← gets currentShader
      let shader' = currShader {
          functions = Map.insert
                        newName
                        f { functionName = newName, parameters = params', statements = stms }
                        (functions currShader)
        }

      modify (\st → st {
            currentShader = shader'
          , shaders = Map.insert (variableName (output shader')) shader' (shaders st)
        })

      return (newName, args')

mergeStm ∷ Stm → State Merge Stm
mergeStm s@(SDeclAss var (EVar samp)) | variableType var == TSampler =
  updateSampler (variableName var) samp >> return s
mergeStm s = mapStmExpM mergeExp s

mergeExp ∷ Exp → State Merge Exp
mergeExp (ECall f [ex, ey]) = do
  shd  ← resolveShader f
  shds ← gets shaders
  ex'  ← mergeExp ex
  ey'  ← mergeExp ey
  funs ← gets (functions . currentShader)

  -- Tries inlining
  let inlined = shd >>= flip Map.lookup shds >>=
                  (\s → mayhaps (sampleCount s < 3) (inline s ex' ey'))

  -- Tries branching
  let branch = fmap (\g → mergeFun g [ex', ey'] >>= (\(fun, args) → return (ECall fun args))) (Map.lookup f funs)

  -- Performs actual computation
  fromJust $ inlined `mplus` branch `mplus` Just (return (ECall f [ex', ey']))
mergeExp (ECall f es) = do
  es' ← mapM mergeExp es
  funs ← gets (functions . currentShader)
  case Map.lookup f funs of
    Just f' → mergeFun f' es' >>= (\(fun, args) → return (ECall fun args))
    Nothing → return (ECall f es')
mergeExp e@(EAss (EVar v) (EVar s)) = -- Naïve solution :S
  updateSampler v s >>
  return e
mergeExp e = mapExpM mergeExp e


inline ∷ Shader → Exp → Exp → State Merge Exp
inline shd ex ey = do
  let mainF = fromJust $ Map.lookup "main" (functions shd)
  let main' = renameMain mainF (variableName $ output shd)
  let shd' = shd {
    functions = (Map.insert (functionName main') main' . Map.delete "main") (functions shd)}
  modify (\st → st { currentShader = mergeShader (currentShader st) shd', dirty = True})
  return $ ECall (functionName main') [ex,ey]

-- Merge the second shader into the first, removing unneccecary in/outs
mergeShader ∷ Shader → Shader → Shader
mergeShader orig new = Shader {
      variables = variables orig `Map.union` variables new
    , functions = functions orig `Map.union` (uniquify orig new `Map.union` functions new)
    , output = output orig
    , inputs = Map.filter (\v → variableName v /= variableName (output new)) $
                inputs orig `Map.union` inputs new
  }
 where
  uniquify ∷ Shader → Shader → Functions
  uniquify shOrig shNew =
    if Map.null $ intersec shOrig shNew
      then functions shNew
      else uniquify shOrig (renameShader (Map.keys $ intersec shOrig shNew) shNew)
  intersec ∷ Shader → Shader → Functions
  intersec shOrig shNew = Map.filterWithKey
    (\k v → let Just y  = (Map.lookup k (functions shOrig)) in statements v /= statements y)
    $ functions shNew `Map.intersection` functions shOrig
  renameShader ∷ [String] → Shader → Shader
  renameShader ss shd = foldr (\s pr → pr { functions = renameCalls (functions pr) s ('r':s) }) shd ss
  renameCalls ∷ Functions → String → String → Functions
  renameCalls fs fun newName =
    Map.fromList $ map (\(k,f) →
      let n = if k == fun then newName else k in
      (n,f {
        statements = map (mapStmExp (renameExp fun newName)) (statements f),
        functionName = n
      })) $ Map.toList fs
  renameExp ∷ String → String → Exp → Exp
  renameExp fun replace (ECall str es) | str == fun = ECall replace (map (renameExp fun replace) es)
  renameExp fun replace e = mapExp (renameExp fun replace) e

renameMain ∷ Function → String → Function
renameMain f s = f { functionName = 'm':s }

iterateFun ∷ String → State Merge String
iterateFun s = iterateFun' ['a'..'z']
 where
  iterateFun' ∷ String → State Merge String
  iterateFun' more = do
    funs ← gets (concatMap (Map.keys . functions) . Map.elems . shaders)
    if s ++ [head more] `elem` funs
      then iterateFun' (tail more)
      else return $ s ++ [head more]

