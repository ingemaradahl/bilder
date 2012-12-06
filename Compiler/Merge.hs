{-# LANGUAGE UnicodeSyntax, TupleSections #-}

module Compiler.Merge where

import Control.Arrow
import Control.Monad.State

import Data.Maybe
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





data Merge = Merge {
  currentShader ∷ Shader,
  shaders ∷ Map.Map String Shader,
  samplers ∷ Map.Map String Variable
}

updateSampler ∷ String → String → State Merge ()
updateSampler var samp = do
  smps ← gets samplers
  case Map.lookup samp smps of
    Just s  → modify (\st → st { samplers = Map.insert var s smps})
    Nothing → return () -- Sampler wasn't referencing anything useful :(


--putStr $ unlines $ map printTree $ simpleToGLSL $ testing shdA [shdA,shdB]
testing ∷ Shader → [Shader] → [Shader]
testing startshader shds = (Map.elems . shaders) $ execState (startMerge startshader) (freshState startshader shds)

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
  (Map.fromList $ map ((variableName . output) &&& id) shds)
  (Map.filter ((==) TSampler . variableType) (inputs shd))


-- Finds which shader is referred to with the string s in the current context
resolveShader ∷ String → State Merge (Maybe String)
resolveShader s = gets (fmap variableName . Map.lookup s . samplers)


mergeFun ∷ Function → [Exp] → State Merge ()
mergeFun f args = do
  -- Add sampler aliases to state
  modify (\st → st {
    samplers = Map.union (samplers st) $ Map.fromList
      [ (alias, fromJust (Map.lookup sampler (samplers st)))
        | (Variable alias TSampler _, EVar sampler)
        ← zip (parameters f) args
      ]
    })

  stms ← mapM mergeStm (statements f)

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
                  (\s → mayhaps (sampleCount s < 3) (inline s ex ey))

  -- Tries branching
  let branch = fmap (\g → mergeFun g [ex', ey']) (Map.lookup f funs) >>
                  return (return (ECall f [ex', ey']))

  -- Performs actual computation
  fromJust $ inlined `mplus` branch `mplus` Just (return (ECall f [ex', ey']))
mergeExp (ECall f es) = do
  es' ← mapM mergeExp es
  funs ← gets (functions . currentShader)
  case Map.lookup f funs of
    Just f' → mergeFun f' es' >> return (ECall f es')
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
  modify (\st → st { currentShader = mergeShader (currentShader st) shd'})
  return $ ECall (functionName main') [ey,ex]

-- Merge the second shader into the first, removing unneccecary in/outs
mergeShader ∷ Shader → Shader → Shader
mergeShader orig new = Shader {
      variables = variables orig `Map.union` variables new
    , functions = functions orig `Map.union` functions new
    , output = output orig
    , inputs = Map.filter (\v → variableName v /= variableName (output new)) $
                inputs orig `Map.union` inputs new
  }

renameMain ∷ Function → String → Function
renameMain f s = f { functionName = 'm':s }
