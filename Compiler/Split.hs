{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Split where

import Control.Monad.State
import Control.Monad.Writer

import Data.Maybe
import qualified Data.Map as Map

import Compiler.Utils
import TypeChecker.Utils

import TypeChecker.Types (Function, Function(Null), Variable, Source, statements, paramVars, ident)
import qualified TypeChecker.Types as Source (Source(functions), Source(variables))
import FrontEnd.AbsGrammar

import Text.Printf

type Chunk = ([(Function, [Stm])], String)

data Shader = Shader {
    funs ∷ Map.Map String Function
  , vars ∷ Map.Map String Variable
  , output ∷ String
  , inputs ∷ [String]
}

data St = St {
    functions ∷ Map.Map String Function
  , variables ∷ Map.Map String Variable
  , currentFun ∷ Function
  , gobbled ∷ [(Function, [Stm])]
  , freeRefs ∷ [Int]
  , chunks ∷ [Chunk]
}

pushFun ∷ Function → State St ()
pushFun f = modify (\st → st { gobbled = (f,[]):gobbled st})

popFun ∷ State St ()
popFun = modify (\st → st { gobbled = tail (gobbled st)})

addStm ∷ Stm → State St ()
addStm stm = do
  (f, gobs) ← gets (head . gobbled)
  modify (\st → st { gobbled = (f, stm:gobs):tail (gobbled st)})

getFun ∷ String → State St Function
getFun s = liftM (fromJust . Map.lookup s) $ gets functions

splitSource ∷ Source → [Source]
splitSource src = evalState (split mainFun)
  St {
      functions = Source.functions src
    , variables = Source.variables src
    , currentFun = Null
    , gobbled = []
    , freeRefs = [1..]
    , chunks = []
  }
 where
  mainFun ∷ Function
  mainFun = fromJust $ Map.lookup "main" (Source.functions src)

newRef ∷ String → State St String
newRef s = do
  newId ← gets (head . freeRefs)
  modify (\st → st { freeRefs = tail (freeRefs st)})
  return $ printf "img%03d%s" newId s

split ∷ Function → State St [Source]
split fun = do
  collectRewrite fun
  return undefined

buildShader ∷ Chunk → State St Shader
buildShader = undefined


buildFun ∷ (Function, [Stm]) → Function
buildFun (f, ss) = stripArgs $ f { statements = ss }

-- Strips arguments not needed
stripArgs ∷ Function → Function
stripArgs f = f { paramVars = filter (\v → ident v `notElem` usedVars) (paramVars f) }
 where
  usedVars ∷ [String]
  usedVars = execWriter (mapM_ (mapStmExpM collect) (statements f))
  collect ∷ Exp → Writer [String] Exp
  collect e@(EVar cid) = tell [cIdentToString cid] >> return e
  collect e = return e




collectRewrite ∷ Function → State St ()
collectRewrite fun = do
  pushFun fun
  mapM_ gobbleStm (statements fun)
  popFun

gobbleStm ∷ Stm → State St ()
gobbleStm stm = mapStmExpM gobble stm >>= addStm

gobble ∷ Exp → State St Exp
gobble (EPartCall cid es _) = do
  --f ← getFun (cIdentToString cid)
  d ← deps es
  r ← newRef (cIdentToString cid)
  addChunk (d,r)
  return (EVar (CIdent ((0,0),r)))
gobble e@(ECall cid _) = getFun (cIdentToString cid) >>= collectRewrite >> return e
gobble e = return e

deps ∷ [Exp] → State St [(Function, [Stm])]
deps = undefined

addChunk ∷ Chunk → State St ()
addChunk c = modify (\st → st { chunks = c:chunks st })





createsImg ∷ Stm → Bool
createsImg (SDecl (Dec qs (DecAss _ _ (EPartCall {})))) = qualsToType qs == imgType
createsImg (SExp (EAss _ _ (EPartCall {}))) = True
createsImg (SType t s) = t == imgType && createsImg s
createsImg _ = False

branches ∷ Exp → Bool
branches = isJust . branchTarget

branchTarget ∷ Exp → Maybe String
branchTarget = foldExp f Nothing
 where
  f ∷ Maybe String → Exp → Maybe String
  f p (ECall cid _) = p `mplus` Just (cIdentToString cid)
  f p _ = p

imgType ∷ Type
imgType = TFun TVec4 [TFloat, TFloat]

