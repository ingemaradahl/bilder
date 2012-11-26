{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Split where

import Control.Monad.State

import Data.Maybe
import qualified Data.Map as Map

import Compiler.Utils
import TypeChecker.Utils

import TypeChecker.Types (Function, Function(Null), Variable, Source, statements)
import qualified TypeChecker.Types as Source (Source(functions), Source(variables))
import FrontEnd.AbsGrammar

import Text.Printf

type Chunk = (Function, [[Stm]], String)

data St = St {
    functions ∷ Map.Map String Function
  , variables ∷ Map.Map String Variable
  , currentFun ∷ Function
  , gobbled ∷ [[Stm]]
  , freeRefs ∷ [Int]
  , chunks ∷ [Chunk]
}

pushFun ∷ State St ()
pushFun = modify (\st → st { gobbled = []:gobbled st})

popFun ∷ State St ()
popFun = modify (\st → st { gobbled = tail (gobbled st)})

addStm ∷ Stm → State St ()
addStm stm = modify (\st → st {
  gobbled = (stm:head (gobbled st)):tail (gobbled st)})

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

collectRewrite ∷ Function → State St ()
collectRewrite fun = do
  pushFun
  mapM_ gobbleStm (statements fun)
  popFun

gobbleStm ∷ Stm → State St ()
gobbleStm stm = mapStmExpM gobble stm >>= addStm

gobble ∷ Exp → State St Exp
gobble (EPartCall cid es _) = do
  f ← getFun (cIdentToString cid)
  d ← deps es
  r ← newRef (cIdentToString cid)
  addChunk (f,d,r)
  return (EVar (CIdent ((0,0),r)))
gobble e@(ECall cid _) = do
  f ← getFun (cIdentToString cid)
  collectRewrite f
  return e
gobble e = return e

deps ∷ [Exp] → State St [[Stm]]
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

