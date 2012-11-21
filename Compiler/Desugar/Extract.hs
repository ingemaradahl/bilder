{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Desugar.Extract  where

import Control.Monad.State

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Traversable as Trav (mapM)

import Compiler.Utils
import TypeChecker.Utils

import TypeChecker.Types
import FrontEnd.AbsGrammar

import Text.Printf

data Ids = Ids {
  unused ∷ [Int],
  pending ∷ [Stm],
  funs ∷ Map.Map String Function
}

newVar ∷ State Ids Exp
newVar = do
  e ← gets (printf "_l%03d" . head . unused)
  modify (\st → st { unused = tail (unused st) })
  return $ EVar (CIdent ((0,0), e))

putDec ∷ Stm → State Ids ()
putDec stm = modify (\st → st { pending = pending st ++ [stm] })

getPendings ∷ State Ids [Stm]
getPendings = do
  p ← gets pending
  modify (\st → st { pending = []})
  return p

getFun ∷ String → State Ids Function
getFun s = do
  fs ← gets funs
  return $ fromJust $ Map.lookup s fs


expandSource ∷ Source → Source
expandSource src = src { functions = evalState (Trav.mapM expandFun (functions src)) (Ids [1..] [] (functions src))}

expandFun ∷ Function → State Ids Function
expandFun fun = do
  stms ← expand (statements fun)
  return $ fun { statements = stms}

expand ∷ [Stm] → State Ids [Stm]
expand (SExp e:ss) = do
  e' ← extractCalls e
  sdecs ← getPendings
  ss' ← expand ss
  return $ sdecs ++ [SExp e'] ++ ss'
expand (SWhile tkw e stm:ss) = do
  e' ← extractCalls e
  sdecs ← getPendings
  stm' ← liftM makeBlock $ expand [stm]
  ss' ← expand ss
  return $ sdecs ++ [SWhile tkw e' stm'] ++ ss'
expand (SDoWhile tkd stm tkw e:ss) = do
  stms ← expand [stm]
  e' ← extractCalls e
  sdecs ← getPendings
  let stm' = makeBlock $ stms ++ sdecs
  ss' ← expand ss
  return $ SDoWhile tkd stm' tkw e':ss'
--expand SFor ...
--expand SIf|SIfElse..
expand (SReturn tkr e:ss) = do
  e' ← extractCalls e
  sdecs ← getPendings
  ss' ← expand ss
  return $ sdecs ++ [SReturn tkr e'] ++ ss'
expand ss = expandStmM expand ss

extractCalls ∷ Exp → State Ids Exp
extractCalls (EPartCall cid es ts) = do
  es' ← mapM extractCalls es
  var@(EVar varCid) ← newVar
  f ← getFun (cIdentToString cid)
  let typ = TFun (retType f) $ map varType $ drop (length ts) (paramVars f)
  putDec $ makeDec varCid typ (EPartCall cid es' ts)
  return var
extractCalls e = mapExpM extractCalls e

makeDec ∷ CIdent → Type → Exp → Stm
makeDec cid typ e = SDecl (Dec [QType typ]
                         (DecAss [cid] tkass e))
 where
  tkass = TkAss ((0,0), "=")
