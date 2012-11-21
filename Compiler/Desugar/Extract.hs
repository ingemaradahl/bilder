{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Desugar.Extract  where

import Control.Monad.State

import qualified Data.Traversable as Map (mapM)

import Compiler.Utils

import TypeChecker.Types
import FrontEnd.AbsGrammar

data Ids = Ids {
  unused ∷ [Int],
  pending ∷ [Stm]
}

newVar ∷ State Ids Exp
newVar = do
  e ← gets (("_l" ++) . show . head . unused)
  modify (\st → st { unused = tail (unused st) })
  return $ EVar (CIdent ((0,0), e))

putDec ∷ Stm → State Ids ()
putDec stm = modify (\st → st { pending = pending st ++ [stm] })


expandSource ∷ Source → Source
expandSource src = src { functions = evalState (Map.mapM expandFun (functions src)) (Ids [1..] [])}

expandFun ∷ Function → State Ids Function
expandFun fun = do
  stms ← expand (statements fun)
  return $ fun { statements = stms}

expand ∷ [Stm] → State Ids [Stm]
expand (SExp e:ss) = do
  e' ← extractCalls e
  sdecs ← gets pending
  ss' ← expand ss
  return $ sdecs ++ [SExp e'] ++ ss'
expand (SWhile tkw e stm:ss) = do
  e' ← extractCalls e
  sdecs ← gets pending
  stm' ← liftM makeBlock $ expand [stm]
  ss' ← expand ss
  return $ sdecs ++ [SWhile tkw e' stm'] ++ ss'
expand (SDoWhile tkd stm tkw e:ss) = do
  stms ← expand [stm]
  e' ← extractCalls e
  sdecs ← gets pending
  let stm' = makeBlock $ stms ++ sdecs
  ss' ← expand ss
  return $ SDoWhile tkd stm' tkw e':ss'
--expand SFor ...
--expand SIf|SIfElse..
expand (SReturn tkr e:ss) = do
  e' ← extractCalls e
  sdecs ← gets pending
  ss' ← expand ss
  return $ sdecs ++ [SReturn tkr e'] ++ ss'
expand ss = expandStmM expand ss

extractCalls ∷ Exp → State Ids Exp
extractCalls (EPartCall cid es ts) = do
  es' ← mapM extractCalls es
  var@(EVar varCid) ← newVar
  putDec $ makeDec varCid (EPartCall cid es' ts)
  return var
extractCalls e = mapExpM extractCalls e

makeDec ∷ CIdent → Exp → Stm
makeDec cid e = SDecl (Dec [QType imgtype]
                         (DecAss [cid] tkass e))
 where
  imgtype = TFun TVec4 [TFloat, TFloat]
  tkass = TkAss ((0,0), "=")
