{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Desugar.Extract  where

import Control.Applicative
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
expand (SDecl (Dec qs (DecAss cids tk e)):ss) = do
  (e', sdecs) ← case e of
    (EPartCall cid es ts) → do
      exs ← mapM expandExp es
      let (es', sds) = foldr (\(x,ds) (px,pds) → (x:px,ds++pds)) ([],[]) exs
      return (EPartCall cid es' ts, sds)
    ex → expandExp ex
  ss' ← expand ss
  return $ sdecs ++ [SDecl (Dec qs (DecAss cids tk e'))] ++ ss'
expand (SExp e:ss) = do
  (e', sdecs) ← expandExp e
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
expand (SFor tkf fds els ers stm:ss) = do
  fds' ← mapM extractForDecl fds
  els' ← mapM extractCalls els
  ers' ← mapM extractCalls ers
  sdecs ← getPendings
  stm' ← liftM makeBlock $ expand [stm]
  ss' ← expand ss
  return $ sdecs ++ [SFor tkf fds' els' ers' stm'] ++ ss'
expand (SIf tk e stm:ss) = do
  (e', sdecs) ← expandExp e
  stm' ← liftM makeBlock $ expand [stm]
  ss' ← expand ss
  return $ sdecs ++ [SIf tk e' stm'] ++ ss'
expand (SIfElse tki e strue tke sfalse:ss) = do
  (e', sdecs) ← expandExp e
  strue' ← liftM makeBlock $ expand [strue]
  sfalse' ← liftM makeBlock $ expand [sfalse]
  ss' ← expand ss
  return $ sdecs ++ [SIfElse tki e' strue' tke sfalse'] ++ ss'
expand (SReturn tkr e:ss) = do
  (e',sdecs) ← expandExp e
  ss' ← expand ss
  return $ sdecs ++ [SReturn tkr e'] ++ ss'
expand ss = expandStmM expand ss

expandExp ∷ Exp → State Ids (Exp,[Stm])
expandExp e = (,) <$> extractCalls e <*> getPendings

extractForDecl ∷ ForDecl → State Ids ForDecl
extractForDecl (FDecl (Dec qs (DecAss cds tk e))) =
  FDecl <$> Dec qs <$> DecAss cds tk <$> extractCalls e
extractForDecl (FExp e) = FExp <$> extractCalls e
-- DecFun is not allowed here - no need to handle it.
-- TODO: Structs.
extractForDecl fd = return fd

extractCalls ∷ Exp → State Ids Exp
extractCalls (EAss el tk (EPartCall cid es ts)) = do
  es' ← mapM extractCalls es
  return $ EAss el tk (EPartCall cid es' ts)
extractCalls ecall@(ECall cid es) = do
  es' ← mapM extractCalls es
  var@(EVar varCid) ← newVar
  fs ← gets funs
  case Map.lookup (cIdentToString cid) fs of
    Nothing  → return ecall
    Just fun → if retType fun /= TVoid
      then putDec (makeDec varCid (retType fun) (ECall cid es')) >> return var
      else return ecall
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
