{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Desugar.Uncurry where

import Prelude hiding (mapM)

import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.State hiding (mapM)

import Data.Traversable

import Text.Printf

import Compiler.Utils
import TypeChecker.Types
import TypeChecker.Utils

import FrontEnd.AbsGrammar

tkass ∷ TkAss
tkass = TkAss ((-1,-1),"=")

tkfor ∷ TkFor
tkfor = TkFor ((-1,-1),"for")

uncurrySource ∷ Source → Source
uncurrySource src = src { functions = funs' }
 where
  funs' = evalState (mapM uncurryFun (functions src)) (Ids [1..] [])

uncurryFun ∷ Function → State Ids Function
uncurryFun f = do
  ss' ← uncurryStms (statements f)
  return $ f { statements = ss'}

data Ids = Ids {
  unused ∷ [Int],
  pending ∷ [String]
}

newId ∷ State Ids String
newId = do
  new ← gets (printf "_c%03d" . head . unused)
  modify (\st → st { unused = tail (unused st), pending = pending st ++ [new]})
  return new

popId ∷ State Ids String
popId = do
  ret ← gets (head . pending)
  modify (\st → st { pending = tail (pending st)})
  return ret

uncurryStms ∷ [Stm] → State Ids [Stm]
uncurryStms ((dec@(SDecl (Dec qs (DecAss cids tk e)))):ss)
  | hasCurryCall e = (++) <$> decStm e <*>
    ((:) <$> (SDecl <$> (Dec qs <$> (DecAss cids tk <$> uncurryExp e)))
         <*> uncurryStms ss)
  | otherwise = (:) <$> pure dec <*> uncurryStms ss
uncurryStms (SExp e:ss)
  | hasCurryCall e = (++) <$> decStm e <*>
    ((:) <$> (SExp <$> uncurryExp e) <*> uncurryStms ss)
  | otherwise = (:) <$> pure (SExp e) <*> uncurryStms ss
uncurryStms (SBlock sbs:ss) = (:) <$> (SBlock <$> uncurryStms sbs) <*>
    uncurryStms ss
uncurryStms (SReturn tk e:ss)
  | hasCurryCall e = (++) <$> decStm e <*>
    ((:) <$> (SReturn tk <$> uncurryExp e) <*> uncurryStms ss)
  | otherwise = (:) <$> pure (SReturn tk e) <*> uncurryStms ss
uncurryStms (SWhile tk e s:ss)
  | hasCurryCall e = do
    decs ← decFDecl e
    e' ← uncurryExp e
    ss' ← uncurryStms ss
    return $ SFor tkfor decs [e']
      (map (\(FDecl (Dec _ (DecAss (cid:_) _ ex))) → EAss (EVar cid) tkass ex) decs ) s : ss'
  | otherwise = (:) <$> pure (SWhile tk e s) <*> uncurryStms ss
uncurryStms (SIf tk e s:ss) | hasCurryCall e = do
  decs ← decStm e
  e' ← uncurryExp e
  s' ← uncurryStms [s]
  ss' ← uncurryStms ss
  if length s' == 1
    then return $ decs ++ (SIf tk e' (head s'):ss')
    else return $ decs ++ (SIf tk e' (SBlock s'):ss')
  | otherwise = do
  s' ← uncurryStms [s]
  ss' ← uncurryStms ss
  if length s' == 1
    then return $ SIf tk e (head s'):ss'
    else return $ SIf tk e (SBlock s'):ss'
uncurryStms (SIfElse tkif econd strue tkelse sfalse:ss)
  | hasCurryCall econd = (++) <$> decStm econd <*> (uncurryExp econd >>= cont)
  | otherwise = cont econd
 where
  cont ∷ Exp → State Ids [Stm]
  cont cond = do
    strue' ← uncurryStms [strue]
    sfalse' ← uncurryStms [sfalse]
    ss' ← uncurryStms ss
    let strue'' = if length strue' == 1
                    then head strue'
                    else SBlock strue'
    let sfalse'' = if length sfalse' == 1
                    then head sfalse'
                    else SBlock sfalse'
    return $ SIfElse tkif cond strue'' tkelse sfalse'':ss'
uncurryStms (SType t s:ss) = do
  _s' ← uncurryStms [s]
  let (s',s'') = case _s' of
                    (a:[]) → (a,[])
                    (a:as) → (last as, a:init as)
  ss' ← uncurryStms ss
  return $ s'' ++ [SType t s'] ++ ss'
uncurryStms (s:ss) = (:) <$> pure s <*> uncurryStms ss
uncurryStms [] = return []

decStm ∷ Exp → State Ids [Stm]
decStm = flip toDecs SDecl

decFDecl ∷ Exp → State Ids [ForDecl]
decFDecl = flip toDecs FDecl

toDecs ∷ Exp → (Decl → a) → State Ids [a]
toDecs e f =
  -- Folding instead of mapping to get same order of evaluation as in curryCalls
  foldM (\p e' → liftM ((p++) . return) (cCallsToDec e' f)) [] (curryCalls e)


uncurryExp ∷ Exp → State Ids Exp
uncurryExp e@(ECurryCall cid _ t)
  | isVec t = do
    ident' ← popId
    return $ ECall cid [ EMember (var ident') (memb [m])
      | (_, m) ← zip [1..(vecLength t)] "xyzw" ]
  | otherwise = error  $ "Non-vector given as curried call: " ++ show e
 where
  var s = EVar (CIdent ((-1,-1), s))
  memb s = CIdent ((-1,-1), s)
uncurryExp e = mapExpM uncurryExp e

cCallsToDec ∷ Exp → (Decl → a) → State Ids a
cCallsToDec (ECurryCall _ e t) con | isVec t = do
  new ← newId
  return $ con $ buildDecl t e new
cCallsToDec e _ = error $ show e ++ " is not a curried call"

buildDecl ∷ Type → Exp → String → Decl
buildDecl t e s = Dec [QType t] (DecAss [mkCId s] (TkAss ((-1,-1),"=")) e)

mkCId ∷ String → CIdent
mkCId s = CIdent ((-1,-1), s)

curryCalls ∷ Exp → [Exp]
curryCalls = foldExp (\p e → p `mplus` [ e | isCurryCall e ]) []

hasCurryCall ∷ Exp → Bool
hasCurryCall = not . null . curryCalls

isCurryCall ∷ Exp → Bool
isCurryCall (ECurryCall {}) = True
isCurryCall _ = False
