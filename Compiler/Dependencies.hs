{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Dependencies where

import FrontEnd.AbsGrammar

import TypeChecker.Utils

data Dep =
    Var String
  | Fun String
  | None
 deriving (Ord,Eq,Show)

type DepList = [(Dep, [Dep])]

-- | Returns a list of all affected variables and their dependencies.
stmDeps ∷ Stm → DepList
stmDeps (SDecl decl) = declDeps decl
stmDeps (SExp e) = expDeps e
stmDeps (SBlock ss) = concatMap stmDeps ss
stmDeps (SWhile _ e s) = expDeps e ++ stmDeps s
stmDeps (SDoWhile _ s _ e) = stmDeps s ++ expDeps e
stmDeps (SFor _ fds cones loopes s) =
  concatMap forDeclDeps fds ++ concatMap expDeps (cones ++ loopes) ++ stmDeps s
stmDeps (SReturn _ e) = expDeps e
stmDeps (SVoidReturn _) = []
stmDeps (SIf _ e s) = expDeps e ++ stmDeps s
stmDeps (SIfElse _ e st _ sf) = expDeps e ++ stmDeps st ++ stmDeps sf
stmDeps (SBreak _) = []
stmDeps (SContinue _) = []
stmDeps (SDiscard _) = []
stmDeps (SType _ s) = stmDeps s
stmDeps (SFunDecl {}) = error "SFunDecl really not allowed here."

forDeclDeps ∷ ForDecl → DepList
forDeclDeps (FDecl d) = declDeps d
forDeclDeps (FExp e) = expDeps e

declDeps ∷ Decl → DepList
declDeps (Dec _ dp) = declPostDeps dp
declDeps d = error $ "not implemented (structs :/): " ++ show d

declPostDeps ∷ DeclPost → DepList
declPostDeps (Vars cids) = [(Var n,[]) | n ← map cIdentToString cids]
declPostDeps (DecAss cids _ e) =
  [(Var n, concatMap snd (expDeps e)) | n ← map cIdentToString cids] ++ onlyAssDeps (expDeps e)
declPostDeps (DecFun {}) = error "inner function declarations not allowed."

expDeps ∷ Exp → DepList
expDeps (EAss el _ er) = expAss el er
expDeps (EAssAdd el _ er) = expAss el er
expDeps (EAssSub el _ er) = expAss el er
expDeps (EAssMul el _ er) = expAss el er
expDeps (EAssDiv el _ er) = expAss el er
expDeps (EAssMod el _ er) = expAss el er
expDeps (EAssBWAnd el _ er) = expAss el er
expDeps (EAssBWXOR el _ er) = expAss el er
expDeps (EAssBWOR el _ er) = expAss el er
expDeps (ECond ec _ et _ ef) = concatMap expDeps [ec,et,ef]
expDeps (EOR el _ er) = concatMap expDeps [el,er]
expDeps (EXOR el _ er) = concatMap expDeps [el,er]
expDeps (EAnd el _ er) = concatMap expDeps [el,er]
expDeps (EBWOR el _ er) = concatMap expDeps [el,er]
expDeps (EBWXOR el _ er) = concatMap expDeps [el,er]
expDeps (EBWAnd el _ er) = concatMap expDeps [el,er]
expDeps (EEqual el _ er) = concatMap expDeps [el,er]
expDeps (ENEqual el _ er) = concatMap expDeps [el,er]
expDeps (ELt el _ er) = concatMap expDeps [el,er]
expDeps (EGt el _ er) = concatMap expDeps [el,er]
expDeps (ELEt el _ er) = concatMap expDeps [el,er]
expDeps (EGEt el _ er) = concatMap expDeps [el,er]
expDeps (EBWShiftLeft el _ er) = concatMap expDeps [el,er]
expDeps (EBWShiftRight el _ er) = concatMap expDeps [el,er]
expDeps (EAdd el _ er) = concatMap expDeps [el,er]
expDeps (ESub el _ er) = concatMap expDeps [el,er]
expDeps (EMul el _ er) = concatMap expDeps [el,er]
expDeps (EDiv el _ er) = concatMap expDeps [el,er]
expDeps (EMod el _ er) = concatMap expDeps [el,er]
expDeps (ENeg _ e) = expDeps e
expDeps (ENegSign _ e) = expDeps e
expDeps (EComplement _ e) = expDeps e
expDeps (EPos _ e) = expDeps e
expDeps (EPreInc _ e) = expDeps e
expDeps (EPreDec _ e) = expDeps e
expDeps (EPostInc e _) = expDeps e
expDeps (EPostDec e _) = expDeps e
-- Only non "real" members (e.g. xyz).
expDeps (EMember e _) = (None, concatMap snd e') : e'
 where
  e' = expDeps e
-- Only built in member calls (e.g. map).
expDeps (EMemberCall e _ es) =
  (None, concatMap snd es') : es'
 where
  es' = concatMap expDeps (e:es)
expDeps (ECall cid es) = (None, Var (cIdentToString cid) : Fun (cIdentToString cid) : concatMap (concatMap snd) es') : concatMap onlyAssDeps es'
 where
  es' = map expDeps es
expDeps (ETypeCall _ es) = concatMap expDeps es
expDeps (EVar cid) = [(None, [Var (cIdentToString cid)])]
expDeps (EIndex cid e) = (None, Var (cIdentToString cid) : concatMap snd e') : e'
 where
  e' = expDeps e
expDeps (EFloat {}) = []
expDeps (EInt _) = []
expDeps (ETrue) = []
expDeps (EFalse) = []
expDeps (EPartCall cid es _) =
  (None, Var (cIdentToString cid):Fun (cIdentToString cid):concatMap snd es') : es'
 where
  es' = concatMap expDeps es
expDeps (ECurryCall {}) = error "ECurryCall should be uncurried here."
expDeps (EVarType cid _) = [(None, [Var (cIdentToString cid)])]

expAss ∷ Exp → Exp → DepList
expAss (EVar cid) e =
  (Var (cIdentToString cid), concatMap snd (expDeps e)) : onlyAssDeps (expDeps e)
expAss (EIndex cid e) el =
  (Var (cIdentToString cid), concatMap snd (el'++e')) : onlyAssDeps el' ++ e'
 where
  e' = expDeps e
  el' = expDeps el
expAss el er =
  -- el depends on all er's dependencies
  map (\(a,c) → (a,c++desper)) el'
  -- and the whole assignment depends on all dependencies in er'
  ++ er'
 where
  el' = expDeps el
  er' = expDeps er
  desper = concatMap snd er'

onlyAssDeps ∷ DepList → DepList
onlyAssDeps = filter ((/=) None. fst)
