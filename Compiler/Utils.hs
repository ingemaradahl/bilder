{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Utils where

import FrontEnd.AbsGrammar

import TypeChecker.Utils (cIdentToString, paramToString)

import Control.Monad.Identity

-- | Finds all declared (inner) functions.
findInnerFuns ∷ [Stm] → [AbsFun]
findInnerFuns [] = []
findInnerFuns (SFunDecl cid _ ps stms:rest) =
  (cIdentToString cid, ps, stms) : findInnerFuns rest ++ findInnerFuns stms
findInnerFuns (_:rest) = findInnerFuns rest

-- | Extracts a declared variables name.
declToName ∷ Decl → [String]
declToName (Dec _ dp) = declPostToName dp
declToName (Struct {}) = []
declToName (StructDecl _ _ _ cid) = [cIdentToString cid]

declPostToName ∷ DeclPost → [String]
declPostToName (Vars cids) = map cIdentToString cids
declPostToName (DecAss cids _ _) = map cIdentToString cids
-- DecFun is replaced with SFunDecl in the typechecker.
--declPostToName (DecFun {}) = []

-- Exp and Stm helpers {{{
foldExpM ∷ Monad m => (a → Exp → m a) → a → Exp → m a
foldExpM f p e@(EAss el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EAssAdd el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EAssSub el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EAssMul el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EAssDiv el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EAssMod el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EAssBWAnd el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EAssBWXOR el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EAssBWOR el _ er) = foldM f p [el,er,e]
foldExpM f p e@(ECond econd _ etrue _ efalse) = foldM f p [econd,etrue,efalse,e]
foldExpM f p e@(EOR el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EXOR el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EAnd el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EBWOR el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EBWXOR el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EBWAnd el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EEqual el _ er) = foldM f p [el,er,e]
foldExpM f p e@(ENEqual el _ er) = foldM f p [el,er,e]
foldExpM f p e@(ELt el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EGt el _ er) = foldM f p [el,er,e]
foldExpM f p e@(ELEt el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EGEt el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EBWShiftLeft el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EBWShiftRight el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EAdd el _ er) = foldM f p [el,er,e]
foldExpM f p e@(ESub el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EMul el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EDiv el _ er) = foldM f p [el,er,e]
foldExpM f p e@(EMod el _ er) = foldM f p [el,er,e]
foldExpM f p e@(ENeg _ ei) = foldM f p [ei,e]
foldExpM f p e@(ENegSign _ ei) = foldM f p [ei,e]
foldExpM f p e@(EComplement _ ei) = foldM f p [ei,e]
foldExpM f p e@(EPos _ ei) = foldM f p [ei,e]
foldExpM f p e@(EPreInc _ ei) = foldM f p [ei,e]
foldExpM f p e@(EPreDec _ ei) = foldM f p [ei,e]
foldExpM f p e@(EPostInc ei _) = foldM f p [ei,e]
foldExpM f p e@(EPostDec ei _) = foldM f p [ei,e]
foldExpM f p e@(EMember ei _) = foldM f p [ei,e]
foldExpM f p e@(EMemberCall ei _ es) = foldM f p $ [ei,e] ++ es
foldExpM f p e@(ECall _ es) = foldM f p (e:es)
foldExpM f p e@(ETypeCall _ es) = foldM f p (e:es)
foldExpM f p e@(EVar {}) = f p e
foldExpM f p e@(EIndex _ ei) = foldM f p [ei,e]
foldExpM f p e@(EFloat {}) = f p e
foldExpM f p e@(EInt {}) = f p e
foldExpM f p e@ETrue = f p e
foldExpM f p e@EFalse = f p e

foldExp ∷ (a → Exp → a) → a → Exp → a
foldExp f p e = runIdentity (foldExpM (liftIdentity f) p e)

foldStmM ∷ Monad m => (a → Stm → m a) → a → Stm → m a
foldStmM f p s@(SDecl {}) = f p s
foldStmM f p s@(SExp {}) = f p s
foldStmM f p s@(SBlock iss) = foldM f p (s:iss)
foldStmM f p s@(SWhile _ _ is) = foldM f p [is,s]
foldStmM f p s@(SDoWhile _ is _ _) = foldM f p [is,s]
foldStmM f p s@(SFor _ _ _ _ is) = foldM f p [is,s]
foldStmM f p s@(SReturn {}) = f p s
foldStmM f p s@(SVoidReturn {}) = f p s
foldStmM f p s@(SIf _ _ is) = foldM f p [is,s]
foldStmM f p s@(SIfElse _ _ istrue _ isfalse) = foldM f p [istrue,isfalse,s]
foldStmM f p s@(SBreak {}) = f p s
foldStmM f p s@(SContinue {}) = f p s
foldStmM f p s@(SDiscard {}) = f p s
foldStmM f p s@(SType _ is) = foldM f p [is,s]
foldStmM f p s@(SFunDecl _ _ _ iss) = foldM f p (s:iss)

foldStm ∷ (a → Stm → a) → a → Stm → a
foldStm f p s = runIdentity (foldStmM (liftIdentity f) p s)

liftIdentity ∷ (a → b → a) → a → b → Identity a
liftIdentity f' p' s' = Identity (f' p' s')
-- }}}
-- Free Variables {{{
type AbsFun = (String, [Param], [Stm])

-- | Find all free variables in a function.
freeFunctionVars ∷ [String] → AbsFun → [String]
freeFunctionVars global (_, ps, stms) = snd $ foldl stmVars (bound, []) stms
 where
  bound = global ++ map paramToString ps

-- | Calculates a statements bound and free variables
--   (given already known bound and free variables)
stmVars ∷ ([String], [String]) → Stm → ([String], [String])
stmVars (b, f) (SDecl d) = (declToName d ++ b, f)
stmVars (b, f) (SExp e) = (b, f ++ filterBound b (expVars e))
stmVars vs (SBlock stms) = foldl stmVars vs stms
stmVars vs (SWhile _ e s) = (b, f ++ filterBound b (expVars e))
 where
  (b, f) = stmVars vs s -- Order does not matter because of unique names
stmVars (b, f) (SReturn _ e) = (b, f ++ filterBound b (expVars e))
stmVars vs (SVoidReturn _) = vs
stmVars (b, f) (SIf _ e stm) = (b'', f ++ f' ++ filterBound b'' (expVars e))
 where
  (b', f') = stmVars (b, f) stm
  b'' = b ++ b'
stmVars (b, f) (SIfElse _ econd strue _ sfalse) =
  (b', f ++ ftrue ++ ffalse ++ filterBound b' (expVars econd))
 where
  (btrue, ftrue) = stmVars (b, f) strue
  (bfalse, ffalse) = stmVars (b, f) sfalse
  b' = b ++ btrue ++ bfalse -- This is OK because of unique names.
stmVars vs (SType _ stm) = stmVars vs stm
stmVars vs (SFunDecl {}) = vs
stmVars _ stm = error $ "UNHANDLED " ++ show stm

-- | Known bound variables → Variables → Free variables
filterBound ∷ [String] → [String] → [String]
filterBound bound = filter (not . (`elem` bound))

-- | Find all referenced variables in an expression.
expVars ∷ Exp → [String]
expVars = foldExp expVar []
 where
  expVar ∷ [String] → Exp → [String]
  expVar p (EVar cid) = cIdentToString cid : p
  expVar p (EIndex cid _) = cIdentToString cid : p
  expVar p _ = p
-- }}}
