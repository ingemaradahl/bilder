{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Utils where

import Control.Applicative
import Control.Monad.Identity

import FrontEnd.AbsGrammar

import TypeChecker.Utils (cIdentToString, paramToString)


-- | Finds all declared (inner) functions.
findInnerFuns ∷ [Stm] → [(String, [Param], [Stm])]
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

-- Free Variables {{{
-- | Find all free variables in a function.
freeFunctionVars ∷ (String, [Param], [Stm]) → [String]
freeFunctionVars (_, ps, stms) = snd $ foldl stmVars (bound, []) stms
 where
  bound = map paramToString ps -- TODO: Add global variables.

-- | Calculates a statements bound and free variables
--   (given already known bound and free variables)
stmVars ∷ ([String], [String]) → Stm → ([String], [String])
stmVars (b, f) (SDecl d) = (declToName d ++ b, f)
stmVars (b, f) (SExp e) = (b, f ++ filterBound b (expVars e))
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
expVars (EAss el _ er) = concatMap expVars [el, er]
expVars (EAssAdd el _ er) = concatMap expVars [el, er]
expVars (EAssSub el _ er) = concatMap expVars [el, er]
expVars (EAssMul el _ er) = concatMap expVars [el, er]
expVars (EAssDiv el _ er) = concatMap expVars [el, er]
expVars (EAssMod el _ er) = concatMap expVars [el, er]
expVars (EAssBWAnd el _ er) = concatMap expVars [el, er]
expVars (EAssBWXOR el _ er) = concatMap expVars [el, er]
expVars (EAssBWOR el _ er) = concatMap expVars [el, er]
expVars (ECond ec _ et _ ef) = concatMap expVars [ec, et, ef]
expVars (EOR el _ er) = concatMap expVars [el, er]
expVars (EXOR el _ er) = concatMap expVars [el, er]
expVars (EAnd el _ er) = concatMap expVars [el, er]
expVars (EBWOR el _ er) = concatMap expVars [el, er]
expVars (EBWXOR el _ er) = concatMap expVars [el, er]
expVars (EBWAnd el _ er) = concatMap expVars [el, er]
expVars (EEqual el _ er) = concatMap expVars [el, er]
expVars (ENEqual el _ er) = concatMap expVars [el, er]
expVars (ELt el _ er) = concatMap expVars [el, er]
expVars (EGt el _ er) = concatMap expVars [el, er]
expVars (ELEt el _ er) = concatMap expVars [el, er]
expVars (EGEt el _ er) = concatMap expVars [el, er]
expVars (EBWShiftLeft el _ er) = concatMap expVars [el, er]
expVars (EBWShiftRight el _ er) = concatMap expVars [el, er]
expVars (EAdd el _ er) = concatMap expVars [el, er]
expVars (ESub el _ er) = concatMap expVars [el, er]
expVars (EMul el _ er) = concatMap expVars [el, er]
expVars (EDiv el _ er) = concatMap expVars [el, er]
expVars (EMod el _ er) = concatMap expVars [el, er]
expVars (ENeg _ e) = expVars e
expVars (ENegSign _ e) = expVars e
expVars (EComplement _ e) = expVars e
expVars (EPos _ e) = expVars e
expVars (EPreInc _ e) = expVars e
expVars (EPreDec _ e) = expVars e
expVars (EPostInc e _) = expVars e
expVars (EPostDec e _) = expVars e
expVars (EMember e _) = expVars e
expVars (EMemberCall e _ es) = expVars e ++ concatMap expVars es
expVars (ECall _ es) = concatMap expVars es
expVars (ETypeCall _ es) = concatMap expVars es
expVars (EVar cid) = [cIdentToString cid]
expVars (EIndex cid e) = cIdentToString cid : expVars e
expVars (EFloat {}) = []
expVars (EInt {}) = []
expVars (ETrue) = []
expVars (EFalse) = []
-- }}}

mapExpM ∷ (Monad m, Applicative m) => (Exp → m Exp) → Exp → m Exp
mapExpM f (EAss el tk er) = EAss <$> f el <*> pure tk <*> f er
mapExpM f (EAssAdd el tk er) = EAssAdd <$> f el <*> pure tk <*> f er
mapExpM f (EAssSub el tk er) = EAssSub <$> f el <*> pure tk <*> f er
mapExpM f (EAssMul el tk er) = EAssMul <$> f el <*> pure tk <*> f er
mapExpM f (EAssDiv el tk er) = EAssDiv <$> f el <*> pure tk <*> f er
mapExpM f (EAssMod el tk er) = EAssMod <$> f el <*> pure tk <*> f er
mapExpM f (EAssBWAnd el tk er) = EAssBWAnd <$> f el <*> pure tk <*> f er
mapExpM f (EAssBWXOR el tk er) = EAssBWXOR <$> f el <*> pure tk <*> f er
mapExpM f (EAssBWOR el tk er) = EAssBWOR <$> f el <*> pure tk <*> f er
mapExpM f (EOR el tk er) = EOR <$> f el <*> pure tk <*> f er
mapExpM f (EXOR el tk er) = EXOR <$> f el <*> pure tk <*> f er
mapExpM f (EAnd el tk er) = EAnd <$> f el <*> pure tk <*> f er
mapExpM f (EBWOR el tk er) = EBWOR <$> f el <*> pure tk <*> f er
mapExpM f (EBWXOR el tk er) = EBWXOR <$> f el <*> pure tk <*> f er
mapExpM f (EBWAnd el tk er) = EBWAnd <$> f el <*> pure tk <*> f er
mapExpM f (EEqual el tk er) = EEqual <$> f el <*> pure tk <*> f er
mapExpM f (ENEqual el tk er) = ENEqual <$> f el <*> pure tk <*> f er
mapExpM f (ELt el tk er) = ELt <$> f el <*> pure tk <*> f er
mapExpM f (EGt el tk er) = EGt <$> f el <*> pure tk <*> f er
mapExpM f (ELEt el tk er) = ELEt <$> f el <*> pure tk <*> f er
mapExpM f (EGEt el tk er) = EGEt <$> f el <*> pure tk <*> f er
mapExpM f (EBWShiftLeft el tk er) = EBWShiftLeft <$> f el <*> pure tk <*> f er
mapExpM f (EBWShiftRight el tk er) = EBWShiftRight <$> f el <*> pure tk <*> f er
mapExpM f (EAdd el tk er) = EAdd <$> f el <*> pure tk <*> f er
mapExpM f (ESub el tk er) = ESub <$> f el <*> pure tk <*> f er
mapExpM f (EMul el tk er) = EMul <$> f el <*> pure tk <*> f er
mapExpM f (EDiv el tk er) = EDiv <$> f el <*> pure tk <*> f er
mapExpM f (EMod el tk er) = EMod <$> f el <*> pure tk <*> f er
mapExpM f (ECond eq tkl el tkr er) = ECond <$> f eq <*> pure tkl <*> f el <*> pure tkr <*> f er
mapExpM f (ENeg tk e) = ENeg tk <$> f e
mapExpM f (ENegSign tk e) = ENegSign tk <$> f e
mapExpM f (EComplement tk e) = EComplement tk <$> f e
mapExpM f (EPos tk e) = EPos tk <$> f e
mapExpM f (EPreInc tk e) = EPreInc tk <$> f e
mapExpM f (EPreDec tk e) = EPreDec tk <$> f e
mapExpM f (EPostInc e tk) = EPostInc <$> f e <*> pure tk
mapExpM f (EPostDec e tk) = EPostDec <$> f e <*> pure tk
mapExpM f (EMember e cid) = EMember <$> f e <*> pure cid
mapExpM f (EMemberCall el cid es) = EMemberCall <$> f el <*> pure cid <*> mapM f es
mapExpM f (ECall cid es) = ECall cid <$> mapM f es
mapExpM f (ETypeCall t es) = ETypeCall t <$> mapM f es
mapExpM f (EIndex cid e) = EIndex cid <$> f e
mapExpM _ (EVar cid) = pure $ EVar cid
mapExpM _ e@(EFloat {}) = pure e
mapExpM _ e@(EInt {}) = pure e
mapExpM _ e@ETrue = pure e
mapExpM _ e@EFalse = pure e

mapExp ∷ (Exp → Exp) → Exp → Exp
mapExp f ex = runIdentity $ mapExpM f' ex
 where
  f' ∷ Exp → Identity Exp
  f' e = return $ f e

mapStmM ∷ (Monad m, Applicative m) => (Stm → m Stm) → Stm → m Stm
mapStmM _ s@(SDecl {}) = pure s
mapStmM _ s@(SExp {}) = pure s
mapStmM f (SBlock ss) = SBlock <$> mapM f ss
mapStmM f (SWhile tk e s) = SWhile tk e <$> f s
mapStmM f (SDoWhile tkl s tkr e) = SDoWhile tkl <$> f s <*> pure tkr <*> pure e
mapStmM f (SFor tk dec el er s) = SFor tk dec el er <$> f s
mapStmM _ s@(SReturn {}) = pure s
mapStmM _ s@(SVoidReturn {}) = pure s
mapStmM f (SIf tk e s) = SIf tk e <$> f s
mapStmM f (SIfElse tk e st tke se) = SIfElse tk e <$> f st <*> pure tke <*> f se
mapStmM _ s@(SBreak {}) = pure s
mapStmM _ s@(SContinue {}) = pure s
mapStmM _ s@(SDiscard {}) = pure s
mapStmM f (SType t s) = SType t <$> f s
mapStmM f (SFunDecl cid t ps ss) = SFunDecl cid t ps <$> mapM f ss

mapStm ∷ (Stm → Stm) → Stm → Stm
mapStm f s = runIdentity $ mapStmM f' s
 where
  f' ∷ Stm → Identity Stm
  f' stm = return $ f stm

-- vi:fdm=marker
