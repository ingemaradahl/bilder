{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Utils where

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
