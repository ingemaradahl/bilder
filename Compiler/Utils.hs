{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Utils where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import CompilerError

import FrontEnd.AbsGrammar

import TypeChecker.Utils (cIdentToString, cIdentToPos, paramToString)
import qualified TypeChecker.Types as T

import Data.List (nub)


liftCError ∷ CError a → StateT b CError a
liftCError m = StateT (\s → case m of { Fail f → Fail f; Pass a → return (a,s) })

-- | Creates a Variable from a FilePath and a Param
paramToVar ∷ FilePath → Param → T.Variable
paramToVar f (ParamDec qs cid) =
  T.Variable (cIdentToString cid) (f, cIdentToPos cid) (qualsToType qs) Nothing
paramToVar f (ParamDefault qs cid _ e) =
  T.Variable (cIdentToString cid) (f, cIdentToPos cid) (qualsToType qs) (Just e)

varTypeToParam ∷ String → Type → Param
varTypeToParam n t = ParamDec [QType t] name
 where
  name = CIdent ((-1,-1),n)

-- | Extracts a declared variables name.
declToName ∷ Decl → [String]
declToName (Dec _ dp) = declPostToName dp
declToName (Struct {}) = []
declToName (StructDecl _ _ _ cid) = [cIdentToString cid]

declToTypeAndName ∷ Decl → [(Type, String)]
declToTypeAndName (Dec qs dp) = zip (repeat $ qualsToType qs) (declPostToName dp)
-- TODO: Structs.
--declToTypeAndName (Struct {}) = undefined
--declToTypeAndName (StructDecl {}) = undefined

-- | Qualifiers to type (typechecked so it should always be OK)
qualsToType ∷ [Qualifier] → Type
--qualsToType [] = undefined -- Should never happen (typechecked)
qualsToType (QType t:_) = t
qualsToType (_:qs) = qualsToType qs

declPostToName ∷ DeclPost → [String]
declPostToName (Vars cids) = map cIdentToString cids
declPostToName (DecAss cids _ _) = map cIdentToString cids
-- DecFun is replaced with SFunDecl in the typechecker.
--declPostToName (DecFun {}) = []

-- Exp and Stm folding and mapping {{{
foldExpM ∷ Monad m => (a → Exp → m a) → a → Exp → m a
foldExpM f p e@(EAss el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssAdd el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssSub el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssMul el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssDiv el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssMod el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssBWAnd el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssBWXOR el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssBWOR el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ECond econd _ etrue _ efalse) =
  foldM (foldExpM f) p [econd,etrue,efalse] >>= flip f e
foldExpM f p e@(EOR el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EXOR el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAnd el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EBWOR el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EBWXOR el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EBWAnd el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EEqual el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ENEqual el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ELt el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EGt el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ELEt el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EGEt el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EBWShiftLeft el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EBWShiftRight el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAdd el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ESub el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EMul el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EDiv el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EMod el _ er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ENeg _ ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(ENegSign _ ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EComplement _ ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EPos _ ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EPreInc _ ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EPreDec _ ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EPostInc ei _) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EPostDec ei _) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EMember ei _) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EMemberCall ei _ es) = foldM (foldExpM f) p (ei:es) >>= flip f e
foldExpM f p e@(ECall _ es) = foldM (foldExpM f) p es >>= flip f e
foldExpM f p e@(EPartCall _ es _) = foldM (foldExpM f) p es >>= flip f e
foldExpM f p e@(ECurryCall _ ei _) = foldExpM f p ei >>= flip f e
foldExpM f p e@(ETypeCall _ es) = foldM (foldExpM f) p es >>= flip f e
foldExpM f p e@(EVar {}) = f p e
foldExpM f p e@(EVarType {}) = f p e
foldExpM f p e@(EIndex _ ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EIndexDouble _ e1 e2) = foldM (foldExpM f) p [e1, e2] >>= flip f e
foldExpM f p e@(EFloat {}) = f p e
foldExpM f p e@(EInt {}) = f p e
foldExpM f p e@ETrue = f p e
foldExpM f p e@EFalse = f p e

foldExp ∷ (a → Exp → a) → a → Exp → a
foldExp f p e = runIdentity (foldExpM (liftIdentity f) p e)

foldStmM ∷ Monad m => (a → Stm → m a) → a → Stm → m a
foldStmM f p s@(SDecl {}) = f p s
foldStmM f p s@(SExp {}) = f p s
foldStmM f p s@(SBlock iss) = foldM (foldStmM f) p iss >>= flip f s
foldStmM f p s@(SWhile _ _ is) = foldStmM f p is >>= flip f s
foldStmM f p s@(SDoWhile _ is _ _) = foldStmM f p is >>= flip f s
foldStmM f p s@(SFor _ _ _ _ is) = foldStmM f p is >>= flip f s
foldStmM f p s@(SReturn {}) = f p s
foldStmM f p s@(SVoidReturn {}) = f p s
foldStmM f p s@(SIf _ _ is) = foldStmM f p is >>= flip f s
foldStmM f p s@(SIfElse _ _ istrue _ isfalse) =
  foldM (foldStmM f) p [istrue,isfalse] >>= flip f s
foldStmM f p s@(SBreak {}) = f p s
foldStmM f p s@(SContinue {}) = f p s
foldStmM f p s@(SDiscard {}) = f p s
foldStmM f p s@(SType _ is) = foldStmM f p is >>= flip f s
foldStmM f p s@(SFunDecl _ _ _ _ iss) = foldM (foldStmM f) p iss >>= flip f s

foldStm ∷ (a → Stm → a) → a → Stm → a
foldStm f p s = runIdentity (foldStmM (liftIdentity f) p s)

liftIdentity ∷ (a → b → a) → a → b → Identity a
liftIdentity f' p' s' = Identity (f' p' s')

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
mapExpM f (EPartCall cid es ts) = EPartCall cid <$> mapM f es <*> pure ts
mapExpM f (ECurryCall cid e t) = ECurryCall cid <$> f e <*> pure t
mapExpM f (ETypeCall t es) = ETypeCall t <$> mapM f es
mapExpM f (EIndex cid e) = EIndex cid <$> f e
mapExpM f (EIndexDouble cid e1 e2) = EIndexDouble cid <$> f e1 <*> f e2
mapExpM _ e@(EVar {}) = pure e
mapExpM _ e@(EFloat {}) = pure e
mapExpM _ e@(EInt {}) = pure e
mapExpM _ e@ETrue = pure e
mapExpM _ e@EFalse = pure e
mapExpM _ e@(EVarType {}) = pure e

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
mapStmM f (SFunDecl cid t px ps ss) = SFunDecl cid t px ps <$> mapM f ss

mapStm ∷ (Stm → Stm) → Stm → Stm
mapStm f s = runIdentity $ mapStmM f' s
 where
  f' ∷ Stm → Identity Stm
  f' stm = return $ f stm

-- | Mapping over all Exp in Stm.
mapStmExpM ∷ (Monad m, Applicative m) => (Exp → m Exp) → Stm → m Stm
mapStmExpM f (SDecl (Dec qs (DecAss cids tk e))) =
  SDecl <$> Dec qs <$> DecAss cids tk <$> f e
mapStmExpM f (SExp e) = SExp <$> f e
mapStmExpM f (SDoWhile tkd s tkw e) =
  SDoWhile tkd <$> mapStmExpM f s <*> pure tkw <*> f e
mapStmExpM f (SWhile tk e s) =
  SWhile tk <$> f e <*> mapStmExpM f s
mapStmExpM f (SFor tk fds ecs els s) =
  SFor tk <$> mapM (mapForDeclExpM f) fds <*> mapM f ecs <*> mapM f els <*> mapStmExpM f s
mapStmExpM f (SReturn tk e) = SReturn tk <$> f e
mapStmExpM f (SIf tk e s) = SIf tk <$> f e <*> mapStmExpM f s
mapStmExpM f (SIfElse tki e st tke stf) =
  SIfElse tki <$> f e <*> mapStmExpM f st <*> pure tke <*> mapStmExpM f stf
mapStmExpM f s = mapStmM (mapStmExpM f) s

mapForDeclExpM ∷ (Monad m, Applicative m) => (Exp → m Exp) → ForDecl → m ForDecl
mapForDeclExpM f (FDecl (Dec qs (DecAss cids tk e))) =
  liftM (FDecl . Dec qs . DecAss cids tk) (f e)
-- TODO: Structs!
mapForDeclExpM _ d@(FDecl _) = return d
mapForDeclExpM f (FExp e) = FExp <$> f e

mapStmExp ∷ (Exp → Exp) → Stm → Stm
mapStmExp f s = runIdentity $ mapStmExpM f' s
 where
  f' ∷ Exp → Identity Exp
  f' e = return $ f e

-- Expands statements, useful for rewriting code
expandStmM ∷ (Monad m, Applicative m) => ([Stm] → m [Stm]) → [Stm] → m [Stm]
expandStmM f (SBlock stms:ss) = (:) <$> (SBlock <$> f stms) <*> f ss
expandStmM f (SWhile tkw e s:ss) = do
  s' ← liftM makeBlock $ f [s]
  ss' ← f ss
  return $ SWhile tkw e s':ss'
expandStmM f (SDoWhile tkd s tkw e:ss) = do
  s' ← liftM makeBlock $ f [s]
  ss' ← f ss
  return $ SDoWhile tkd s' tkw e:ss'
expandStmM f (SFor tkf fdec es es' s:ss) =
  (:) <$> (SFor tkf fdec es es' <$> liftM makeBlock (f [s])) <*> f ss
expandStmM f (SIf tkif e s:ss) = (:)
  <$> (SIf tkif e <$> liftM makeBlock (f [s]))
  <*> f ss
expandStmM f (SIfElse tkif e st tke sf:ss) = do
  st' ← liftM makeBlock (f [st])
  sf' ← liftM makeBlock (f [sf])
  ss' ← f ss
  return $ SIfElse tkif e st' tke sf':ss'
expandStmM f (SType t s:ss) = (++) <$> liftM (map (SType t)) (f [s]) <*> f ss
expandStmM f (s:ss) = (:) <$> pure s <*> expandStmM f ss
expandStmM _ [] = return []

makeBlock ∷ [Stm] → Stm
makeBlock ss = if length ss == 1 then head ss else SBlock ss

expandStm ∷ ([Stm] → [Stm]) → [Stm] → [Stm]
expandStm f s = runIdentity $ expandStmM f' s
 where
  f' ∷ [Stm] → Identity [Stm]
  f' s' = return $ f s'

-- }}}
-- Free Variables {{{
-- TODO: Move this to Compiler/Types.hs
type AbsFun = (String, [Param], [Stm])

-- | Find all free variables in a function.
freeFunctionVars ∷ [String] → AbsFun → [String]
freeFunctionVars global (_, ps, stms) = nub . snd $ foldl stmVars (bound, []) stms
 where
  bound = global ++ map paramToString ps

-- | Calculates a statements bound and free variables
--   (given already known bound and free variables)
stmVars ∷ ([String], [String]) → Stm → ([String], [String])
stmVars (b, f) s@(SDecl d) = (declToName d ++ b, f ++ filterBound b (usedVars' [s]))
stmVars (b, f) (SExp e) = (b, f ++ filterBound b (expVars e))
stmVars vs (SBlock stms) = foldl stmVars vs stms
stmVars vs (SFor _ fd el er s) = (b' ++ b, filterBound (b' ++ b) (f'' ++ f' ++ f))
 where
  b' = concatMap declToName [ d | FDecl d ← fd ]
  f'' = concatMap expVars [ e | FExp e ← fd ]
  f' = concatMap expVars (el ++ er)
  (b, f) = stmVars vs s
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
  expVar p (ECall cid es) = concatMap expVars es ++ cIdentToString cid : p
  expVar p (EVar cid) = cIdentToString cid : p
  expVar p (EIndex cid e) = expVars e ++ cIdentToString cid : p
  expVar p (EIndexDouble cid e1 e2) = expVars e1 ++ expVars e2 ++ cIdentToString cid : p
  expVar p _ = p

gather' ∷ Monoid a => (Exp → Writer a Exp) → [Stm] → a
gather' f ss = execWriter (mapM_ (mapStmExpM f) ss)

-- Get a list of all variables references
usedVars' ∷ [Stm] → [String]
usedVars' = nub . gather' collect
 where
  collect ∷ Exp → Writer [String] Exp
  collect e@(EVar cid) = tell [cIdentToString cid] >> return e
  collect e@(ECall cid es) = tell [cIdentToString cid] >> mapM_ collect es >> return e
  collect e@(EIndex cid es) = tell [cIdentToString cid] >> collect es >> return e
  collect e@(EIndexDouble cid e1 e2) = tell [cIdentToString cid] >> collect e1 >> collect e2 >> return e
  collect e = mapExpM collect e

-- }}}

-- vi:fdm=marker
