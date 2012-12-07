{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Simple.Utils where

import Control.Applicative
import Control.Monad
import Control.Monad.Identity

import Compiler.Simple.AbsSimple

foldExpM ∷ Monad m => (a → Exp → m a) → a → Exp → m a
foldExpM f p e@(EAss el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssAdd el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssSub el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssMul el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssDiv el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssMod el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssBWAnd el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssBWXOR el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAssBWOR el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ECond econd etrue efalse) =
  foldM (foldExpM f) p [econd,etrue,efalse] >>= flip f e
foldExpM f p e@(EOR el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EXOR el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAnd el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EBWOR el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EBWXOR el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EBWAnd el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EEqual el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ENEqual el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ELt el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EGt el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ELEt el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EGEt el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EBWShiftLeft el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EBWShiftRight el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EAdd el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ESub el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EMul el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EDiv el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(EMod el er) = foldM (foldExpM f) p [el,er] >>= flip f e
foldExpM f p e@(ENeg ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(ENegSign ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EComplement ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EPos ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EPreInc ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EPreDec ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EPostInc ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EPostDec ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EMember ei _) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EMemberCall ei _ es) = foldM (foldExpM f) p (ei:es) >>= flip f e
foldExpM f p e@(ECall _ es) = foldM (foldExpM f) p es >>= flip f e
foldExpM f p e@(ETypeCall _ es) = foldM (foldExpM f) p es >>= flip f e
foldExpM f p e@(EVar {}) = f p e
foldExpM f p e@(EIndex _ ei) = foldExpM f p ei >>= flip f e
foldExpM f p e@(EFloat {}) = f p e
foldExpM f p e@(EInt {}) = f p e
foldExpM f p e@ETrue = f p e
foldExpM f p e@EFalse = f p e

foldExp ∷ (a → Exp → a) → a → Exp → a
foldExp f p e = runIdentity (foldExpM (liftIdentity f) p e)

foldStmM ∷ Monad m => (a → Stm → m a) → a → Stm → m a
foldStmM f p s@(SDecl {}) = f p s
foldStmM f p s@(SDeclAss {}) = f p s
foldStmM f p s@(SExp {}) = f p s
foldStmM f p s@(SWhile _ is) = foldM (foldStmM f) p is >>= flip f s
foldStmM f p s@(SDoWhile is _) = foldM (foldStmM f) p is >>= flip f s
foldStmM f p s@(SFor _ _ _ is) = foldM (foldStmM f) p is >>= flip f s
foldStmM f p s@(SReturn {}) = f p s
foldStmM f p s@(SVoidReturn {}) = f p s
foldStmM f p s@(SIf _ is) = foldM (foldStmM f) p is >>= flip f s
foldStmM f p s@(SIfElse _ istrue isfalse) =
  foldM (foldStmM f) p (istrue ++ isfalse) >>= flip f s
foldStmM f p s@(SBreak {}) = f p s
foldStmM f p s@(SContinue {}) = f p s
foldStmM f p s@(SDiscard {}) = f p s

foldStm ∷ (a → Stm → a) → a → Stm → a
foldStm f p s = runIdentity (foldStmM (liftIdentity f) p s)

foldStmExpM ∷ Monad m => (a → Exp → m a) → a → Stm → m a
foldStmExpM f p (SDeclAss _ e) = foldExpM f p e
foldStmExpM f p (SExp e) = foldExpM f p e
foldStmExpM f p (SWhile e ss) = foldExpM f p e >>= (\l → foldM (foldStmExpM f) l ss)
foldStmExpM f p (SDoWhile ss e) = foldM (foldStmExpM f) p ss >>= (\l → foldExpM f l e)
foldStmExpM f p (SFor ss econd epost sblock) = do
  ss' ← foldM (foldStmExpM f) p ss
  econd' ← foldM (foldExpM f) ss' econd
  epost' ← foldM (foldExpM f) econd' epost
  foldM (foldStmExpM f) epost' sblock
foldStmExpM f p (SReturn e) = foldExpM f p e
foldStmExpM f p (SIf e ss) = foldExpM f p e >>= (\l → foldM (foldStmExpM f) l ss)
foldStmExpM f p (SIfElse e st sf) = foldExpM f p e >>=
  (\l → foldM (foldStmExpM f) l st) >>= (\l → foldM (foldStmExpM f) l sf)
foldStmExpM _ p _ = return p

foldStmExp ∷ (a → Exp → a) → a → Stm → a
foldStmExp f p s = runIdentity (foldStmExpM (liftIdentity f) p s)

liftIdentity ∷ (a → b → a) → a → b → Identity a
liftIdentity f' p' s' = Identity (f' p' s')

mapExpM ∷ (Monad m, Applicative m) => (Exp → m Exp) → Exp → m Exp
mapExpM f (EAss el er) = EAss <$> f el <*> f er
mapExpM f (EAssAdd el er) = EAssAdd <$> f el <*> f er
mapExpM f (EAssSub el er) = EAssSub <$> f el <*> f er
mapExpM f (EAssMul el er) = EAssMul <$> f el <*> f er
mapExpM f (EAssDiv el er) = EAssDiv <$> f el <*> f er
mapExpM f (EAssMod el er) = EAssMod <$> f el <*> f er
mapExpM f (EAssBWAnd el er) = EAssBWAnd <$> f el <*> f er
mapExpM f (EAssBWXOR el er) = EAssBWXOR <$> f el <*> f er
mapExpM f (EAssBWOR el er) = EAssBWOR <$> f el <*> f er
mapExpM f (EOR el er) = EOR <$> f el <*> f er
mapExpM f (EXOR el er) = EXOR <$> f el <*> f er
mapExpM f (EAnd el er) = EAnd <$> f el <*> f er
mapExpM f (EBWOR el er) = EBWOR <$> f el <*> f er
mapExpM f (EBWXOR el er) = EBWXOR <$> f el <*> f er
mapExpM f (EBWAnd el er) = EBWAnd <$> f el <*> f er
mapExpM f (EEqual el er) = EEqual <$> f el <*> f er
mapExpM f (ENEqual el er) = ENEqual <$> f el <*> f er
mapExpM f (ELt el er) = ELt <$> f el <*> f er
mapExpM f (EGt el er) = EGt <$> f el <*> f er
mapExpM f (ELEt el er) = ELEt <$> f el <*> f er
mapExpM f (EGEt el er) = EGEt <$> f el <*> f er
mapExpM f (EBWShiftLeft el er) = EBWShiftLeft <$> f el <*> f er
mapExpM f (EBWShiftRight el er) = EBWShiftRight <$> f el <*> f er
mapExpM f (EAdd el er) = EAdd <$> f el <*> f er
mapExpM f (ESub el er) = ESub <$> f el <*> f er
mapExpM f (EMul el er) = EMul <$> f el <*> f er
mapExpM f (EDiv el er) = EDiv <$> f el <*> f er
mapExpM f (EMod el er) = EMod <$> f el <*> f er
mapExpM f (ECond eq el er) = ECond <$> f eq <*> f el <*> f er
mapExpM f (ENeg e) = ENeg <$> f e
mapExpM f (ENegSign e) = ENegSign <$> f e
mapExpM f (EComplement e) = EComplement <$> f e
mapExpM f (EPos e) = EPos <$> f e
mapExpM f (EPreInc e) = EPreInc <$> f e
mapExpM f (EPreDec e) = EPreDec <$> f e
mapExpM f (EPostInc e) = EPostInc <$> f e
mapExpM f (EPostDec e) = EPostDec <$> f e
mapExpM f (EMember e cid) = EMember <$> f e <*> pure cid
mapExpM f (EMemberCall el cid es) = EMemberCall <$> f el <*> pure cid <*> mapM f es
mapExpM f (ECall cid es) = ECall cid <$> mapM f es
mapExpM f (ETypeCall t es) = ETypeCall t <$> mapM f es
mapExpM f (EIndex cid e) = EIndex cid <$> f e
mapExpM _ e@(EVar {}) = pure e
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
mapStmM _ s@(SDeclAss {}) = pure s
mapStmM _ s@(SExp {}) = pure s
mapStmM f (SWhile e s) = SWhile e <$> mapM f s
mapStmM f (SDoWhile s e) = SDoWhile <$> mapM f s <*> pure e
mapStmM f (SFor dec el er s) = SFor dec el er <$> mapM f s
mapStmM _ s@(SReturn {}) = pure s
mapStmM _ s@(SVoidReturn {}) = pure s
mapStmM f (SIf e s) = SIf e <$> mapM f s
mapStmM f (SIfElse e st se) = SIfElse e <$> mapM f st <*> mapM f se
mapStmM _ s@(SBreak {}) = pure s
mapStmM _ s@(SContinue {}) = pure s
mapStmM _ s@(SDiscard {}) = pure s

mapStm ∷ (Stm → Stm) → Stm → Stm
mapStm f s = runIdentity $ mapStmM f' s
 where
  f' ∷ Stm → Identity Stm
  f' stm = return $ f stm

mapStmExpM ∷ (Monad m, Applicative m) => (Exp → m Exp) → Stm → m Stm
mapStmExpM f (SDeclAss var e) = SDeclAss var <$> f e
mapStmExpM f (SExp e) = SExp <$> f e
mapStmExpM f (SDoWhile s e) = SDoWhile <$> mapM (mapStmExpM f) s <*> f e
mapStmExpM f (SWhile e s) = SWhile <$> f e <*> mapM (mapStmExpM f) s
mapStmExpM f (SFor fds ecs els s) =
  SFor <$> mapM (mapStmExpM f) fds <*> mapM f ecs <*> mapM f els <*> mapM (mapStmExpM f) s
mapStmExpM f (SReturn e) = SReturn <$> f e
mapStmExpM f (SIf e s) = SIf <$> f e <*> mapM (mapStmExpM f) s
mapStmExpM f (SIfElse e st stf) =
  SIfElse <$> f e <*> mapM (mapStmExpM f) st <*> mapM (mapStmExpM f) stf
mapStmExpM f s = mapStmM (mapStmExpM f) s

mapStmExp ∷ (Exp → Exp) → Stm → Stm
mapStmExp f s = runIdentity $ mapStmExpM f' s
 where
  f' ∷ Exp → Identity Exp
  f' e = return $ f e

expandStmM ∷ (Monad m, Applicative m) => ([Stm] → m [Stm]) → [Stm] → m [Stm]
--expandStmM f (SBlock stms:ss) = (:) <$> (SBlock <$> f stms) <*> f ss
expandStmM f (SWhile e s:ss) = do
  s' ← f s
  ss' ← f ss
  return $ SWhile e s':ss'
expandStmM f (SDoWhile s e:ss) = do
  s' ← f s
  ss' ← f ss
  return $ SDoWhile s' e:ss'
expandStmM f (SFor fdec es es' s:ss) =
  (:) <$> (SFor fdec es es' <$> f s) <*> f ss
expandStmM f (SIf e s:ss) = (:)
  <$> (SIf e <$> f s)
  <*> f ss
expandStmM f (SIfElse e st sf:ss) = do
  st' ← f st
  sf' ← f sf
  ss' ← f ss
  return $ SIfElse e st' sf':ss'
expandStmM f (s:ss) = (:) <$> pure s <*> expandStmM f ss
expandStmM _ [] = return []

expandStm ∷ ([Stm] → [Stm]) → [Stm] → [Stm]
expandStm f s = runIdentity $ expandStmM f' s
 where
  f' ∷ [Stm] → Identity [Stm]
  f' s' = return $ f s'
