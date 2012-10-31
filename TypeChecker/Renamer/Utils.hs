{-# Language UnicodeSyntax #-}

module TypeChecker.Renamer.Utils where

import Control.Applicative
import Control.Monad.State hiding (mapM)

import Data.Map as Map (insert, lookup, empty)

import TypeChecker.TCM
import TypeChecker.TCM.Utils
import TypeChecker.TCM.Errors

import TypeChecker.Types
import TypeChecker.Environment

import FrontEnd.AbsGrammar

import Text.Printf

popId ∷ TCM ()
popId = modify (\st → st { freeAliases = tail (freeAliases st)})

newId ∷ TCM String
newId = gets (head . freeAliases) >>= (\i → popId >> return (printf "_%03d" i))

newIdSuffix ∷ String → TCM String
newIdSuffix s = (++) <$> newId <*> pure s

newAlias ∷ String → TCM String
newAlias s = do
  s' ← newIdSuffix s
  as ← gets aliases
  modify (\st → st { aliases = insert s s' (head as):tail as})
  return s'

pushAlias ∷ TCM ()
pushAlias = modify (\st → st { aliases = Map.empty:aliases st})

popAlias ∷ TCM ()
popAlias = modify (\st → st { aliases = tail (aliases st)})

lookupAlias ∷ String → TCM String
lookupAlias s = do
  as ← gets aliases
  case lookupAlias' as of
    Just alias' → return alias'
    Nothing → debugError "ALIAS NOT FOUND"
 where
  lookupAlias' ∷ [Aliases] → Maybe String
  lookupAlias' [] = Nothing
  lookupAlias' (a:as) =
    case Map.lookup s a of
      Just alias' → Just alias'
      Nothing → lookupAlias' as

addSource ∷ Source → TCM ()
addSource src = do
  mergeFunctions' (functions src)
  mergeVariables (variables src)
  mergeTypedefs  (typedefs src)

renameVariable ∷ Variable → TCM Variable
renameVariable (Variable name loc typ) = Variable <$> newAlias name <*>
  pure loc <*> pure typ

annotateFunction ∷ Function → TCM Function
annotateFunction f = do
  ident' ← newAlias (ident f)
  return $ f { alias = ident' }

renameParam ∷ Param → TCM Param
renameParam (ParamDec qs cid) = ParamDec <$> pure qs <*> renameCIdent cid
renameParam (ParamDefault qs cid tk e) = ParamDefault <$> pure qs <*>
  renameCIdent cid <*> pure tk <*> renameExp e

renameCIdent ∷ CIdent → TCM CIdent
renameCIdent (CIdent (pos, s)) = CIdent <$> ((,) <$> pure pos <*> lookupAlias s)

renameStm ∷ Stm → TCM Stm
renameStm = undefined

renameExp ∷ Exp → TCM Exp
renameExp (EVar cid) = EVar <$> renameCIdent cid
renameExp (ECall cid e) = ECall <$> renameCIdent cid <*> mapM renameExp e
renameExp e =  mapExp renameExp e


--awk '{print $0, substr($3,2), "<$> f el <*> pure tk <*> f er"}'
mapExp ∷ (Monad m, Applicative m) => (Exp → m Exp) → Exp → m Exp
mapExp f (EAss el tk er) = EAss <$> f el <*> pure tk <*> f er
mapExp f (EAssAdd el tk er) = EAssAdd <$> f el <*> pure tk <*> f er
mapExp f (EAssSub el tk er) = EAssSub <$> f el <*> pure tk <*> f er
mapExp f (EAssMul el tk er) = EAssMul <$> f el <*> pure tk <*> f er
mapExp f (EAssDiv el tk er) = EAssDiv <$> f el <*> pure tk <*> f er
mapExp f (EAssMod el tk er) = EAssMod <$> f el <*> pure tk <*> f er
mapExp f (EAssBWAnd el tk er) = EAssBWAnd <$> f el <*> pure tk <*> f er
mapExp f (EAssBWXOR el tk er) = EAssBWXOR <$> f el <*> pure tk <*> f er
mapExp f (EAssBWOR el tk er) = EAssBWOR <$> f el <*> pure tk <*> f er
mapExp f (EOR el tk er) = EOR <$> f el <*> pure tk <*> f er
mapExp f (EXOR el tk er) = EXOR <$> f el <*> pure tk <*> f er
mapExp f (EAnd el tk er) = EAnd <$> f el <*> pure tk <*> f er
mapExp f (EBWOR el tk er) = EBWOR <$> f el <*> pure tk <*> f er
mapExp f (EBWXOR el tk er) = EBWXOR <$> f el <*> pure tk <*> f er
mapExp f (EBWAnd el tk er) = EBWAnd <$> f el <*> pure tk <*> f er
mapExp f (EEqual el tk er) = EEqual <$> f el <*> pure tk <*> f er
mapExp f (ENEqual el tk er) = ENEqual <$> f el <*> pure tk <*> f er
mapExp f (ELt el tk er) = ELt <$> f el <*> pure tk <*> f er
mapExp f (EGt el tk er) = EGt <$> f el <*> pure tk <*> f er
mapExp f (ELEt el tk er) = ELEt <$> f el <*> pure tk <*> f er
mapExp f (EGEt el tk er) = EGEt <$> f el <*> pure tk <*> f er
mapExp f (EBWShiftLeft el tk er) = EBWShiftLeft <$> f el <*> pure tk <*> f er
mapExp f (EBWShiftRight el tk er) = EBWShiftRight <$> f el <*> pure tk <*> f er
mapExp f (EAdd el tk er) = EAdd <$> f el <*> pure tk <*> f er
mapExp f (ESub el tk er) = ESub <$> f el <*> pure tk <*> f er
mapExp f (EMul el tk er) = EMul <$> f el <*> pure tk <*> f er
mapExp f (EDiv el tk er) = EDiv <$> f el <*> pure tk <*> f er
mapExp f (EMod el tk er) = EMod <$> f el <*> pure tk <*> f er
mapExp f (ECond eq tkl el tkr er) = ECond <$> f eq <*> pure tkl <*> f el <*> pure tkr <*> f er
mapExp f (ENeg tk e) = ENeg tk <$> f e
mapExp f (ENegSign tk e) = ENegSign tk <$> f e
mapExp f (EComplement tk e) = EComplement tk <$> f e
mapExp f (EPos tk e) = EPos tk <$> f e
mapExp f (EPreInc tk e) = EPreInc tk <$> f e
mapExp f (EPreDec tk e) = EPreDec tk <$> f e
mapExp f (EPostInc e tk) = EPostInc <$> f e <*> pure tk
mapExp f (EPostDec e tk) = EPostDec <$> f e <*> pure tk
mapExp f (EMember e cid) = EMember <$> f e <*> pure cid
mapExp f (EMemberCall el cid es) = EMemberCall <$> f el <*> pure cid <*> mapM f es
mapExp f (ECall cid es) = ECall cid <$> mapM f es
mapExp f (ETypeCall t es) = ETypeCall t <$> mapM f es
mapExp f (EIndex cid e) = EIndex cid <$> f e
mapExp _ (EVar cid) = pure $ EVar cid
mapExp _ e@(EFloat {}) = pure e
mapExp _ e@(EInt {}) = pure e
mapExp _ e@ETrue = pure e
mapExp _ e@EFalse = pure e

