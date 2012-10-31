{-# Language UnicodeSyntax #-}

module TypeChecker.Renamer.Utils where

import Control.Applicative
import Control.Monad.State hiding (mapM)

import Data.Map as Map (insert, lookup, empty)

import TypeChecker.TCM
import TypeChecker.TCM.Utils
import TypeChecker.TCM.Errors

import TypeChecker.Types
import TypeChecker.Environment hiding (pushScope, popScope)

import Compiler.Utils

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
    Nothing → debugError $ "ALIAS " ++ s ++ "NOT FOUND"
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

renameDecl ∷ Decl → TCM Decl
renameDecl (Dec qs post) = Dec qs <$> renameDeclPost post
-- TODO STRUCT HELL

renameForDecl ∷ ForDecl → TCM ForDecl
renameForDecl (FDecl dec) = FDecl <$> renameDecl dec
renameForDecl (FExp e) = FExp <$> renameExp e

renameDeclPost ∷ DeclPost → TCM DeclPost
renameDeclPost (Vars cids) = Vars <$> mapM renameCIdent cids
renameDeclPost (DecAss cids tk e) = DecAss <$> mapM renameCIdent cids <*>
  pure tk <*> renameExp e
-- TODO SCOPING IN DECFUN
renameDeclPost (DecFun cid ps ss) = do
  cid' ← renameCIdent cid
  ps'  ← mapM renameParam ps

  pushScope
  pushAlias
  ss' ← mapM renameStm ss
  popAlias
  popScope

  return $ DecFun cid' ps' ss'

renameCIdent ∷ CIdent → TCM CIdent
renameCIdent (CIdent (pos, s)) = CIdent <$> ((,) <$> pure pos <*> lookupAlias s)

renameStm ∷ Stm → TCM Stm
renameStm (SDecl dec) = SDecl <$> renameDecl dec
renameStm (SExp e) = SExp <$> renameExp e
renameStm (SBlock ss) = SBlock <$> mapM renameStm ss
renameStm (SWhile tk e s) = SWhile tk <$> renameExp e <*> renameStm s
renameStm (SDoWhile tkdo s tkwh e) = SDoWhile tkdo <$> renameStm s <*>
  pure tkwh <*> renameExp e
renameStm (SFor tk decls esl esr s) = SFor tk <$> mapM renameForDecl decls <*>
  mapM renameExp esl <*> mapM renameExp esr <*> renameStm s
renameStm (SIf tk e s) = SIf tk <$> renameExp e <*> renameStm s
renameStm (SIfElse tki e st tke sf) = SIfElse tki <$> renameExp e <*>
  renameStm st <*> pure tke <*> renameStm sf
renameStm (SType t s) = SType t <$> renameStm s
renameStm (SFunDecl cid t ps ss) = SFunDecl <$> renameCIdent cid <*> pure t <*>
  mapM renameParam ps <*> mapM renameStm ss
renameStm s = mapStmM renameStm s

renameExp ∷ Exp → TCM Exp
renameExp (EVar cid) = EVar <$> renameCIdent cid
renameExp (ECall cid e) = ECall <$> renameCIdent cid <*> mapM renameExp e
renameExp e =  mapExpM renameExp e

