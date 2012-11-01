{-# Language UnicodeSyntax #-}

module TypeChecker.Renamer where

import Control.Applicative
import Control.Monad.State hiding (mapM)

import Data.Tree
import Data.Map as Map hiding (fold, map)
import Data.Monoid

import TypeChecker.TCM
import TypeChecker.TCM.Utils hiding (initScope)

import TypeChecker.Utils
import TypeChecker.Inferring

import TypeChecker.Environment hiding (pushScope, popScope)
import TypeChecker.Renamer.Utils

import TypeChecker.Types as Types
import TypeChecker.Types.Blob (Blob, filename)
import qualified TypeChecker.Types.Blob as Blob (functions, typedefs, variables)

import Compiler.Utils

import FrontEnd.AbsGrammar

rename ∷ Blob → [Tree (Source, Aliases)] → TCM (Source, Aliases)
rename b ss = do
  done ← gets renamed
  if filename b `elem` done
    then return (mempty :: Source, Map.empty)
    else renameBlob b ss

renameBlob ∷ Blob → [Tree (Source, Aliases)] → TCM (Source, Aliases)
renameBlob blob children = do
  clearScope

  -- Insert aliases from children
  modify (\st → st { aliases = [Map.unions $ Prelude.map (snd . rootLabel) children] })

  -- Populate Scope
  mapM_ (addSource . fst . rootLabel) children
  annotFuns ← mapM annotateFunction $ concat $ elems (Blob.functions blob)
  mapM_ addFunction annotFuns

  variables' ← renameVariables $ Blob.variables blob
  functions' ← mapM renameFunction annotFuns

  aliases' ← gets (head . aliases)

  return (Source
    (fromList (Prelude.map (\f → (ident f, f)) functions'))
    Map.empty
    variables'

    , aliases')


renameFunction ∷ Function → TCM Function
renameFunction fun = do
  updateFile (locfile fun)
  pushScope
  pushAlias

  paramVars' ← mapM renameVariable (paramVars fun)
  parameters' ← mapM renameParam (parameters fun)
  statements' ← mapM renameStm (statements fun)

  popAlias
  popScope

  return fun {
      functionName = alias fun
    , paramVars = paramVars'
    , parameters = parameters'
    , statements = statements'
  }

renameVariables ∷ Map String Variable → TCM (Map String Variable)
renameVariables vars = do
  vars' ← mapM (renameVariable . snd) (toList vars)
  return $ fromList $ Prelude.map (\v → (ident v, v)) vars'


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
renameStm (SFunDecl cid t ps ss) = do
  ps' ← mapM paramToVar ps
  file ← gets currentFile
  addCIdentVariable cid (TFun t (map varType ps'))

  fun ← annotateFunction Types.Function {
      functionName = cIdentToString cid
    , alias = ""
    , functionLocation = (file, cIdentToPos cid)
    , retType = t
    , paramVars = ps'
    , parameters = ps
    , statements = ss
  }

  addFunction fun

  fun' ← renameFunction fun

  return $ SFunDecl (toCIdent fun') (retType fun') (parameters fun') (statements fun')
renameStm s = mapStmM renameStm s

renameExp ∷ Exp → TCM Exp
renameExp (EVar cid) = EVar <$> renameCIdent cid
renameExp (ECall cid e) = ECall <$> renameCIdent cid <*> mapM renameExp e-- FIND CORRECT THINGY
renameExp e =  mapExpM renameExp e

renameVariable ∷ Variable → TCM Variable
renameVariable (Variable name loc typ) = Variable <$> newAlias name <*>
  pure loc <*> pure typ

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
renameDeclPost (Vars cids) = Vars <$> mapM newCIdAlias cids
renameDeclPost (DecAss cids tk e) = DecAss <$> mapM newCIdAlias cids <*>
  pure tk <*> renameExp e
renameDeclPost (DecFun {}) = error "Should not happen :("

renameCIdent ∷ CIdent → TCM CIdent
renameCIdent (CIdent (pos, s)) = CIdent <$> ((,) <$> pure pos <*> lookupAlias'' s pos)
