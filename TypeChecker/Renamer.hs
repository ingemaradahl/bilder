{-# Language UnicodeSyntax #-}

module TypeChecker.Renamer where

import Control.Applicative
import Control.Monad
import Control.Monad.State hiding (mapM)
import Control.Monad.Reader

import Data.Tree
import Data.Map as Map hiding (fold)
import Data.Monoid

import TypeChecker.TCM
import TypeChecker.TCM.Utils hiding (initScope)
import TypeChecker.TCM.Errors

import TypeChecker.Utils
import TypeChecker.Inferring

import TypeChecker.Environment hiding (pushScope, popScope)
import TypeChecker.Renamer.Utils

import TypeChecker.Types as Types
import TypeChecker.Types.Blob (Blob, filename)
import qualified TypeChecker.Types.Blob as Blob (functions, typedefs, variables)

import Compiler.Utils hiding (paramToVar)

import FrontEnd.AbsGrammar

fst3 ∷ (a,b,c) → a
fst3 (x,_,_) = x

snd3 ∷ (a,b,c) → b
snd3 (_,x,_) = x

trd3 ∷ (a,b,c) → c
trd3 (_,_,x) = x

rename ∷ Blob → [Tree (Source, Blob, Aliases)] → TCM (Source, Blob, Aliases)
rename b ss = do
  done ← gets renamed
  if filename b `elem` done
    then return (mempty :: Source, b, Map.empty)
    else renameBlob b ss

strip ∷ (Source, Blob, Aliases) → Source
strip = fst3

renameBlob ∷ Blob → [Tree (Source, Blob, Aliases)] → TCM (Source, Blob, Aliases)
renameBlob blob children = do
  clearScope

  -- Insert aliases from children
  modify (\st → st { aliases = [Map.unions $ Prelude.map (trd3 . rootLabel) children] })

  -- Populate Scope
  mapM_ (mergeTypedefs . Blob.typedefs . snd3 . rootLabel) children
  mergeTypedefs $ Blob.typedefs blob
  mapM_ (addSource . fst3 . rootLabel) children
  mergeVariables $ Blob.variables blob
  annotFuns ← mapM annotateFunction $ concat $ elems (Blob.functions blob)
  mapM_ addFunction annotFuns

  variables' ← renameVariables $ Blob.variables blob
  functions' ← mapM renameFunction annotFuns

  -- replace all TDefined with the actual type.
  let tdefs = Prelude.map (Blob.typedefs . snd3) $ concatMap flatten children
      funs' = Prelude.map (\f → runReader (replaceFunTDefs f) tdefs) functions'

  aliases' ← gets (head . aliases)

  return (Source
    (fromList (Prelude.map (\f → (alias f, f )) funs'))
    (Map.map (\v → runReader (replaceVarTDefs v) tdefs) variables')
    ,

    blob

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

replaceVarTDefs ∷ Variable → Reader [Map String Typedef] Variable
replaceVarTDefs var = do
  t ← replaceType (varType var)
  me ← maybe (return Nothing) (liftM Just . replaceExp) (value var)
  return $ var { varType = t, value = me }

replaceFunTDefs ∷ Function → Reader [Map String Typedef] Function
replaceFunTDefs fun = do
  stms' ← mapM replaceStm (statements fun)
  return $ fun { statements = stms' }

replaceStm ∷ Stm → Reader [Map String Typedef] Stm
replaceStm (SType t s) = SType <$> replaceType t <*> replaceStm s
replaceStm (SFunDecl i t px ps ss) =
  SFunDecl i <$> replaceType t <*> pure px <*> mapM replaceParam ps <*> mapM replaceStm ss
replaceStm (SFor tk fds ecs eis s) =
  SFor tk <$> mapM (mapForDeclExpM replaceExp) fds <*> mapM replaceExp ecs <*> mapM replaceExp eis <*> replaceStm s
replaceStm s = mapStmExpM replaceExp s

replaceParam ∷ Param → Reader [Map String Typedef] Param
replaceParam (ParamDec qs cid) =
  ParamDec <$> mapM replaceQualifier qs <*> pure cid
replaceParam (ParamDefault qs cid tk e) =
  ParamDefault <$> mapM replaceQualifier qs <*> pure cid <*> pure tk <*> replaceExp e

replaceQualifier ∷ Qualifier → Reader [Map String Typedef] Qualifier
replaceQualifier (QType t) = QType <$> replaceType t
replaceQualifier q = pure q

replaceExp ∷ Exp → Reader [Map String Typedef] Exp
replaceExp (ETypeCall t es) =
  ETypeCall <$> replaceType t <*> mapM replaceExp es
replaceExp (EPartCall cid es ts) =
  EPartCall cid <$> mapM replaceExp es <*> mapM replaceType ts
replaceExp (ECurryCall cid e t) =
  ECurryCall cid <$> replaceExp e <*> replaceType t
replaceExp (EVarType cid t) =
  EVarType cid <$> replaceType t
replaceExp e = mapExpM replaceExp e

replaceType ∷ Type → Reader [Map String Typedef] Type
replaceType (TDefined (TypeIdent (_, i))) = do
  ms ← ask
  case Prelude.map (Map.lookup i) ms of
    [] → error $ "could not find any typedef for " ++ show i
    (Just t:_) → return $ typedefType t
replaceType (TArray t) = replaceType t
replaceType (TFunc tl tk tr) =
  TFunc <$> replaceType tl <*> pure tk <*> replaceType tr
replaceType (TFun t ts) = TFun <$> replaceType t <*> mapM replaceType ts
replaceType (TConst t) = TConst <$> replaceType t
replaceType t = return t


renameVariables ∷ Map String Variable → TCM (Map String Variable)
renameVariables vars = do
  vars' ← mapM (renameVariable . snd) (toList vars)
  return $ fromList $ Prelude.map (\v → (ident v, v)) vars'

addToScope ∷ Decl → TCM ()
addToScope (Dec qs post) = do
  t ← verifyQualsType qs >>= filterTDef
  sequence_ [ addCIdentVariable cid t (declPostExp post) | cid ← declPostIdents post]

renameStm ∷ Stm → TCM Stm
renameStm (SDecl dec) = addToScope dec >> SDecl <$> renameDecl dec
renameStm (SExp e) = SExp <$> renameExp e
renameStm (SBlock ss) = SBlock <$> mapM renameStm ss
renameStm (SWhile tk e s) = SWhile tk <$> renameExp e <*> renameStm s
renameStm (SDoWhile tkdo s tkwh e) = SDoWhile tkdo <$> renameStm s <*>
  pure tkwh <*> renameExp e
renameStm (SFor tk decls esl esr s) = do
  pushScope
  mapM_ addToScope [ d | (FDecl d) ← decls ]
  decls' ← mapM renameForDecl decls
  esl' ← mapM renameExp esl
  esr' ← mapM renameExp esr
  s' ← renameStm s
  popScope
  return $ SFor tk decls' esl' esr' s'
renameStm (SReturn t e) = SReturn t <$> renameExp e
renameStm (SIf tk e s) = SIf tk <$> renameExp e <*> renameStm s
renameStm (SIfElse tki e st tke sf) = SIfElse tki <$> renameExp e <*>
  renameStm st <*> pure tke <*> renameStm sf
renameStm (SType t s) = SType <$> filterTDef t <*> renameStm s
renameStm (SFunDecl cid t px ps ss) = do
  ps' ← mapM paramToVar ps
  file ← gets currentFile
  t' ← filterTDef t
  let (TFun ret _) = t'
  addCIdentVariable cid (TFun ret (Prelude.map varType ps')) Nothing

  fun ← annotateFunction Types.Function {
      functionName = cIdentToString cid
    , alias = ""
    , functionLocation = (file, cIdentToPos cid)
    , retType = ret
    , pixelwise = px == ITrue
    , paramVars = ps'
    , parameters = ps
    , statements = ss
  }

  addFunction fun

  fun' ← renameFunction fun

  setCIdentAssigned cid

  return $ SFunDecl (toCIdent fun') t' px (parameters fun') (statements fun')
renameStm s = mapStmM renameStm s

renameExp ∷ Exp → TCM Exp
renameExp (EVar cid) = setCIdentAssigned cid >> EVar <$> renameCIdent cid
renameExp (EIndex cid e) = setCIdentAssigned cid >> EIndex <$> renameCIdent cid <*> renameExp e
renameExp (EIndexDouble cid e1 e2) = setCIdentAssigned cid >> EIndexDouble <$> renameCIdent cid <*> renameExp e1 <*> renameExp e2
renameExp (ECall cid es) = do
  args ← mapM inferExp es
  funsAlias ← lookupAliasMaybe (cIdentToString cid) >>=
      (\x → case x of Just y → liftM Just (lookupFunction y); Nothing → return Nothing )
  funs ← lookupFunction (cIdentToString cid)
  fun ← maybe err return $ tryApply funs args `mplus` tryUncurry funs args
          `mplus` (funsAlias >>= (`tryApply` args)) `mplus` (funsAlias >>= (`tryUncurry` args))

  -- It's possible that the found function is created on the fly by
  -- lookupFunction, in which case the alias field is empty.
  alias' ← if Prelude.null (alias fun)
    then lookupAlias (cIdentToString cid)
    else return (alias fun)

  -- Rename argument expressions
  es' ← mapM renameExp es

  -- If the argument types match, whe have a clean application, otherwise it's
  -- either a partial application or the argument is uncurried
  let funArgs = Prelude.map varType (paramVars fun)
  if funArgs == args
    then return $ ECall (newCIdent cid alias') es'
    else if curried funArgs args
      then return $ ECurryCall (newCIdent cid alias') (head es') (head args)
      else return $ EPartCall (newCIdent cid alias') es' args
 where
  err = compileError (cIdentToPos cid) $ "Unable to find function " ++ cIdentToString cid
  curried ∷ [Type] → [Type] → Bool
  curried funArgs inferred = length inferred == 1 && isVec (head inferred) &&
    length funArgs == vecLength (head inferred)
renameExp e = mapExpM renameExp e

renameVariable ∷ Variable → TCM Variable
renameVariable (Variable name loc typ e) = Variable <$> newAlias name <*>
  pure loc <*> pure typ <*> maybe (pure Nothing) (liftM Just . renameExp) e

filterQuals ∷ [Qualifier] → TCM [Qualifier]
filterQuals (QType t:qs) = (:) <$> (QType <$> filterTDef t) <*> filterQuals qs
filterQuals (q:qs) = (:) <$> pure q <*> filterQuals qs
filterQuals [] = pure []

renameParam ∷ Param → TCM Param
renameParam (ParamDec qs cid) = verifyQualsType qs >>= filterTDef >>=
  (\t → addCIdentVariable cid t Nothing) >> ParamDec <$> filterQuals qs <*> renameCIdent cid
renameParam (ParamDefault qs cid tk e) = do
  e' ← renameExp e
  verifyQualsType qs >>= filterTDef >>= (\t → addCIdentVariable cid t (Just e'))
  ParamDefault <$> filterQuals qs <*> renameCIdent cid <*> pure tk <*> pure e'

renameDecl ∷ Decl → TCM Decl
renameDecl (Dec qs post) = do
  mapM_ setCIdentAssigned $ declPostIdents post
  Dec <$> filterQuals qs <*> renameDeclPost post
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

newCIdent ∷ CIdent → String → CIdent
newCIdent (CIdent (pos, _)) s = CIdent (pos, s)
