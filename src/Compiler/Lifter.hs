{-
 -      This file is part of Bilder.
 -
 -   Bilder is free software: you can redistribute it and/or modify
 -   it under the terms of the GNU Lesser General Public License as published by
 -   the Free Software Foundation, either version 3 of the License, or
 -   (at your option) any later version.
 -
 -   Bilder is distributed in the hope that it will be useful,
 -   but WITHOUT ANY WARRANTY; without even the implied warranty of
 -   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -   GNU Lesser General Public License for more details.
 -
 -   You should have received a copy of the GNU Lesser General Public License
 -   along with Bilder.  If not, see <http://www.gnu.org/licenses/>.
 -
 -   Copyright © 2012-2013 Filip Lundborg
 -   Copyright © 2012-2013 Ingemar Ådahl
 -
 -}
{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Lifter where

import Data.Map (Map, toList, insert, lookup, empty, adjust, fromList, keys, union)
import Data.Maybe (fromJust, isNothing, isJust, catMaybes)
import Control.Monad.State
import Control.Applicative ((<$>), (<*>), pure)

import FrontEnd.AbsGrammar

import CompilerError
import Compiler.Utils
import CompilerTypes

import Text.Printf

import TypeChecker.Utils (cIdentToString, cIdentToPos, paramToString, paramToQuals)
import TypeChecker.Types (Source, functions, variables, paramVars)
import qualified TypeChecker.Types as T

-- Lifter monad - keeps source and lifting environment in State with CError
type LM a = StateT Environment CError a

data Environment = Environment {
  source ∷ Source,
  varTypes ∷ Map String Type,
  callExpansions ∷ Map String [String],
  currentFile ∷ String,
  warnings ∷ [String],
  paramRenameCount ∷ Int,
  outerVarTypes ∷ Map String Type,
  globalAssigns ∷ [(String, Type)]
}
 deriving (Show)

-- Environment and state helpers {{{
buildEnv ∷ Source → Environment
buildEnv s = Environment {
    source = s,
    varTypes = empty,
    callExpansions = empty,
    currentFile = "",
    warnings = [],
    paramRenameCount = 0,
    outerVarTypes = empty,
    globalAssigns = []
  }

nextRenameIndex ∷ LM Int
nextRenameIndex = do
  c ← gets paramRenameCount
  modify (\s → s { paramRenameCount = c + 1 })
  return c

warning ∷ String → LM ()
warning s = modify (\st → st { warnings = warnings st ++ [s] })

sourceFunctions ∷ LM (Map String T.Function)
sourceFunctions = gets (functions . source)

sourceVariables ∷ LM (Map String T.Variable)
sourceVariables = gets (variables . source)

addSourceFunction ∷ T.Function → LM ()
addSourceFunction f =
  modifySource (\s → s { functions = insert (T.functionName f) f (functions s) })

updateSourceFunction ∷ T.Function → LM ()
updateSourceFunction f =
  modifySource (\s → s { functions = adjust (\_ → f) (T.functionName f) (functions s) })

modifySource ∷ (Source → Source) → LM ()
modifySource fun = do
  src ← gets source
  modify (\s → s { source = fun src })

setCurrentFile ∷ String → LM ()
setCurrentFile f = modify (\s → s { currentFile = f })

addVarType ∷ String → Type → LM ()
addVarType n t = do
  vt ← gets varTypes
  modify (\s → s { varTypes = insert n t vt })

addGlobalAssign ∷ String → Type → LM ()
addGlobalAssign n t = do
  gas ← gets globalAssigns
  modify (\s → s { globalAssigns = (n, t) : gas })

setOuterVarTypes ∷ LM ()
setOuterVarTypes = do
  vs ← gets varTypes
  modify (\s → s { outerVarTypes = vs })

outerVarType ∷ String → LM (Maybe Type)
outerVarType n = do
  vt ← gets outerVarTypes
  return $ Data.Map.lookup n vt

-- | Returns the variables type (should always exist, hence no need for Maybe)
varType ∷ String → LM Type
varType n = do
  vt ← gets varTypes
  return $ let Just y = Data.Map.lookup n vt in y

clearVarTypes ∷ LM (Map String Type)
clearVarTypes = do
  vt ← gets varTypes
  modify (\s → s { varTypes = empty })
  return vt

setVarTypes ∷ Map String Type → LM ()
setVarTypes vt = modify (\s → s { varTypes = vt })

clearCallExpansions ∷ LM ()
clearCallExpansions = modify (\s → s { callExpansions = empty })

addCallExpansion ∷ String → [String] → LM ()
addCallExpansion n fs = do
  ces ← gets callExpansions
  modify (\s → s { callExpansions = insert n fs ces })
-- }}}-

-- | Lifts all free variables in all inner functions and globifies the inner functions.
lambdaLift ∷ LM ()
lambdaLift = do
  -- lift free variables.
  funs ← sourceFunctions
  mapM (liftFunVars . snd) (toList funs) >>= mapM updateSourceFunction

  -- lift inner functions.
  liftFuns

  -- Add all globally lifted variables.
  src ← gets source
  gvs ← buildGlobalDecls
  modify (\s → s { source = src { variables = gvs `union` variables src }})

buildGlobalDecls ∷ LM (Map String T.Variable)
buildGlobalDecls = do
  gas ← gets globalAssigns
  return $ fromList [ (n, T.Variable n ("madeup",(0,0)) t Nothing) | (n,t) ← gas ]

-- Variable lifting {{{
liftFunVars ∷ T.Function → LM T.Function
liftFunVars f = do
  clearVarTypes
  sequence_ [ addVarType (paramToString p) ((qualsToType . paramToQuals) p) | p ← T.parameters f ]
  stms ← mapM liftInnerFunVars (T.statements f)
  -- change all local assignments that have been globified.
  stms' ← liftM concat $ mapM replaceStm stms
  return f { T.statements = stms' }

-- | Replaces declarations of variables that have been lifted to the global scope.
replaceStm ∷ Stm → LM [Stm]
replaceStm s = do
  gas ← gets globalAssigns
  return $ replaceGlobals gas [s]

replaceGlobals ∷ [(String, Type)] → [Stm] → [Stm]
replaceGlobals gas (SDecl (Dec tk (Vars cs)):ss) =
  (SDecl $ Dec tk $ Vars cs') : replaceGlobals gas ss
 where
  cs' = filter (\c → cIdentToString c `notElem` map fst gas) cs
replaceGlobals gas (SDecl (Dec tk (DecAss cs tkd e)):ss) =
  if null cs'
    then rest
    else (SDecl $ Dec tk $ DecAss cs' tkd e) : rest
 where
  cs' = filter (\c → cIdentToString c `notElem` map fst gas) cs
  news = [ SExp $ EAss (EVar c) (TkAss ((0,0),"=")) e | c ← cs, cIdentToString c `elem` map fst gas ]
  rest = news ++ replaceGlobals gas ss
replaceGlobals gas ss = expandStm (replaceGlobals gas) ss

-- | Iterates a list of statements and lifts all inner functions variables.
liftInnerFunVars ∷ Stm → LM Stm
liftInnerFunVars (SType t stm) = SType t <$> liftInnerFunVars stm
liftInnerFunVars (SDecl d) = do
  -- Add all declared variables.
  let ts = declToTypeAndName d
  mapM_ (uncurry $ flip addVarType) ts
  SDecl <$> expandECallDecl d
liftInnerFunVars (SExp e) = SExp <$> expandECall e
liftInnerFunVars (SBlock stms) = SBlock <$> mapM liftInnerFunVars stms
liftInnerFunVars (SWhile t e stm) =
  SWhile t <$> expandECall e <*> liftInnerFunVars stm
liftInnerFunVars (SDoWhile tkdo stm tkwhile e) =
  SDoWhile tkdo <$> liftInnerFunVars stm <*> pure tkwhile <*> expandECall e
liftInnerFunVars (SFor tk fdecls econs eloop stm) =
  SFor tk <$> mapM expandECallForDecl fdecls <*> mapM expandECall econs <*> mapM expandECall eloop <*> liftInnerFunVars stm
liftInnerFunVars (SReturn tk e) = SReturn tk <$> expandECall e
liftInnerFunVars (SIf tk e s) = SIf tk <$> expandECall e <*> liftInnerFunVars s
liftInnerFunVars (SIfElse tkif e strue tkelse sfalse) =
  SIfElse tkif <$> expandECall e <*> liftInnerFunVars strue <*> pure tkelse <*> liftInnerFunVars sfalse
liftInnerFunVars (SFunDecl cid rt px ps stms) = do
  sequence_ [ addVarType (paramToString p) ((qualsToType . paramToQuals) p) | p ← ps ]

  setOuterVarTypes
  outerScopeVars ← gets (keys . varTypes)
  stms' ← mapM liftInnerFunVars stms

  -- Find all assignments to variables declared in outer scopes (not globals).
  mapM_ (mapStmExpM (\e → expGlobals e >> return e)) stms'

  -- Calculate free variables in the function.
  vars ← sourceVariables
  gas ← gets globalAssigns
  let globals = Data.Map.keys vars ++ ["fl_Resolution"] ++ map fst gas
  let frees = freeFunctionVars globals (cIdentToString cid, ps, stms')

  -- Filter out "real" function calls
  let frees' = filter (`elem` outerScopeVars) frees

  -- Add the free variables as parameters and return type.
  (ps', renames) ← prependFreeVars ps frees'

  -- Add all renamed variables as types aswell
  sequence_ [varType old >>= addVarType new | (old,new) ← toList renames]

  -- Remember the expansion so that all calls can be expanded.
  addCallExpansion (cIdentToString cid) frees'

  -- TODO: Examine why this doesn't work
  -- pst ← mapM (varType . paramToString) ps'
  -- addVarType (cIdentToString cid) (TFun rt pst)

  return $ SFunDecl cid rt px ps' (map (mapStmExp (renameExpVars renames)) stms')
liftInnerFunVars x = return x -- The rest: SBreak, SContinue, SDiscard

-- | Find all assigned.
expGlobals ∷ Exp → LM ()
expGlobals (EAss e _ _) = expVar e
expGlobals (EAssAdd e _ _) = expVar e
expGlobals (EAssSub e _ _) = expVar e
expGlobals (EAssMul e _ _) = expVar e
expGlobals (EAssDiv e _ _) = expVar e
expGlobals (EAssMod e _ _) = expVar e
expGlobals (EAssBWAnd e _ _) = expVar e
expGlobals (EAssBWXOR e _ _) = expVar e
expGlobals (EAssBWOR e _ _) = expVar e
expGlobals e = void $ mapExpM (\x → expGlobals x >> return x) e

expVar ∷ Exp → LM ()
expVar (EVar cid) = do
  t ← outerVarType (cIdentToString cid)
  when (isJust t) $ addGlobalAssign (cIdentToString cid) (fromJust t)
expVar (EMember e _) = expVar e
expVar (EIndex cid _) = do
  t ← outerVarType (cIdentToString cid)
  when (isJust t) $ addGlobalAssign (cIdentToString cid) (fromJust t)

-- | Renames all variables in an expression according to the Map.
renameExpVars ∷ Map String String → Exp → Exp
renameExpVars rm (EVar cid) = EVar (renameCIdent rm cid)
renameExpVars rm (ECall cid es) = ECall (renameCIdent rm cid) (map (renameExpVars rm) es)
renameExpVars rm (ECurryCall cid e t) = ECurryCall (renameCIdent rm cid) (renameExpVars rm e) t
renameExpVars rm (EIndex cid e) = EIndex (renameCIdent rm cid) (renameExpVars rm e)
renameExpVars rm e = mapExp (renameExpVars rm) e

renameForDeclVars ∷ Map String String → ForDecl → ForDecl
renameForDeclVars rm (FExp e) = FExp (renameExpVars rm e)
renameForDeclVars rm (FDecl (Dec qs (DecAss cids tk e))) =
  FDecl (Dec qs (DecAss cids tk (renameExpVars rm e)))
renameForDeclVars _ fd = fd

renameCIdent ∷ Map String String → CIdent → CIdent
renameCIdent rm (CIdent (pos,n)) =
  case Data.Map.lookup n rm of
    Nothing → CIdent (pos,n)
    Just n' → CIdent (pos,n')

-- | Expands all ECall's to a function in given statements with the added free variables.
expandECall ∷ Exp → LM Exp
expandECall (ECall cid es) = do
  eps ← gets callExpansions
  es' ← mapM expandECall es
  case Data.Map.lookup (cIdentToString cid) eps of
    Nothing → return $ ECall cid es'
    Just vs → return $ ECall cid (map nameToEVar vs ++ es')
expandECall (EPartCall cid es ts) = do
  eps ← gets callExpansions
  es' ← mapM expandECall es
  case Data.Map.lookup (cIdentToString cid) eps of
    Nothing → return $ EPartCall cid es' ts
    Just vs → EPartCall cid (map nameToEVar vs ++ es') <$> ((++) ts <$> mapM varType vs)
expandECall e@(EVar cid) = do
  eps ← gets callExpansions
  case Data.Map.lookup name eps of
    Nothing → do
      -- just a regular EVar... however
      -- if it's a function it should be replaced with ECall or EPartCall.
      funs ← sourceFunctions
      case Data.Map.lookup name funs of
        Nothing  → return e
        Just fun → if null $ paramVars fun
          then return $ ECall cid []
          else return $ EPartCall cid [] []
    Just vs → EPartCall cid (map nameToEVar vs) <$> mapM varType vs
 where
  name = cIdentToString cid
expandECall e = mapExpM expandECall e

nameToEVar ∷ String → Exp
nameToEVar s = EVar (CIdent ((-1,-1), s))

expandECallForDecl ∷ ForDecl → LM ForDecl
expandECallForDecl (FDecl d) = FDecl <$> expandECallDecl d
expandECallForDecl (FExp e) = FExp <$> expandECall e

expandECallDecl ∷ Decl → LM Decl
expandECallDecl (Dec qs dp) = Dec qs <$> expandECallDeclPost dp
-- TODO: Structs.
expandECallDecl d = error $ "Not expandable: " ++ show d

expandECallDeclPost ∷ DeclPost → LM DeclPost
expandECallDeclPost dp@(Vars {}) = return dp
expandECallDeclPost (DecAss cid tk e) = DecAss cid tk <$> expandECall e
--expandECallDeclPost (DecFun {}) = undefined -- DecFun -> SFunDecl in typechecker.

-- | Prepends free variables to the end of parameter list.
--   Returns the new list of parameters and a map of all renames.
prependFreeVars ∷ [Param] → [String] → LM ([Param], Map String String)
prependFreeVars ps fs = do
  ps' ← sequence [ do
      t ← varType n
      n' ← renameFreeVar n
      return (varTypeToParam n' t, (n, n'))
    | n ← fs
    ]
  return (map fst ps' ++ ps, fromList (map snd ps'))

renameFreeVar ∷ String → LM String
renameFreeVar s = nextRenameIndex >>= (\i → return $ printf "_f%0.3i%s" i s)

-- | Prepends free variables types to a functions return type.
prependReturnTypes ∷ Type → [String] → LM Type
prependReturnTypes (TFun t ps) fs = do
  ts ← mapM varType fs
  return $ TFun t (ts ++ ps)
--appendReturnTypes t _ = error $ "only functions are supported " ++ show t
-- }}}
-- Function lifting {{{
-- | Lifts all inner functions to the global scope.
liftFuns ∷ LM ()
liftFuns = do
  funs ← sourceFunctions
  mapM (liftInnerFuns . snd) (toList funs) >>= mapM_ updateSourceFunction
 where
  liftInnerFuns ∷ T.Function → LM T.Function
  liftInnerFuns f = do
    setCurrentFile (fst $ T.functionLocation f)
    sequence_ [ addVarType (paramToString p) ((qualsToType . paramToQuals) p) | p ← T.parameters f ]
    stms' ← funLifter (T.statements f)
    return $ f { T.statements = stms' }

-- | Lifts all functions from a list of Stm
funLifter ∷ [Stm] → LM [Stm]
funLifter [] = return []
funLifter (SBlock ss:rest) = do
  ss' ← funLifter ss
  rest' ← funLifter rest
  case ss' of
    [] → do
      mapM_ (unusedFunDecl . fromJust) ((filter isJust . map findSFunDecl) ss)
      return rest'
    _  → return $ SBlock ss':rest'
funLifter (s:ss) = do
  s' ← liftSFunDecl s
  ss' ← funLifter ss
  case s' of
    Just stm → return (stm:ss')
    Nothing  → return ss'

unusedFunDecl ∷ Stm → LM ()
unusedFunDecl stm =
  warning $ printf "Unused function declaration of %s at line %s and column %s"
    (sFunDeclToName stm)
    (show line)
    (show col)
 where
  (line, col) = sFunDeclToPos stm
  sFunDeclToPos ∷ Stm → Position
  sFunDeclToPos (SFunDecl cid _ _ _ _) = cIdentToPos cid
  sFunDeclToPos s = maybe (0,0) sFunDeclToPos (findSFunDecl s)
  sFunDeclToName ∷ Stm → String
  sFunDeclToName (SFunDecl cid _ _ _ _) = cIdentToString cid
  sFunDeclToName s = maybe "N/A" sFunDeclToName (findSFunDecl s)

findSFunDecl ∷ Stm → Maybe Stm
findSFunDecl (SBlock ss) =
  case map findSFunDecl ss of
    []  → Nothing
    ss' → head $ filter isJust ss'
findSFunDecl (SWhile _ _ s) = findSFunDecl s
findSFunDecl (SDoWhile _ s _ _) = findSFunDecl s
findSFunDecl (SFor _ _ _ _ s) = findSFunDecl s
findSFunDecl (SIf _ _ s) = findSFunDecl s
findSFunDecl (SIfElse _ _ st _ sf) =
  case map findSFunDecl [st,sf] of
    []  → Nothing
    ss' → head $ filter isJust ss'
findSFunDecl (SType _ s) = findSFunDecl s
findSFunDecl s@(SFunDecl {}) = Just s


liftStm ∷ Stm → (Stm → Stm) → LM (Maybe Stm)
liftStm s f = do
  s' ← liftSFunDecl s
  when (isNothing s') $ unusedFunDecl s
  return $ maybe Nothing (return . f) s'

liftSFunDecl ∷ Stm → LM (Maybe Stm)
liftSFunDecl (SBlock stms) = do
  stms' ← liftM catMaybes $ mapM liftSFunDecl stms
  case stms' of
    [] → return Nothing
    _  → return $ Just $ SBlock stms'
liftSFunDecl (SWhile tk e s) = liftStm s (SWhile tk e)
liftSFunDecl (SDoWhile tkd s tkw e) = liftStm s (\s' → SDoWhile tkd s' tkw e)
liftSFunDecl (SFor tk fd econs eloop s) = liftStm s (SFor tk fd econs eloop)
liftSFunDecl (SIf tk e s) = liftStm s (SIf tk e)
liftSFunDecl (SIfElse tki e strue tke sfalse) = do
  strue' ← liftSFunDecl strue
  when (isNothing strue') $ unusedFunDecl strue
  sfalse' ← liftSFunDecl sfalse
  when (isNothing sfalse') $ unusedFunDecl sfalse
  case sfalse' of
    Nothing →
      case strue' of
        Nothing → return Nothing -- none
        Just st → return $ Just $ SIf tki e st -- first
    Just sf →
      case strue' of
        Nothing → return $ Just $ negSIf sf -- second
        Just st → return $ Just $ SIfElse tki e st tke sf -- both
 where
  negSIf = SIf tki (ENegSign (TkNegSign ((0,0),"!")) e)
liftSFunDecl (SType t s) = liftStm s (SType t)
liftSFunDecl (SFunDecl cid (TFun rt _) px ps stms) = do
  let pixelMode = px == ITrue
  -- lift inner inner functions first.
  stms' ← funLifter stms
  -- create a new top level function...
  file ← gets currentFile
  addSourceFunction $ mkFun file (cIdentToString cid) rt pixelMode (map (paramToVar file) ps) ps stms'
  return Nothing
liftSFunDecl s = return $ Just s

mkFun ∷ String → String → Type → Bool → [T.Variable] → [Param] → [Stm] → T.Function
mkFun f name rt px vs ps stms =
  T.Function {
    T.functionName = name,
    T.functionLocation = (f, (-1,-1)),
    T.retType = rt,
    T.pixelwise = px,
    T.paramVars = vs,
    T.parameters = ps,
    T.statements = stms,
    T.alias = ""
  }
-- }}}

-- vi:fdm=marker
