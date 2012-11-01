{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Lifter where

import Data.Map (Map, toList, insert, lookup, empty, adjust)
import Data.Maybe (fromJust, isNothing, isJust)
import Control.Monad.State
import Control.Applicative ((<$>), (<*>), pure)

import FrontEnd.AbsGrammar

import CompilerError
import Compiler.Utils
import CompilerTypes

import Text.Printf

import TypeChecker.Utils (cIdentToString, cIdentToPos, paramToString, paramToQuals)
import TypeChecker.Types (Source, functions, typedefs, variables)
import qualified TypeChecker.Types as T

-- Lifter monad - keeps source and lifting environment in State with CError
type LM a = StateT Environment CError a

data Environment = Environment {
  source ∷ Source,
  varTypes ∷ Map String Type,
  callExpansions ∷ Map String [String],
  currentFile ∷ String,
  warnings ∷ [String]
}
 deriving (Show)

-- Environment and state helpers {{{
buildEnv ∷ Source → Environment
buildEnv s = Environment {
    source = s,
    varTypes = empty,
    callExpansions = empty,
    currentFile = "",
    warnings = []
  }

warning ∷ String → LM ()
warning s = modify (\st → st { warnings = warnings st ++ [s] })

sourceFunctions ∷ LM (Map String T.Function)
sourceFunctions = gets source >>= \s → return $ functions s

sourceTypedefs ∷ LM (Map String T.Typedef)
sourceTypedefs = gets source >>= \s → return $ typedefs s

sourceVariables ∷ LM (Map String T.Variable)
sourceVariables = gets source >>= \s → return $ variables s

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

-- | Returns the variables type (should always exist, hence no need for Maybe)
varType ∷ String → LM Type
varType n = do
  vt ← gets varTypes
  return $ fromJust $ Data.Map.lookup n vt

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
-- TODO: lambdaLift ∷ Source → (Warnings, Source)
lambdaLift ∷ LM ()
lambdaLift = do
  -- lift free variables.
  funs ← sourceFunctions
  mapM (liftFunVars . snd) (toList funs) >>= mapM updateSourceFunction

  -- lift inner functions.
  liftFuns

-- Variable lifting {{{
liftFunVars ∷ T.Function → LM T.Function
liftFunVars f = do
  clearVarTypes
  stms ← mapM liftInnerFunVars (T.statements f)
  return f { T.statements = stms }

-- | Iterates a list of statements and lifts all inner functions variables.
liftInnerFunVars ∷ Stm → LM Stm
liftInnerFunVars (SType t stm) = SType t <$> liftInnerFunVars stm
liftInnerFunVars (SDecl d) = do
  -- Add all declared variables.
  let ts = declToTypeAndName d
  mapM_ (uncurry $ flip addVarType) ts
  return $ SDecl d
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
liftInnerFunVars (SFunDecl cid rt ps stms) = do
  sequence_ [ addVarType (paramToString p) ((qualsToType . paramToQuals) p) | p ← ps ]
  stms' ← mapM liftInnerFunVars stms

  -- Calculate free variables in the function.
  vars ← sourceVariables
  let globals = map fst $ toList vars
  let frees = freeFunctionVars globals (cIdentToString cid, ps, stms')

  -- Add the free variables as parameters and return type.
  ps' ← appendFreeVars ps frees
  rt' ← appendReturnTypes rt frees

  -- Remember the expansion so that all calls can be expanded.
  addCallExpansion (cIdentToString cid) frees

  return $ SFunDecl cid rt' ps' stms'
liftInnerFunVars x = return x -- The rest: SBreak, SContinue, SDiscard

-- | Expands all ECall's to a function in given statements with the added free variables.
expandECall ∷ Exp → LM Exp
expandECall (ECall cid es) = do
  eps ← gets callExpansions
  es' ← mapM (mapExpM expandECall) es
  case Data.Map.lookup (cIdentToString cid) eps of
    Nothing → return $ ECall cid es'
    Just vs  → return $ ECall cid (es' ++ map nameToVar vs)
 where
  nameToVar ∷ String → Exp
  nameToVar s = EVar (CIdent ((-1,-1), s))
expandECall e = mapExpM expandECall e

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

-- | Appends free variables to the end of parameter list.
appendFreeVars ∷ [Param] → [String] → LM [Param]
appendFreeVars ps fs = do
  ps' ← sequence [ varType n >>= \t → return $ varTypeToParam n t | n ← fs ]
  return $ ps ++ ps'

-- | Appends free variables types to a functions return type.
appendReturnTypes ∷ Type → [String] → LM Type
appendReturnTypes (TFun t ps) fs = do
  ts ← mapM varType fs
  return $ TFun t (ps ++ ts)
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

sFunDeclToPos ∷ Stm → Position
sFunDeclToPos (SFunDecl cid _ _ _) = cIdentToPos cid
sFunDeclToPos s = maybe (0,0) sFunDeclToPos (findSFunDecl s)

sFunDeclToName ∷ Stm → String
sFunDeclToName (SFunDecl cid _ _ _) = cIdentToString cid
sFunDeclToName s = maybe "N/A" sFunDeclToName (findSFunDecl s)

liftSFunDecl ∷ Stm → LM (Maybe Stm)
liftSFunDecl (SWhile tk e s) = do
  s' ← liftSFunDecl s
  when (isNothing s') $ unusedFunDecl s
  return $ maybe Nothing (return . SWhile tk e) s'
liftSFunDecl (SDoWhile tkd s tkw e) = do
  s' ← liftSFunDecl s
  when (isNothing s') $ unusedFunDecl s
  return $ maybe Nothing (\stm → return $ SDoWhile tkd stm tkw e) s'
liftSFunDecl (SFor tk fd econs eloop s) = do
  s' ← liftSFunDecl s
  when (isNothing s') $ unusedFunDecl s
  return $ maybe Nothing (return . SFor tk fd econs eloop) s'
liftSFunDecl (SIf tk e s) = do
  s' ← liftSFunDecl s
  when (isNothing s') $ unusedFunDecl s
  return $ maybe Nothing (return . SIf tk e) s'
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
liftSFunDecl (SType t s) = do
  s' ← liftSFunDecl s
  when (isNothing s') $ unusedFunDecl s
  return $ maybe Nothing (return . SType t) s'
liftSFunDecl (SFunDecl cid rt ps stms) = do
  -- lift inner inner functions first.
  stms' ← funLifter stms
  -- create a new top level function...
  file ← gets currentFile
  addSourceFunction $ mkFun file (cIdentToString cid) rt (map (paramToVar file) ps) ps stms'
  return Nothing
liftSFunDecl s = return $ Just s

mkFun ∷ String → String → Type → [T.Variable] → [Param] → [Stm] → T.Function
mkFun f name rt vs ps stms =
  T.Function {
    T.functionName = name,
    T.functionLocation = (f, (-1,-1)),
    T.retType = rt,
    T.paramVars = vs,
    T.parameters = ps,
    T.statements = stms,
    T.alias = ""
  }
-- }}}

-- vi:fdm=marker
