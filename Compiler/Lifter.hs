{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Lifter where

import Data.Map (Map, toList, fromList, insert, lookup, empty)
import Data.Maybe (fromJust)
import Control.Monad.State
import Control.Applicative ((<$>), (<*>), pure)

import FrontEnd.AbsGrammar
import qualified TypeChecker.Types as T

import CompilerError
import Compiler.Utils

-- TODO: Should this be here?
import TypeChecker.Utils (cIdentToString)

import Control.Arrow (second)

-- TEMPORARY - SHOULD NOT BE HERE {{{
data Source = Source {
  functions ∷ Map String T.Function,
  typedefs ∷ Map String T.Typedef,
  variables ∷ Map String T.Variable
}
 deriving (Show)

blobToSource ∷ T.Blob → Source
blobToSource b = Source {
    functions = fromList $ map (second head) (toList $ T.functions b),
    typedefs = T.typedefs b,
    variables = T.variables b
  }
-- }}}

-- Lifter monad - keeps source and lifting envirornment in State with CError
type LM a = StateT Environment CError a

data Environment = Environment {
  source ∷ Source,
  varTypes ∷ Map String Type,
  callExpansions ∷ Map String [String]
}
 deriving (Show)

-- Environment helpers {{{
buildEnv ∷ Source → Environment
buildEnv s = Environment { source = s, varTypes = empty, callExpansions = empty }

sourceFunctions ∷ LM (Map String T.Function)
sourceFunctions = gets source >>= \s → return $ functions s

sourceTypedefs ∷ LM (Map String T.Typedef)
sourceTypedefs = gets source >>= \s → return $ typedefs s

sourceVariables ∷ LM (Map String T.Variable)
sourceVariables = gets source >>= \s → return $ variables s

modifySource ∷ (Source → Source) → LM ()
modifySource fun = do
  src ← gets source
  modify (\s → s { source = fun src })

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
-- }}}
-- Variable and function lifting {{{
-- | Lifts all free variables in all inner functions and globifies the inner functions.
lambdaLift ∷ LM ()
lambdaLift = do
  funs ← sourceFunctions
  funs' ← sequence [ liftFunVars f >>= (\v → return (k,v)) | (k,f) ← toList funs ]
  modifySource (\s → s { functions = fromList funs' })

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
  -- Store this functions varTypes, lift inner inner functions and restore.
  -- FIXME: Should varTypes be cleared?
  --vts ← clearVarTypes
  stms' ← mapM liftInnerFunVars stms
 --setVarTypes vts

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
  nameToVar s = EVar (CIdent ((0,0), s))
expandECall e = mapExpM expandECall e

expandECallForDecl ∷ ForDecl → LM ForDecl
expandECallForDecl (FDecl d) = FDecl <$> expandECallDecl d
expandECallForDecl (FExp e) = FExp <$> expandECall e

expandECallDecl ∷ Decl → LM Decl
expandECallDecl (Dec qs dp) = Dec qs <$> expandECallDeclPost dp
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

-- | Appends free variables types a functions return type.
appendReturnTypes ∷ Type → [String] → LM Type
appendReturnTypes (TFun t ps) fs = do
  ts ← mapM varType fs
  return $ TFun t (ps ++ ts)
--appendReturnTypes t _ = error $ "only functions are supported " ++ show t
-- }}}
