{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.TCM.Utils where

import TypeChecker.TCM
import TypeChecker.TCM.Errors
import TypeChecker.Environment as Env
import qualified TypeChecker.Scope as Scope
import TypeChecker.Types as TC hiding (functions, typedefs, filename, variables)
import qualified TypeChecker.Types as Blob (functions, typedefs, filename, variables)
import TypeChecker.Utils

import CompilerTypes

import FrontEnd.AbsGrammar as Abs

import Control.Monad.Trans.State
import Control.Monad.Error

import Prelude hiding (lookup)
import Data.Map
import Data.Tree

import Text.Printf

-- | Adds a function to the environment, and makes sure there are no duplicates
addFunction ∷ Function → TCM ()
addFunction fun = do
  (s:ss) ← gets scopes
  let a = lookup name (Scope.functions s) >>= (\fs → if fun `elem` fs then Just fs; else Nothing)
  case a of
    Just (f:_) → functionDefinedError f fun
    Nothing → modify (\st → st { scopes = Scope.addFunction fun s : ss })
 where
  name = ident fun ∷ String

lookupFunction ∷ String → TCM [Function]
lookupFunction f = do
  scs ← gets scopes
  maybe (return []) return $ lookupFun f scs
 where
  lookupFun ∷ String → [Scope.Scope] → Maybe [Function]
  lookupFun fun (s:ss) =
    case lookup fun (Scope.functions s) of
      Just fs → Just fs
      Nothing → lookupFun f ss
  lookupFun _ [] = Nothing

-- | Adds a type definition to the environment
addTypedef ∷ TypeIdent → Type → TCM ()
addTypedef tid typeFunc = do
  scs ← gets scopes
  defs ← lookupTypedef' name
  case defs of
    Just t' → unless (t == t') (typedefError tid t' t)
    Nothing → modify (\st → st { scopes =  Scope.addTypedef name t (head scs):tail scs })
 where
  t = uncurryType typeFunc
  name = typeIdentToString tid

lookupTypedef ∷ TypeIdent → TCM Type
lookupTypedef tid = do
  def ← lookupTypedef' n
  case def of
    Just t → return t
    Nothing → typedefNotFoundError tid
 where
  n = typeIdentToString tid

lookupTypedef' ∷ String → TCM (Maybe Type)
lookupTypedef' n = liftM (Scope.lookupTypedef n) $ gets scopes


-- | Adds a variable to the current scope, making sure there are no duplicates
addVariable ∷ Variable → TCM ()
addVariable var = do
  (s:ss) ← gets scopes
  case lookup name (Scope.variables s) of
    Just _ → typeError pos $ printf "variable '%s' already defined" name
    Nothing → modify (\st → st { scopes = Scope.addVariable var s : ss } )
 where
  pos = position var ∷ Position
  name = ident var ∷ String

-- | Add a variable to the current scope, using currentFile from the state
addVariable' ∷ String → Position → Type → TCM ()
addVariable' n p t = do
  file ← gets currentFile
  addVariable $ Variable n (file,p) t

-- | Lookup a variable denoted by it's name. May throw error
lookupVar ∷ String → TCM Variable
lookupVar name = gets scopes >>= lookupVar'
 where
  lookupVar' ∷ [Scope.Scope] → TCM Variable
  lookupVar'     [] = typeError (-1,-1) $ "Cannot find variable " ++ name
  lookupVar' (s:ss) = case lookup name (Scope.variables s) of
    Just v  → return v
    Nothing → lookupVar' ss

-- | Sets which file is currently checked
updateFile ∷ FilePath → TCM ()
updateFile f = modify (\st → st { currentFile = f })

-- | Sets which function is currently checked
updateFunction ∷ Function → TCM ()
updateFunction f = modify (\st → st { currentFunction = f })

-- | Pushes a new scope to the environment
pushScope ∷ TCM ()
pushScope = modify Env.pushScope

-- | Pops a scope off the environment
popScope ∷ TCM ()
popScope = modify Env.popScope

clearScope ∷ TCM ()
clearScope = modify (\st → st { scopes = [Scope.emptyScope, Scope.builtInScope] })

newFile ∷ FilePath → TCM ()
newFile f = updateFile f >> clearScope

makeBlob ∷ Map String [Function] → TCM Blob
makeBlob funs = do
  scope ← gets $ head . scopes
  file ← gets currentFile
  return $ Blob file funs (Scope.typedefs scope) (Scope.variables scope)

initScope ∷ [Tree Blob] → TCM ()
initScope = mapM_ (addBlob . rootLabel)
 where
  addBlob ∷ Blob → TCM ()
  addBlob b = do
    mergeFunctions (Blob.functions b)
    mergeVariables (Blob.variables b)
    mergeTypedefs (Blob.typedefs b)

mergeFunctions ∷ Map String [Function] → TCM ()
mergeFunctions funs = sequence_ [ mapM addFunction fs | (_,fs) ← toList funs]

mergeVariables ∷ Map String Variable → TCM ()
mergeVariables vars = mapM_ (addVariable . snd) $ toList vars

mergeTypedefs ∷ Map String Type → TCM ()
mergeTypedefs defs = mapM_ (\(n,t) → addTypedef (TypeIdent ((-1,-1),n)) t ) $ toList defs
{-
 -  scs ← gets scopes
 -  let scope = head scs
 -  let scopeDefs = Scope.typedefs scope
 -  let inters = intersection defs scopeDefs
 -
 -  -- Assure there are no name collisions
 -  -- TODO: Check for same stuff
 -  unless (Data.Map.null inters) $ debugError (show inters) --"LOLAAOEUAOEUAOEU" --undefined -- TODO
 -
 -  let scope' = scope { Scope.typedefs = scopeDefs `union` defs }
 -  modify (\st → st { scopes = scope':tail scs })
 -}


tcFun ∷ Toplevel → TCM Function
tcFun (Abs.Function t cident params stms) = do
  params' ← mapM paramToVar params
  retType' ← filterTDef t
  file ← gets currentFile
  let fun = TC.Function {
    functionName = cIdentToString cident,
    functionLocation = (file, cIdentToPos cident),
    retType = retType',
    paramVars = params',
    parameters = params,
    statements = stms
  }
  updateFunction fun
  return fun
tcFun _ = compileError (-1,-1) "Non-function given as argument to tcFun"

paramExp ∷ Param → TCM Exp
paramExp (ParamDefault _ _ e) = return e
paramExp p = compileError (paramToPos p)
  "Trying to find expression on non-expression parameter declaration"

paramType ∷ Param → TCM Type
paramType p@(ParamDec qs _) = maybe (paramNoTypeGiven p) return (qualType qs)
paramType p@(ParamDefault qs _ _) = maybe (paramNoTypeGiven p) return (qualType qs)

paramToVar ∷ Param → TCM Variable
paramToVar p = do
  varTyp ← paramType p >>= filterTDef
  file ← gets currentFile
  return $ Variable (paramToString p) (file, paramToPos p) varTyp

filterTDef ∷ Type → TCM Type
filterTDef (TDefined tid) = lookupTypedef tid
filterTDef t = return t


{-qualType ∷ [Qualifier] → TCM Type-}
{-qualType qs | length qs /= length (nub qs) = fail "TODO01"-}
