{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.TCM.Utils where

import TypeChecker.TCM
import TypeChecker.TCM.Errors
import TypeChecker.Environment as Env
import qualified TypeChecker.Scope as Scope
import TypeChecker.Types as TC
import TypeChecker.Utils

import CompilerTypes

import FrontEnd.AbsGrammar as Abs

import Control.Monad.Trans.State
import Control.Monad.Error

import Prelude hiding (lookup)
import Data.Map

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
  ts ← gets typedefs
  case lookup name ts of
    Just t' → unless (t == t') (typedefError tid t' t)
    Nothing → modify (\st → st { typedefs = insert name t ts })
 where
  t = uncurryType typeFunc
  name = typeIdentToString tid

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


tcFun ∷ Toplevel → TCM Function
tcFun (Abs.Function t cident params stms) = do
  updateFile $ cIdentToString cident
  params' ← mapM paramToVar params
  file ← gets currentFile
  return TC.Function {
    functionName = cIdentToString cident,
    functionLocation = (file, cIdentToPos cident),
    retType = t,
    paramVars = params',
    parameters = params,
    statements = stms
  }
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
  varTyp ← paramType p
  file ← gets currentFile
  return $ Variable (paramToString p) (file, paramToPos p) varTyp

{-qualType ∷ [Qualifier] → TCM Type-}
{-qualType qs | length qs /= length (nub qs) = fail "TODO01"-}
