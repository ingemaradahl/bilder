{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker where

import Control.Monad.Trans.State
import Control.Monad.Error

import Data.Traversable as Traverse
import Data.Tree
import Data.Map hiding (map)

import Compiler hiding (Environment, Env, options, buildEnv)
import FrontEnd.AbsGrammar
import CompilerError

typeError ∷ Position → FilePath → String → TCM ()
typeError p f s = throwError $ TypeError p f s

-- | Type of functions, where first value is return type, second is arguments
type Function = (Type, [Type])

returnType ∷ Function → Type
returnType = fst

arguments ∷ Function → [Type]
arguments = snd

-- | Environment during type checking
data Environment = Env {
  functions ∷ Map String Function,
  variables ∷ [Map CIdent Type],
  options   ∷ Options,
  currentFile ∷ FilePath
}

buildEnv ∷ Options → Environment
buildEnv opts = Env {
  functions = empty,
  variables = [],
  options = opts,
  currentFile = ""
}

addFunction ∷ CIdent → Function → TCM ()
addFunction (CIdent (pos, name)) types = do
  funs ← gets functions
  file ← gets currentFile
  if member name funs
    then typeError pos file $ "function '" ++ name ++ "' already defined"
    else modify (\st → st { functions = insert name types funs })

updateFile ∷ FilePath → TCM ()
updateFile f = modify (\st → st { currentFile = f })

type TCM a = StateT Environment CError a

-- | Typechecks the given abstract source and annotates the syntax tree
typeCheck ∷ Options → Tree (FilePath, AbsTree) → CError (Tree (FilePath, AbsTree))
typeCheck opts tree = evalStateT (checkTree tree) (buildEnv opts)

checkTree ∷ Tree (FilePath, AbsTree) → TCM (Tree (FilePath, AbsTree))
checkTree tree = do
  Traverse.mapM addFunctions tree
  return tree


paramType ∷ Param → Type
paramType (ConstParamDec t _) = t
paramType (ConstParamDefault t _  _) = t
paramType (ParamDec t _) = t
paramType (ParamDefault t _ _) = t

addFunctions ∷ (FilePath, AbsTree) → TCM ()
addFunctions (filename, AbsTree tree) = do
  updateFile filename
  sequence_ [ addFunction name (ret, map paramType types) | Function ret name types _ ← tree ]
