{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module TypeChecker where

import Prelude hiding (lookup)

import Control.Monad.Trans.State
import Control.Monad.Error

import Data.Traversable as Traverse
import Data.Tree
import Data.Map hiding (map)
import Data.List (intercalate)

import Text.Printf

import Compiler hiding (Environment, Env, options, buildEnv)
import FrontEnd.AbsGrammar
import CompilerError

-- | Throw a type error
typeError ∷ Position → FilePath → String → TCM ()
typeError p f s = throwError $ TypeError p f s

-- | Type of functions, where first value is return type, second is arguments
type Function = (Type, [Type])

returnType ∷ Function → Type
returnType = fst

arguments ∷ Function → [Type]
arguments = snd

showFunction ∷ Function → String
showFunction (ret, args) = intercalate " -> " $ map show (args ++ [ret])

-- | Environment during type checking
data Environment = Env {
  functions ∷ Map String [Function],
  variables ∷ [Map CIdent Type],
  options   ∷ Options,
  currentFile ∷ FilePath
}
 deriving (Show)

-- | Creates an empty environment based on a set of options
buildEnv ∷ Options → Environment
buildEnv opts = Env {
  functions = empty,
  variables = [],
  options = opts,
  currentFile = ""
}

-- | Adds a function to the environment, and makes sure there are no duplicates
addFunction ∷ CIdent → Function → TCM ()
addFunction (CIdent (pos, name)) types = do
  funs ← liftM (lookup name) (gets functions)
  file ← gets currentFile
  case funs of
    Just fs →
      if types `elem` fs
        then typeError pos file $ printf "function '%s' with type %s already defined" name (showFunction types)
        else modify (\st → st { functions = adjust (types:) name (functions st) })
    Nothing → modify (\st → st { functions = insert name [types] (functions st)})

-- | Sets which file is currently checked
updateFile ∷ FilePath → TCM ()
updateFile f = modify (\st → st { currentFile = f })

-- | TypeCheckerMonad - Wrapping up a state around CError
type TCM a = StateT Environment CError a

-- | Only used during interpretation runs
instance (Show a) => Show (TCM a) where
  show s = show $ runStateT s Env { functions = empty, variables = [], options = Options { inputFile = "DEBUG" }, currentFile = ""}

-- | Typechecks the given abstract source and annotates the syntax tree
typeCheck ∷ Options → Tree (FilePath, AbsTree) → CError (Tree (FilePath, AbsTree))
typeCheck opts tree = evalStateT (checkTree tree) (buildEnv opts)

checkTree ∷ Tree (FilePath, AbsTree) → TCM (Tree (FilePath, AbsTree))
checkTree tree = do
  Traverse.mapM addFunctions tree
  return tree

-- | TODO: Move this to some other module
paramType ∷ Param → Type
paramType (ConstParamDec t _) = t
paramType (ConstParamDefault t _  _) = t
paramType (ParamDec t _) = t
paramType (ParamDefault t _ _) = t

-- | Adds function type definitions to the state
addFunctions ∷ (FilePath, AbsTree) → TCM ()
addFunctions (filename, AbsTree tree) = do
  updateFile filename
  sequence_ [ addFunction name (ret, map paramType types) | Function ret name types _ ← tree ]
