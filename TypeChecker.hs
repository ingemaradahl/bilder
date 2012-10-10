{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module TypeChecker where

import Prelude hiding (lookup)

import Control.Monad.Trans.State
import Control.Monad.Error

import Data.Traversable as Traverse
import Data.Tree
import Data.Map hiding (map, null)
import Data.List (intercalate)

import Text.Printf

import Compiler hiding (Environment, Env, options, buildEnv)
import FrontEnd.AbsGrammar
import CompilerError

-- | Throw a type error
typeError ∷ Position → String → TCM ()
typeError = absError TypeError

compileError ∷ Position → String → TCM ()
compileError = absError CompileError

absError ∷ (Position → FilePath → String → CompilerError) → Position → String → TCM ()
absError e p s = do
  f ← gets currentFile
  throwError $ e p f s

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
  scopes  ∷ [Scope],
  options   ∷ Options,
  currentFile ∷ FilePath
}
 deriving (Show)

data Scope = Scope {
  functions ∷ Map String [Function],
  variables ∷ Map CIdent Type
}
 deriving (Show)

-- | Add a function to a scope
saddFunction ∷ String → Function → Scope → Scope
saddFunction n t s = s { functions = fs' }
 where
  fs = functions s
  fs' = if member n fs
          then adjust (t:) n fs
          else insert n [t] fs

-- | Creates an empty environment based on a set of options
buildEnv ∷ Options → Environment
buildEnv opts = Env {
  scopes = [Scope empty empty],
  options = opts,
  currentFile = ""
}

-- | Adds a function to the environment, and makes sure there are no duplicates
addFunction ∷ CIdent → Function → TCM ()
addFunction (CIdent (pos, name)) types = do
  scs ← gets scopes
  let a = lookup name (functions $ head scs) >>= (\fs → if types `elem` fs then Just fs; else Nothing)
  case a of
    Just _ → typeError pos $ printf "function '%s' with type %s already defined" name (showFunction types)
    Nothing → modify (\st → st { scopes = addToTopScope scs name types })
 where
  addToTopScope ∷ [Scope] → String → Function → [Scope]
  addToTopScope (s:ss) n t = saddFunction n t s:ss

-- | Sets which file is currently checked
updateFile ∷ FilePath → TCM ()
updateFile f = modify (\st → st { currentFile = f })

-- | TypeCheckerMonad - Wrapping up a state around CError
type TCM a = StateT Environment CError a

-- | Only used during interpretation runs
instance (Show a) => Show (TCM a) where
  show s = show $ runStateT s $ buildEnv Options { inputFile = "DEBUG" }

-- | Typechecks the given abstract source and annotates the syntax tree
typeCheck ∷ Options → Tree (FilePath, AbsTree) → CError (Tree (FilePath, AbsTree))
typeCheck opts tree = evalStateT (checkTree tree) (buildEnv opts)

checkTree ∷ Tree (FilePath, AbsTree) → TCM (Tree (FilePath, AbsTree))
checkTree tree = do
  Traverse.mapM addFunctions tree
  Traverse.mapM checkFunctions tree

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

checkFunctions ∷ (FilePath, AbsTree) → TCM ()
checkFunctions (filename, AbsTree tree) = do
  updateFile filename
  sequence_ [ checkFunction name ret args stms | Function ret name args stms ← tree ]
  return ()

example ∷ AbsTree
example = AbsTree [Import (TImport ((1,1),"import")) "struct.fl",Function TColor (CIdent ((3,7),"main")) [] [], Function TColor (CIdent ((3,7),"main")) [] [SDecl (Dec TInt (DefaultVars [Ident (CIdent ((5,7),"b"))] (EInt 0))),SFor (TFor ((6,3),"for")) [FDecl (Dec TInt (DefaultVars [Ident (CIdent ((6,12),"a"))] (EInt 0)))] [ELt (EVar (Ident (CIdent ((6,17),"i")))) (EInt 10)] [EPostInc (EVar (Ident (CIdent ((6,23),"i"))))] (SBlock [SExp (EPostInc (EVar (Ident (CIdent ((9,5),"b")))))]),SReturn (TReturn ((11,3),"return")) (EVar (Ident (CIdent ((11,10),"b"))))]]

checkFunction ∷ CIdent → Type → [Param] → [Stm] → TCM ()
checkFunction (CIdent (pos, name)) ret args stms = do
  return ()
