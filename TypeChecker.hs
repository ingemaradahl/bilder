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

import TypeChecker.Environment
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
type Function = (FilePath, Position, Type, [Type])
type Variable = (FilePath, Position, Type)

exists ∷ Function → [Function] → Bool
(_,_,t,ts) `exists` fs = any (\(_,_,t',ts') → (t,ts) == (t',ts')) fs

emptyScope ∷ Scope
emptyScope = Scope { functions = empty, variables = empty }

-- | Add a function to a scope
saddFunction ∷ String → Function → Scope → Scope
saddFunction n t s = s { functions = fs' }
 where
  fs = functions s
  fs' = if member n fs
          then adjust (t:) n fs
          else insert n [t] fs

saddVariable ∷ String → Variable → Scope → Scope
saddVariable n v s = s { variables = vs }
 where
  vs = insert n v $ variables s

-- | Creates an empty environment based on a set of options
buildEnv ∷ Options → Environment
buildEnv opts = Env {
  scopes = [Scope empty empty],
  options = opts,
  currentFile = ""
}

-- | Adds a function to the environment, and makes sure there are no duplicates
addFunction ∷ String → Function → TCM ()
addFunction name fun@(_,pos,_,_) = do
  (s:ss) ← gets scopes
  let a = lookup name (functions s) >>= (\fs → if fun `exists` fs then Just fs; else Nothing)
  case a of
    Just _ → typeError pos $ printf "function '%s' with type %s already defined" name (showFunctionType fun)
    Nothing → modify (\st → st { scopes = saddFunction name fun s : ss })

-- | Adds a variable to the current scope, making sure there are no duplicates
addVariable ∷ String → Variable → TCM ()
addVariable name v@(_,pos,_) = do
  (s:ss) ← gets scopes
  case lookup name (variables s) of
    Just _ → typeError pos $ printf "variable '%s' already defined" name
    Nothing → modify (\st → st { scopes = saddVariable name v s : ss } )

lookupVar ∷ String → TCM Variable
lookupVar name = gets scopes >>= lookupVar'
 where
  lookupVar' ∷ [Scope] → TCM Variable
  lookupVar'     [] = fail "apa" -- TODO
  lookupVar' (s:ss) = case lookup name (variables s) of
    Just v  → return v
    Nothing → lookupVar' ss


-- | Sets which file is currently checked
updateFile ∷ FilePath → TCM ()
updateFile f = modify (\st → st { currentFile = f })

-- | TypeCheckerMonad - Wrapping up a state around CError
type TCM a = StateT Environment CError a

-- | Only used during interpretation runs
instance (Show a) => Show (TCM a) where
  show res = case runStateT res $ buildEnv Options { inputFile = "DEBUG" } of
    Pass (v,s) → printf "%s\n%s" (show v) (show s)
    Fail e → show e

-- | Typechecks the given abstract source and annotates the syntax tree
typeCheck ∷ Options → Tree (FilePath, AbsTree) → CError (Tree (FilePath, AbsTree))
typeCheck opts tree = evalStateT (checkTree tree) (buildEnv opts)

checkTree ∷ Tree (FilePath, AbsTree) → TCM (Tree (FilePath, AbsTree))
checkTree tree = do
  Traverse.mapM addFunctions tree
  --Traverse.mapM addStructs tree
  Traverse.mapM checkFunctions tree

  return tree

-- | TODO: Move this to some other module
paramType ∷ Param → Type
paramType (ConstParamDec _ t i) = idToType i t
paramType (ParamDec t i) = idToType i t
paramType (ParamDefault t i _) = idToType i t

idToType ∷ Id → Type → Type
idToType (IdEmptyArray _) t = TArray t
idToType (IdArray _ _) t = TArray t
idToType _ t = t

-- | Adds function type definitions to the state
addFunctions ∷ (FilePath, AbsTree) → TCM ()
addFunctions (filename, AbsTree tree) = do
  updateFile filename
  sequence_ [ addFunction name (filename, pos, ret, map paramType types) | Function ret (CIdent (pos,name)) types _ ← tree ]

checkFunctions ∷ (FilePath, AbsTree) → TCM ()
checkFunctions (filename, AbsTree tree) = do
  updateFile filename
  sequence_ [ checkFunction name ret args stms | Function ret name args stms ← tree ]
  return ()

example ∷ AbsTree
example = AbsTree [Import (TkImport ((1,1),"import")) "struct.fl", Function TColor (CIdent ((3,7),"main")) [] [SDecl (Dec TInt (DefaultVars [Ident (CIdent ((5,7),"b"))] (EInt 0))),SFor (TkFor ((6,3),"for")) [FDecl (Dec TInt (DefaultVars [Ident (CIdent ((6,12),"a"))] (EInt 0)))] [ELt (EVar (Ident (CIdent ((6,17),"i")))) (EInt 10)] [EPostInc (EVar (Ident (CIdent ((6,23),"i"))))] (SBlock [SExp (EPostInc (EVar (Ident (CIdent ((9,5),"b")))))]),SReturn (TkReturn ((11,3),"return")) (EVar (Ident (CIdent ((11,10),"b"))))]]

checkFunction ∷ CIdent → Type → [Param] → [Stm] → TCM ()
checkFunction (CIdent (pos, name)) ret args stms = do
  return ()
