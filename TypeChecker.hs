{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module TypeChecker where

import Prelude hiding (lookup)

import Control.Monad.Trans.State
import Control.Monad.Error hiding (mapM)

import Data.Traversable as Traverse
import Data.Tree
import Data.Map hiding (map, null)

import Text.Printf

import TypeChecker.Environment
import TypeChecker.Types
import TypeChecker.Utils

import Compiler hiding (Environment, Env, options, buildEnv)
import FrontEnd.AbsGrammar
import CompilerError

-- | TypeCheckerMonad - Wrapping up a state around CError
type TCM a = StateT Environment CError a

-- | Only used during interpretation runs
instance (Show a) => Show (TCM a) where
  show res = case runStateT res $ buildEnv Options { inputFile = "DEBUG" } of
    Pass (v,s) → printf "%s\n%s" (show v) (show s)
    Fail e → show e


-- | Throw a type error
typeError ∷ Position → String → TCM ()
typeError = absError TypeError

compileError ∷ Position → String → TCM ()
compileError = absError CompileError

absError ∷ (Position → FilePath → String → CompilerError) → Position → String → TCM ()
absError e p s = do
  f ← gets currentFile
  throwError $ e p f s


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

addVariable' ∷ String → Position → Type → TCM ()
addVariable' n p t = do
  file ← gets currentFile
  addVariable n (file, p, t)

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

-- | Typechecks the given abstract source and annotates the syntax tree
typeCheck ∷ Options → Tree (FilePath, AbsTree) → CError (Tree (FilePath, AbsTree))
typeCheck opts tree = evalStateT (checkTree tree) (buildEnv opts)

checkTree ∷ Tree (FilePath, AbsTree) → TCM (Tree (FilePath, AbsTree))
checkTree tree = do
  Traverse.mapM addFunctions tree
  --Traverse.mapM addStructs tree
  Traverse.mapM checkFunctions tree

  return tree

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
example = AbsTree [Import (TkImport ((1,1),"import")) "bools.fl",Import (TkImport ((2,1),"import")) "inner/const.fl",Struct (TkStruct ((4,1),"struct")) (CIdent ((4,8),"First")) [SVDecl (Dec TColor (OnlyVars [Ident (CIdent ((5,15),"color"))])),SVDecl (Dec TVec2 (OnlyVars [Ident (CIdent ((6,14),"coordinates"))]))],StructDecl (TkStruct ((9,1),"struct")) (CIdent ((9,8),"Second")) [SVDecl (Dec TVec2 (OnlyVars [Ident (CIdent ((10,14),"coordinates"))]))] (Ident (CIdent ((11,3),"second"))),StructDecl (TkStruct ((13,1),"struct")) (CIdent ((13,8),"Third")) [SVDecl (Dec TVec2 (OnlyVars [Ident (CIdent ((14,14),"coordinates"))]))] (IdArray (CIdent ((15,3),"third")) (EInt 5)),Function TColor (CIdent ((17,7),"main")) [ParamDec TInt (Ident (CIdent ((17,16),"x"))),ParamDec TInt (Ident (CIdent ((17,23),"y")))] [SDecl (DecStruct (Ident (CIdent ((19,9),"First"))) (OnlyVars [Ident (CIdent ((19,15),"a"))])),SDecl (DecStruct (Ident (CIdent ((20,9),"First"))) (DefaultVars [Ident (CIdent ((20,15),"b"))] (ECall (Ident (CIdent ((20,19),"First"))) [ETypeCall TColor [EFloat (CFloat "1.0")] OnlyCall,ETypeCall TVec2 [EFloat (CFloat "1.0"),EFloat (CFloat "2.0")] OnlyCall]))),SExp (EAss (EMember (EVar (Ident (CIdent ((22,9),"second")))) (EVar (Ident (CIdent ((22,16),"coordinates"))))) (ETypeCall TVec2 [EFloat (CFloat "1.0"),EFloat (CFloat "2.0")] OnlyCall)),SReturn (TkReturn ((24,9),"return")) (EMember (EVar (Ident (CIdent ((24,16),"b")))) (EVar (Ident (CIdent ((24,18),"color")))))]]

checkFunction' ∷ Toplevel → TCM ()
checkFunction' (Function t i ps sts) = checkFunction i t ps sts

checkFunction ∷ CIdent → Type → [Param] → [Stm] → TCM ()
checkFunction (CIdent (pos, name)) ret args stms = do
  mapM_ (\p → addVariable' (paramToString p) (paramToPos p) (paramType p)) args
  return ()
