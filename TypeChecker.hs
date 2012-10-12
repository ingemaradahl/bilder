{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker where

import Prelude hiding (lookup)

import Control.Monad
import Control.Monad.Trans.State

import Data.Traversable as Traverse
import Data.Tree

import TypeChecker.TCM
import TypeChecker.TCM.Errors
import TypeChecker.TCM.Utils

import TypeChecker.Environment hiding (pushScope, popScope)
import TypeChecker.Types
import TypeChecker.Utils
import TypeChecker.Conversion

import Compiler hiding (Environment, Env, options, buildEnv)
import FrontEnd.AbsGrammar hiding (Function,Struct)
import qualified FrontEnd.AbsGrammar as Abs (Toplevel (Function), Toplevel (Struct))
import CompilerError

-- | Typechecks the given abstract source and annotates the syntax tree
typeCheck ∷ Options → Tree (FilePath, AbsTree) → CError (Tree (FilePath, AbsTree))
typeCheck opts tree = evalStateT (checkTree tree) (buildEnv opts)

checkTree ∷ Tree (FilePath, AbsTree) → TCM (Tree (FilePath, AbsTree))
checkTree tree = do
  Traverse.mapM addFunctions tree
  --Traverse.mapM addStructs tree
  --Traverse.mapM checkFunctions tree

  return tree

-- | Adds function type definitions to the state
addFunctions ∷ (FilePath, AbsTree) → TCM ()
addFunctions (filename, AbsTree tree) = do
  updateFile filename
  sequence_ [ addFunction (tcFun filename f) | f@(Abs.Function { }) ← tree ]

--checkFunctions ∷ (FilePath, AbsTree) → TCM ()
--checkFunctions (filename, AbsTree tree) = do
--  updateFile filename
--  sequence_ [ checkFunction name ret args stms | Function ret name args stms ← tree ]
--  return ()

example ∷ AbsTree
example = AbsTree [Import (TkImport ((1,1),"import")) "bools.fl",Import (TkImport ((2,1),"import")) "inner/const.fl",Abs.Struct (TkStruct ((4,1),"struct")) (CIdent ((4,8),"First")) [SVDecl (Dec TColor (OnlyVars [Ident (CIdent ((5,15),"color"))])),SVDecl (Dec TVec2 (OnlyVars [Ident (CIdent ((6,14),"coordinates"))]))],StructDecl (TkStruct ((9,1),"struct")) (CIdent ((9,8),"Second")) [SVDecl (Dec TVec2 (OnlyVars [Ident (CIdent ((10,14),"coordinates"))]))] (Ident (CIdent ((11,3),"second"))),StructDecl (TkStruct ((13,1),"struct")) (CIdent ((13,8),"Third")) [SVDecl (Dec TVec2 (OnlyVars [Ident (CIdent ((14,14),"coordinates"))]))] (IdArray (CIdent ((15,3),"third")) (EInt 5)),Abs.Function TColor (CIdent ((17,7),"main")) [ParamDec TInt (Ident (CIdent ((17,16),"x"))),ParamDec TInt (Ident (CIdent ((17,23),"y")))] [SDecl (DecStruct (Ident (CIdent ((19,9),"First"))) (OnlyVars [Ident (CIdent ((19,15),"a"))])),SDecl (DecStruct (Ident (CIdent ((20,9),"First"))) (DefaultVars [Ident (CIdent ((20,15),"b"))] (ECall (Ident (CIdent ((20,19),"First"))) [ETypeCall TColor [EFloat (CFloat "1.0")] OnlyCall,ETypeCall TVec2 [EFloat (CFloat "1.0"),EFloat (CFloat "2.0")] OnlyCall]))),SExp (EAss (EMember (EVar (Ident (CIdent ((22,9),"second")))) (EVar (Ident (CIdent ((22,16),"coordinates"))))) (ETypeCall TVec2 [EFloat (CFloat "1.0"),EFloat (CFloat "2.0")] OnlyCall)),SReturn (TkReturn ((24,9),"return")) (EMember (EVar (Ident (CIdent ((24,16),"b")))) (EVar (Ident (CIdent ((24,18),"color")))))]]

--checkFunction' ∷ Toplevel → TCM ()
--checkFunction' (Function t i ps sts) = checkFunction i t ps sts

--checkFunction ∷ CIdent → Type → [Param] → [Stm] → TCM ()
--checkFunction (CIdent (pos, name)) ret args stms = do
--  pushScope
--  updateFunction name
--  mapM_ (\p → addVariable' (paramToString p) (paramToPos p) (paramType p)) args
--
--  -- Check that eventual parameter assignments are correct
--  mapM_ checkParam args
--  --popScope
--  return ()

checkParam ∷ Param → TCM ()
checkParam p = unless (checkParam' p) error
 where
  checkParam' ∷ Param → Bool
  checkParam' (ConstParamDefault _ t _ e) = t == inferExp e
  checkParam' (ParamDefault t _ e) = t == inferExp e
  checkParam' _ = True
  error = paramExp p >>= paramExpectedTypeError p . inferExp




inferExp ∷ Exp → Type
inferExp (EInt _) = TInt
inferExp (EFloat _) = TFloat
inferExp ETrue = TBool
inferExp EFalse = TBool
