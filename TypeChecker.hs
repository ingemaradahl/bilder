{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker where

import Prelude hiding (lookup)

import Control.Monad
import Control.Monad.Trans.State
import Control.Arrow (second)
import Control.Applicative hiding (empty)

import Data.Tree
import Data.Map (Map, elems, insertWith, empty)
import Data.Maybe (isJust, fromJust)
import GHC.Exts (sortWith)

import TypeChecker.TCM
import TypeChecker.TCM.Errors
import TypeChecker.TCM.Utils

import TypeChecker.Utils
import TypeChecker.Environment hiding (pushScope, popScope)
import qualified TypeChecker.Scope as Scope (functions)
import TypeChecker.Types

import Compiler hiding (Environment, Env, options, buildEnv)
import FrontEnd.AbsGrammar as Abs
import FrontEnd.Instances
import CompilerError
import Builtins

-- | Typechecks the given abstract source and annotates the syntax tree
typeCheck ∷ Options → Tree (FilePath, AbsTree) → CError (Tree Blob)
typeCheck opts tree = evalStateT (traverse checkFile tree) (buildEnv opts)

checkFile ∷ (FilePath, AbsTree) → [Tree Blob] → TCM Blob
checkFile (file, tree) children = do
  newFile file

  initScope children
  pushScope

  addTypedefs tree
  addFunctions tree

  -- Return blob with new annotated functions
  checkFunctions >>= makeBlob

-- Type definitions {{{
-- | Adds type definitions to the to the state
addTypedefs ∷ AbsTree → TCM ()
addTypedefs (AbsTree tree) = sequence_
  [ filterTDef typ >>= addTypeIdentTypedef name | (TypeDef _ name _ typ) ← tree ]

-- }}}
-- Functions {{{
-- | Adds function type definitions to the state
addFunctions ∷ AbsTree → TCM ()
addFunctions (AbsTree tree) = do

  -- Checks function parameter types, and adds function to environment
  funs ← sequence [ tcFun f | f@(Abs.Function {}) ← tree ]
  mapM_ addFunction funs

-- | Check functions for typing errors, returning the type annotated functions
checkFunctions ∷ TCM (Map String [Function])
checkFunctions = do
  funs ← gets (Scope.functions . head . scopes)
    >>= mapM checkFunction . concat . Data.Map.elems
  return $ foldr (\f p → insertWith (++) (ident f) [f] p) empty funs

-- | Checks a function for type errors, and returns the annotated function
checkFunction ∷ Function → TCM Function
checkFunction fun = do
  pushScope
  updateFunction fun

  -- Add parameters to scope
  mapM_ addVariable (paramVars fun)

  -- Check that eventual parameter assignments are correct
  mapM_ checkParam (zip (parameters fun) (paramVars fun))

  -- Check and annotate statements
  statements' ← mapM checkStatement (statements fun)

  popScope
  return fun { statements = statements' }
-- }}}
-- Parameters {{{
checkParam ∷ (Param, Variable) → TCM ()
checkParam (p,v) = checkParam' p >>= flip unless error'
 where
  checkParam' ∷ Param → TCM Bool
  checkParam' (ParamDefault _ _ _ e) = liftM (varType v ==) (inferExp e)
  checkParam' _  = return True
  error' = paramExp p >>= inferExp >>= paramExpectedTypeError p
-- }}}
-- Statements {{{
checkStatement ∷ Stm → TCM Stm
checkStatement s@(SReturn (TkReturn (pos,_)) e) = do
  fun ← gets currentFunction
  t ← inferExp e
  if retType fun == t
    then return $ SType t s
    else returnMismatch pos t
checkStatement s@(SDecl decl) = checkDecl decl >>= (\t → return (SType t s))
checkStatement (SIf tk@(TkIf (pos,_)) cond stm) = do
  condt ← inferExp cond
  unless (condt `elem` [TInt,TFloat,TBool]) $ badConditional condt pos
  SType condt <$> SIf tk cond <$> checkStatement stm
checkStatement (SBlock stms) = SBlock <$> mapM checkStatement stms
checkStatement s@(SExp e) = SType <$> inferExp e <*> pure s
checkStatement s = debugError $ show s ++ " NOT DEFINED"
-- }}}
-- Declarations {{{
checkDecl ∷ Decl → TCM Type
checkDecl (Dec qs post) = do
  t ← verifyQualsType qs >>= filterTDef
  expT ← checkDecAss post

  -- Check that inferred type and declared type matches
  maybe (return ())
    (\a → unless (t == a) $ decAssError (head $ declPostIdents post) a t ) expT

  sequence_ [ addCIdentVariable cid t | cid ← declPostIdents post ]

  return t

checkDecAss ∷ DeclPost → TCM (Maybe Type)
checkDecAss (Vars _) = return Nothing
checkDecAss (DecAss _ _ e) = fmap Just $ inferExp e

-- }}}
-- Expressions {{{
inferExp ∷ Exp → TCM Type
inferExp (EFloat _) = return TFloat
inferExp ETrue = return TBool
inferExp EFalse = return TBool
inferExp (EVar cid) = liftM varType $ lookupVar cid
inferExp (EAss v@(EVar {}) tk e) = do
  targetType ← inferExp v
  valueType ← inferExp e
  case compAssType targetType valueType of
    Just _ → return targetType
    Nothing → debugError "FAILueaoeu"
inferExp (ECall cid es) = do
  args ← mapM inferExp es
  funs ← lookupFunction (cIdentToString cid)
  let matches = map (second fromJust) $
                  filter (isJust . snd) $ zip funs (map (`partialApp` args) funs)
  if null matches
    then noFunctionFound cid args
    else do
      -- Apply as many arguments as possible (shortest list left after application)
      let (fun, args') = head $ sortWith snd matches
      if null args'
        then return (retType fun) -- Function swallowed all arguments
        else return $ TFun (retType fun) args' -- Function partially applied
inferExp (ETypeCall t es) = do
  expts ← mapM inferExp es
  if expts `elem` typeConstuctors t
    then return t
    else noTypeConstructorError t expts
inferExp (EAdd el tk er) = inferBinaryExp tk el er
inferExp (EMul el tk er) = inferBinaryExp tk el er
inferExp (ESub el tk er) = inferBinaryExp tk el er
inferExp (EDiv el tk er) = inferBinaryExp tk el er
inferExp (ELt el tk er) = inferConditional tk el er
inferExp (EMember el cid) = do
  t ← inferExp el
  let pos = memberComponents t
  if any (\p -> all (== True) $ map (`elem` p) n) pos
    then if length n <= length types && length n > 0
      then return $ types !! (length types - 1)
      else vectorTooBig cid (length n)
    else wrongVectorComponents cid t
 where
  types = [TFloat, TVec2, TVec3, TVec4]
  n = cIdentToString cid

inferExp e = debugError $ show e ++ " not inferrablelollolo"


inferConditional ∷ Token a => a → Exp → Exp → TCM Type
inferConditional tk el er = do
  tl ← inferExp el
  tr ← inferExp er
  if all (`elem` [TInt,TFloat]) [tl,tr]
    then return TBool
    else badCondTypes tk tl tr

inferBinaryExp ∷ Token a => a → Exp → Exp → TCM Type
inferBinaryExp tk el er = do
  tl ← inferExp el
  tr ← inferExp er
  case compNumType tl tr of
    Just t  → return t
    Nothing → badBinaryTypes tk tl tr
-- }}}

--example ∷ AbsTree
--example = AbsTree [Import (TkImport ((1,1),"import")) "bools.fl",Import (TkImport ((2,1),"import")) "inner/const.fl",Abs.Struct (TkStruct ((4,1),"struct")) (CIdent ((4,8),"First")) [SVDecl (Dec TColor (OnlyVars [Ident (CIdent ((5,15),"color"))])),SVDecl (Dec TVec2 (OnlyVars [Ident (CIdent ((6,14),"coordinates"))]))],StructDecl (TkStruct ((9,1),"struct")) (CIdent ((9,8),"Second")) [SVDecl (Dec TVec2 (OnlyVars [Ident (CIdent ((10,14),"coordinates"))]))] (Ident (CIdent ((11,3),"second"))),StructDecl (TkStruct ((13,1),"struct")) (CIdent ((13,8),"Third")) [SVDecl (Dec TVec2 (OnlyVars [Ident (CIdent ((14,14),"coordinates"))]))] (IdArray (CIdent ((15,3),"third")) (EInt 5)),Abs.Function TColor (CIdent ((17,7),"main")) [ParamDec TInt (Ident (CIdent ((17,16),"x"))),ParamDec TInt (Ident (CIdent ((17,23),"y")))] [SDecl (DecStruct (Ident (CIdent ((19,9),"First"))) (OnlyVars [Ident (CIdent ((19,15),"a"))])),SDecl (DecStruct (Ident (CIdent ((20,9),"First"))) (DefaultVars [Ident (CIdent ((20,15),"b"))] (ECall (Ident (CIdent ((20,19),"First"))) [ETypeCall TColor [EFloat (CFloat "1.0")] OnlyCall,ETypeCall TVec2 [EFloat (CFloat "1.0"),EFloat (CFloat "2.0")] OnlyCall]))),SExp (EAss (EMember (EVar (Ident (CIdent ((22,9),"second")))) (EVar (Ident (CIdent ((22,16),"coordinates"))))) (ETypeCall TVec2 [EFloat (CFloat "1.0"),EFloat (CFloat "2.0")] OnlyCall)),SReturn (TkReturn ((24,9),"return")) (EMember (EVar (Ident (CIdent ((24,16),"b")))) (EVar (Ident (CIdent ((24,18),"color")))))]]

-- vi:fdm=marker
