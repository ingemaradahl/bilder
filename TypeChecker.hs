{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker where

import Prelude hiding (lookup)

import Control.Monad
import Control.Monad.Trans.State
import Control.Applicative hiding (empty)

import Data.Tree
import Data.Map (Map, elems, insertWith, empty)

import Text.Printf

import TypeChecker.TCM
import TypeChecker.TCM.Errors
import TypeChecker.TCM.Utils

import TypeChecker.Utils
import TypeChecker.Environment hiding (pushScope, popScope)
import qualified TypeChecker.Scope as Scope (functions)
import TypeChecker.Types as Types

import Compiler hiding (Environment, Env, options, buildEnv)
import FrontEnd.AbsGrammar as Abs
import FrontEnd.Instances
import CompilerError
import Builtins

-- | Typechecks the given abstract source and annotates the syntax tree
typeCheck ∷ Options → Tree (FilePath, AbsTree) → CError (Tree Blob)
typeCheck opts tree = do
   blobTree ← evalStateT (traverse checkFile tree) (buildEnv opts)
   unless (rootLabel blobTree `exports` main) noEntryPoint
   return blobTree
 where
  rootFile = (fst. rootLabel) tree
  loc = (rootFile,(-1,-1))
  main = Types.Function "main" loc TVec4 [x,y] [] []
  x = Variable "x" loc TFloat
  y = Variable "y" loc TFloat
  noEntryPoint ∷ CError a
  noEntryPoint = Fail $ TypeError (-1,-1) rootFile $
    printf ("No entrypoint \"main\" of " ++
      "type Float,Float found in %s")
      rootFile

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
inferExp (EInt _) = return TInt
inferExp ETrue = return TBool
inferExp EFalse = return TBool
inferExp (EVar cid) = liftM varType $ lookupVar cid
inferExp (ECond ec tkq etrue tkc efalse) = do
  t ← inferExp ec
  unless (t `elem` [TInt,TFloat,TBool]) $ badConditional t (tkpos tkq)
  tetrue ← inferExp etrue
  tefalse ← inferExp efalse
  unless (tetrue == tefalse) $ typeMismatch (tkpos tkc) tetrue tefalse
  return tetrue
inferExp (EAss v@(EVar {}) tk e) = do
  targetType ← inferExp v
  valueType ← inferExp e
  case compAssType targetType valueType of
    Just _ → return targetType
    Nothing → debugError "FAILueaoeu"
inferExp (ECall cid es) = do
  args ← mapM inferExp es
  funs ← lookupFunction (cIdentToString cid)
  case tryApply funs args ¿ tryUncurry funs args of
    Just fun → return fun
    Nothing  → noFunctionFound cid args
inferExp (ETypeCall t es) = do
  expts ← mapM inferExp es
  t' ← filterTDef t
  if expts `elem` typeConstuctors t'
    then return t'
    else noTypeConstructorError t' expts
inferExp (EAdd el tk er) = inferBinaryExp tk el er
inferExp (EMul el tk er) = inferBinaryExp tk el er
inferExp (ESub el tk er) = inferBinaryExp tk el er
inferExp (EDiv el tk er) = inferBinaryExp tk el er
inferExp (ELt el tk er) = inferConditional tk el er
inferExp (EMember el cid) = do
  t ← inferExp el
  let pos = memberComponents t
  if any (\p -> all (== True) $ map (`elem` p) n) pos
    then if length n <= length types
      then return $ types !! (length n - 1)
      else vectorTooBig cid (length n)
    else wrongVectorComponents cid t
 where
  types = [TFloat, TVec2, TVec3, TVec4]
  n = cIdentToString cid
inferExp (EMemberCall el cid ers) = do
  tel ← inferExp el
  case componentFunc (cIdentToString cid) tel ers of
    Nothing → mapM inferExp ers >>= noFunctionFound cid
    Just (rt, argt, ecf) → do
      tecf ← mapM inferExp ecf
      if tecf == argt
        then return rt
        else noFunctionFound cid tecf

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

-- vi:fdm=marker
