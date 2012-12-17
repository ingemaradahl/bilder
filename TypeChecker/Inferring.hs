{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.Inferring where

-- Imports {{{
import Control.Monad

import Builtins

import TypeChecker.TCM
import TypeChecker.TCM.Errors
import TypeChecker.TCM.Utils

import TypeChecker.Utils
import TypeChecker.Types (varType)

import FrontEnd.AbsGrammar
import FrontEnd.Instances

import Text.Printf (printf)
-- }}}

inferExp ∷ Exp → TCM Type
inferExp (EFloat _) = return TFloat
inferExp (EInt _) = return TInt
inferExp ETrue = return TBool
inferExp EFalse = return TBool
inferExp (EVar cid) = do
  isAssigned cid >>= (\b → unless b $ notAssigned cid)
  types ← lookupVarTypes cid
  when (length types > 1) $ warning (cIdentToPos cid)
    $ printf "more than one function/variable by the name \"%s\" - using the one declared last." (cIdentToString cid)
  return (head types)
inferExp (ECond ec tkq etrue tkc efalse) = do
  t ← inferExp ec
  unless (t `elem` [TInt,TFloat,TBool]) $ badConditional t (tkpos tkq)
  tetrue ← inferExp etrue
  tefalse ← inferExp efalse
  unless (tetrue == tefalse) $ typeMismatch (tkpos tkc) tetrue tefalse
  return tetrue
inferExp (EAss (EVar cid) tk e) = do
  setAssigned cid
  targetType ← liftM varType $ lookupVar cid
  valueType ← inferExp e
  case compAssType targetType valueType of
    Just _ → return targetType
    Nothing → expTypeMismatch tk targetType valueType
inferExp (EAss m@(EMember {}) tk e) = do
  memType ← inferExp m
  valueType ← inferExp e
  unless (memType == valueType) $ expTypeMismatch tk memType valueType
  return valueType
-- TODO: Copy paste technology (.js), generalize cases like this
inferExp (EAssAdd (EVar cid) tk e) = do
  setAssigned cid
  targetType ← liftM varType $ lookupVar cid
  valueType ← inferExp e
  case compAssType targetType valueType of
    Just _ → return targetType
    Nothing → expTypeMismatch tk targetType valueType
inferExp (ECall cid es) = do
  args ← mapM inferExp es
  funs ← lookupFunction (cIdentToString cid)
  case tryApplyType funs args `mplus` tryUncurryType funs args of
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

-- vi:fdm=marker
