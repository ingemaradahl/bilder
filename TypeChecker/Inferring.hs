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
  types ← lookupVarTypes cid
  isAssigned cid >>= (\b → unless b $ notAssigned cid)
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
inferExp (EAss m@(EMember {}) tk e) = do
  memType ← inferExp m
  valueType ← inferExp e
  unless (memType == valueType) $ expTypeMismatch tk memType valueType
  return valueType
inferExp (EAss (EVar cid) tk e) = inferAssignment tk cid e
inferExp (EAssAdd (EVar cid) tk e) = inferAssignment tk cid e
inferExp (EAssSub (EVar cid) tk e) = inferAssignment tk cid e
inferExp (EAssMul (EVar cid) tk e) = inferAssignment tk cid e
inferExp (EAssDiv (EVar cid) tk e) = inferAssignment tk cid e
inferExp (EAssMod (EVar cid) tk e) = inferAssignment tk cid e
inferExp (EAss _ tk _) = lhsMustBeVar tk
inferExp (EAssAdd _ tk _) = lhsMustBeVar tk
inferExp (EAssSub _ tk _) = lhsMustBeVar tk
inferExp (EAssMul _ tk _) = lhsMustBeVar tk
inferExp (EAssDiv _ tk _) = lhsMustBeVar tk
inferExp (EAssMod _ tk _) = lhsMustBeVar tk
inferExp (EAssBWAnd _ tk _) = notSupportedError tk
inferExp (EAssBWXOR _ tk _) = notSupportedError tk
inferExp (EAssBWOR _ tk _) = notSupportedError tk
inferExp (EOR el tk er) = inferBoolexp tk el er
inferExp (EXOR el tk er) = inferBoolexp tk el er
inferExp (EAnd el tk er) = inferBoolexp tk el er
inferExp (EBWOR _ tk _) = notSupportedError tk
inferExp (EBWXOR _ tk _) = notSupportedError tk
inferExp (EBWAnd _ tk _) = notSupportedError tk
inferExp (EEqual el tk er) = inferConditional tk el er
inferExp (ENEqual el tk er) = inferConditional tk el er
inferExp (EBWShiftLeft _ tk _) = notSupportedError tk
inferExp (EBWShiftRight _ tk _) = notSupportedError tk
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
inferExp (EMod _ tk _) = notSupportedError tk -- TODO: implement? :S
inferExp (ENeg tk e) = inferBinaryNumExp tk e
inferExp (EPos tk e) = inferBinaryNumExp tk e
inferExp (ENegSign tk e) = do
  t ← inferExp e
  unless (isBoolish t) $ numOrBoolExpected tk
  return TBool
inferExp (EPreInc tk e) = inferUnaryNumExp tk e
inferExp (EPreDec tk e) = inferUnaryNumExp tk e
inferExp (EPostInc e tk) = inferUnaryNumExp tk e
inferExp (EPostDec e tk) = inferUnaryNumExp tk e
inferExp (EComplement tk _) = notSupportedError tk
inferExp (ELt el tk er) = inferConditional tk el er
inferExp (EGt el tk er) = inferConditional tk el er
inferExp (ELEt el tk er) = inferConditional tk el er
inferExp (EGEt el tk er) = inferConditional tk el er
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
inferExp (EIndex cid es) = do
  t ← inferExp es
  unless (t == TInt) $ debugError "NOPE"
  v ← lookupVar cid
  case varType v of
    TArray arrType → return arrType
    typ → if isMat typ
            then return $ mkVecType $ matSize typ
            else debugError "MOAR NOPE"
inferExp (EIndexDouble cid e1 e2) = do
  t1 ← inferExp e1
  t2 ← inferExp e2
  v ← lookupVar cid
  unless (t1 == t2 && t1 == TInt) $ debugError "NOPE"
  unless (isMat (varType v)) $ debugError "ANNAT NOPE"
  return TFloat
inferExp e = debugError $ show e ++ " not inferrablelollolo"


inferConditional ∷ Token a => a → Exp → Exp → TCM Type
inferConditional tk el er = do
  tl ← inferExp el
  tr ← inferExp er
  if all isNum [tl,tr]
    then return TBool
    else badCondTypes tk tl tr

inferBinaryExp ∷ Token a => a → Exp → Exp → TCM Type
inferBinaryExp tk el er = do
  tl ← inferExp el
  tr ← inferExp er
  case compNumType tl tr of
    Just t  → return t
    Nothing → badBinaryTypes tk tl tr

inferBinaryNumExp ∷ Token a => a → Exp → TCM Type
inferBinaryNumExp tk e = do
  t ← inferExp e
  unless (isNum t) $ numExpected tk
  return t

inferUnaryNumExp ∷ Token a => a → Exp → TCM Type
inferUnaryNumExp tk (EVar n) = do
  t ← liftM varType $ lookupVar n
  unless (isNum t) $ numExpected tk
  return t
inferUnaryNumExp tk _ = numExpected tk

inferAssignment ∷ Token a => a → CIdent → Exp → TCM Type
inferAssignment tk cid e = do
  setCIdentAssigned cid
  targetType ← liftM varType $ lookupVar cid
  valueType ← inferExp e
  case compAssType targetType valueType of
    Just _ → return targetType
    Nothing → expTypeMismatch tk targetType valueType

inferBoolexp ∷ Token a => a → Exp → Exp → TCM Type
inferBoolexp tk el er = do
  tl ← inferExp el
  tr ← inferExp er
  if all isBoolish [tl,tr]
    then return TBool
    else badBinaryTypes tk tl tr

-- vi:fdm=marker
