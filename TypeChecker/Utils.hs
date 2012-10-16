{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker.Utils where

import CompilerTypes
import FrontEnd.AbsGrammar
import TypeChecker.Types

paramQualifiers ∷ Param → [Qualifier]
paramQualifiers (ParamDec qs _) = qs
paramQualifiers (ParamDefault qs _ _) = qs

cIdentToString ∷ CIdent → String
cIdentToString (CIdent (_,s)) = s

cIdentToPos ∷ CIdent → Position
cIdentToPos (CIdent (pos,_)) = pos

typeIdentToString ∷ TypeIdent → String
typeIdentToString (TypeIdent (_,s)) = s

typeIdentToPos ∷ TypeIdent → Position
typeIdentToPos (TypeIdent (pos,_)) = pos

paramToCIdent ∷ Param → CIdent
paramToCIdent (ParamDec _ i) = i
paramToCIdent (ParamDefault _ i _) = i

paramToString ∷ Param → String
paramToString = cIdentToString . paramToCIdent

paramToPos ∷ Param → Position
paramToPos = cIdentToPos .  paramToCIdent

qualType ∷ [Qualifier] → Maybe Type
qualType qs | length types == 1 = Just $ head types
 where
  types = [ t | QType t ← qs ]
qualType _ = Nothing

-- Match function against argument types
partialApp ∷ Function → [Type] → Maybe [Type]
partialApp f args = partial args $ map varType $ paramVars f
 where
  partial ∷ [Type] → [Type] → Maybe [Type]
  partial (a:as) (b:bs) | a == b = partial as bs
                        | otherwise = Nothing
  partial [] bs = Just bs
  partial _  [] = Nothing

uncurryType ∷ Type → Type
uncurryType t@(TFunc {}) = TFun (head ret) args
 where
  unc = uncurryType' t
  (args, ret) = splitAt (length unc -1) unc
  uncurryType' ∷ Type → [Type]
  uncurryType' (TFunc t1 _ t2@(TFunc {})) = t1:uncurryType' t2
  uncurryType' (TFunc t1 _ t2) = t1:[t2]
uncurryType t = t
