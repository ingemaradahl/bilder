{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker.Utils where

import CompilerTypes
import FrontEnd.AbsGrammar
import TypeChecker.Types

paramQualifiers ∷ Param → [Qualifier]
paramQualifiers (ParamDec qs _) = qs
paramQualifiers (ParamDefault qs _ _) = qs

idToType ∷ Id → Type → Type
idToType (IdEmptyArray _) t = TArray t
idToType (IdArray _ _) t = TArray t
idToType _ t = t

idToCIdent ∷ Id → CIdent
idToCIdent (IdEmptyArray c) = c
idToCIdent (IdArray c _) = c
idToCIdent (Ident c) = c

idToPos ∷ Id → Position
idToPos = cIdentToPos . idToCIdent

idToString ∷ Id → String
idToString = cIdentToString . idToCIdent

cIdentToString ∷ CIdent → String
cIdentToString (CIdent (_,s)) = s

cIdentToPos ∷ CIdent → Position
cIdentToPos (CIdent (pos,_)) = pos

paramToId ∷ Param → Id
paramToId (ParamDec _ i) = i
paramToId (ParamDefault _ i _) = i

paramToString ∷ Param → String
paramToString = cIdentToString . idToCIdent . paramToId

paramToPos ∷ Param → Position
paramToPos = cIdentToPos . idToCIdent . paramToId

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
