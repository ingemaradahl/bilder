{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker.Utils where

import Data.Tree
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

paramToQuals ∷ Param → [Qualifier]
paramToQuals (ParamDec qs _) = qs
paramToQuals (ParamDefault qs _ _) = qs

qualToPos ∷ Qualifier → Position
qualToPos (QExternal (TkExternal (p,_))) = p
qualToPos (QConst (TkConst (p,_))) = p
qualToPos (QType _) = (-1,-1)

qualType ∷ [Qualifier] → Maybe Type
qualType qs | length types == 1 = Just $ head types
            | otherwise = Nothing
 where
  types = [ t | QType t ← qs ]

declPostIdents ∷ DeclPost → [CIdent]
declPostIdents (Vars i) = i
declPostIdents (DecAss i _) = i

-- Match function against argument types
partialApp ∷ Function → [Type] → Maybe [Type]
partialApp f args = partial args $ map varType $ paramVars f
 where
  partial ∷ [Type] → [Type] → Maybe [Type]
  partial (a:as) (b:bs) | a == b = partial as bs
                        | otherwise = Nothing
  partial [] bs = Just bs
  partial _  [] = Nothing

compNumType ∷ Type → Type → Maybe Type
compNumType TFloat TFloat = Just TFloat
compNumType TFloat TInt = Just TFloat
compNumType TInt TFloat = Just TFloat
compNumType tl tr | tl == tr = Just tr
                  | isVec tl = mayhaps (tl == tr || tr == TFloat) tl
                  | isVec tr = mayhaps (tl == tr || tl == TFloat) tr
                  | otherwise = Nothing


isVec ∷ Type → Bool
isVec TVec2 = True
isVec TVec3 = True
isVec TVec4 = True
isVec _    = False


uncurryType ∷ Type → Type
uncurryType t@(TFunc {}) = TFun (head ret) args
 where
  unc = uncurryType' t
  (args, ret) = splitAt (length unc -1) unc
  uncurryType' ∷ Type → [Type]
  uncurryType' (TFunc t1 _ t2@(TFunc {})) = t1:uncurryType' t2
  uncurryType' (TFunc t1 _ t2) = t1:[t2]
uncurryType t = t

mayhaps ∷ Bool → a → Maybe a
mayhaps True  v = Just v
mayhaps False _ = Nothing

traverse ∷ Monad m => (a → [Tree b] → m b) → Tree a → m (Tree b)
traverse f (Node r bs) = do
  sub ← mapM (traverse f) bs
  root ← f r sub
  return $ Node root sub

duplicates ∷ Eq a => [a] → [a]
duplicates [] = []
duplicates (x:xs)
  | x `elem` xs = x:duplicates (filter (/= x) xs)
  | otherwise   = duplicates xs

duplicatesWith ∷ (a → a → Bool) → [a] → [a]
duplicatesWith _ [] = []
duplicatesWith f (x:xs)
 | elemBy f x xs = x:duplicatesWith f (filter (not . f x) xs)
 | otherwise     = duplicatesWith f xs

elemBy ∷ (a → a → Bool) → a → [a] → Bool
elemBy _ _ [] = False
elemBy f y (x:xs)
 | f y x = True
 | otherwise = elemBy f y xs

memberComponents ∷ Type → [String]
memberComponents TVec2 = ["xy", "rg", "st"]
memberComponents TVec3 = zipWith (++) (memberComponents TVec2) ["z", "b", "p"]
memberComponents TVec4 = zipWith (++) (memberComponents TVec3) ["w", "a", "q"]
memberComponents _ = []
