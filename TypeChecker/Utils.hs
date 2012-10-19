{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker.Utils where

import Control.Arrow (second)

import Data.Maybe (isJust, fromJust)
import Data.Tree
import GHC.Exts (sortWith)

import CompilerTypes
import FrontEnd.AbsGrammar
import TypeChecker.Types

paramQualifiers ∷ Param → [Qualifier]
paramQualifiers (ParamDec qs _) = qs
paramQualifiers (ParamDefault qs _ _ _) = qs

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
paramToCIdent (ParamDefault _ i _ _) = i

paramToString ∷ Param → String
paramToString = cIdentToString . paramToCIdent

paramToPos ∷ Param → Position
paramToPos = cIdentToPos .  paramToCIdent

paramToQuals ∷ Param → [Qualifier]
paramToQuals (ParamDec qs _) = qs
paramToQuals (ParamDefault qs _ _ _) = qs

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
declPostIdents (DecAss i _ _) = i

-- Match function against argument types
partialApp ∷ Function → [Type] → Maybe [Type]
partialApp f args = partial args $ map varType $ paramVars f
 where
  partial ∷ [Type] → [Type] → Maybe [Type]
  partial (a:as) (b:bs) | a == b = partial as bs
                        | otherwise = Nothing
  partial [] bs = Just bs
  partial _  [] = Nothing

compAssType ∷ Type → Type → Maybe Type
compAssType TFloat TFloat = Just TFloat
compAssType TFloat TInt = Just TFloat
compAssType TInt TFloat = Just TFloat
compAssType tl tr = mayhaps (isVec tl && (tl == tr || tr == TFloat)) tl

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

isNum ∷ Type → Bool
isNum TInt = True
isNum TFloat = True
isNum _ = False

vecLength ∷ Type → Int
vecLength TVec2 = 2
vecLength TVec3 = 3
vecLength TVec4 = 4

buildAnonFunc ∷ String → Location → Type → [Type] → Function
buildAnonFunc name loc ret args = TypeChecker.Types.Function {
    functionName = name,
    functionLocation = loc,
    retType = ret,
    paramVars = map buildAnonVar args,
    parameters = [],
    statements = []
  }
 where
  buildAnonVar ∷ Type → Variable
  buildAnonVar = Variable "anonvar" ("anonvar",(-1,-1))

uncurryType ∷ Type → Type
uncurryType t@(TFunc {}) = TFun (head ret) args
 where
  unc = uncurryType' t
  (args, ret) = splitAt (length unc -1) unc
  uncurryType' ∷ Type → [Type]
  uncurryType' (TFunc t1 _ t2@(TFunc {})) = t1:uncurryType' t2
  uncurryType' (TFunc t1 _ t2) = t1:[t2]
uncurryType t = t

tryApply ∷ [Function] → [Type] → Maybe Type
tryApply funs args = if null matches
  then Nothing
  else do
    -- Apply as many arguments as possible (shortest list left after application)
    let (fun, args') = head $ sortWith snd matches
    if null args'
      then Just (retType fun) -- Function swallowed all arguments
      else Just $ TFun (retType fun) args' -- Function partially applied
 where
  matches = map (second fromJust) $
              filter (isJust . snd) $ zip funs (map (`partialApp` args) funs)

tryUncurry ∷ [Function] → [Type] → Maybe Type
tryUncurry funs (t:[]) | isVec t = tryUncurry' funs
                       | otherwise = Nothing
 where
  tryUncurry' ∷ [Function] → Maybe Type
  tryUncurry' (f:fs) = if all isNum (map varType (paramVars f)) && length (map varType (paramVars f)) == vecLength t
    then Just $ retType f
    else tryUncurry' fs
  tryUncurry' [] = Nothing
tryUncurry _ _ = Nothing

mayhaps ∷ Bool → a → Maybe a
mayhaps True  v = Just v
mayhaps False _ = Nothing

class Mongoid a where
  (¿) ∷ a → a → a

instance Mongoid (Maybe a) where
  Nothing ¿ perhaps = perhaps
  Just v  ¿ _       = Just v

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
