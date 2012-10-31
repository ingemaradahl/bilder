{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker.Utils where

-- Imports {{{
import Prelude hiding (lookup)

import Control.Arrow (second)

import Data.Map (lookup)
import Data.Maybe (isJust, fromJust)
import GHC.Exts (sortWith)

import Utils
import CompilerTypes
import FrontEnd.AbsGrammar
import FrontEnd.Instances
import TypeChecker.Types
import TypeChecker.Types.Blob
-- }}}

-- Conversions {{{
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

stmPos ∷ Stm → Position
stmPos (SWhile t _ _) = tkpos t
stmPos (SDoWhile t _ _ _) = tkpos t
stmPos (SFor t _ _ _ _) = tkpos t
stmPos (SReturn t _) = tkpos t
stmPos (SVoidReturn t) = tkpos t
stmPos (SIf t _ _) = tkpos t
stmPos (SIfElse t _ _ _ _) = tkpos t
stmPos (SBreak t) = tkpos t
stmPos (SContinue t) = tkpos t
stmPos (SDiscard t) = tkpos t
stmPos (SType _ s) = stmPos s
stmPos (SBlock ss) = stmPos $ head ss
stmPos (SDecl (Dec _ decpost)) = cIdentToPos $ head $ declPostIdents decpost
stmPos _ = (-1,-1)


-- }}}
-- Function checking {{{
-- Match function against argument types
partialApp ∷ Function → [Type] → Maybe [Type]
partialApp f args = partial args $ map varType $ paramVars f
 where
  partial ∷ [Type] → [Type] → Maybe [Type]
  partial (a:as) (b:bs) | a == b = partial as bs
                        | otherwise = Nothing
  partial [] bs = Just bs
  partial _  [] = Nothing

buildAnonFunc ∷ String → Location → Type → [Type] → Function
buildAnonFunc name loc ret args = TypeChecker.Types.Function {
    functionName = name,
    alias = "",
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

-- Try to find a match for applying the set of arguments to a function in the
-- given list.
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

-- Try to find a match where uncurrying the vector in the list might help with
-- finding a match in the list of functions.
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

checkReturns ∷ [Stm] → Bool
checkReturns = foldr ((||) . returns) False

returns ∷ Stm → Bool
returns (SReturn _ _) = True
returns (SVoidReturn _) = True
returns (SIfElse _ _ s _ s') = returns s && returns s'
returns (SBlock ss) = checkReturns ss
returns (SFor _ _ _ _ s) = returns s
returns (SWhile _ _ s) = returns s
returns (SDoWhile _ s _ _) = returns s
returns (SType _ s) = returns s
returns _ = False

-- }}}
-- Vector functions {{{
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
-- }}}
-- Type comparisons {{{
compAssType ∷ Type → Type → Maybe Type
compAssType TFloat TFloat = Just TFloat
compAssType TFloat TInt = Just TFloat
compAssType TInt TFloat = Just TFloat
compAssType TInt TInt = Just TInt
compAssType tl tr = mayhaps (isVec tl && (tl == tr || tr == TFloat)) tl

compNumType ∷ Type → Type → Maybe Type
compNumType TFloat TFloat = Just TFloat
compNumType TFloat TInt = Just TFloat
compNumType TInt TFloat = Just TFloat
compNumType tl tr | tl == tr = Just tr
                  | isVec tl = mayhaps (tl == tr || tr == TFloat) tl
                  | isVec tr = mayhaps (tl == tr || tl == TFloat) tr
                  | otherwise = Nothing

-- }}}

exports ∷ Blob → Function → Bool
exports (Blob  _ funs _ _) fun =
  case lookup (ident fun) funs of
    Just fs → fun `elem` fs
    Nothing → False


memberComponents ∷ Type → [String]
memberComponents TVec2 = ["xy", "rg", "st"]
memberComponents TVec3 = zipWith (++) (memberComponents TVec2) ["z", "b", "p"]
memberComponents TVec4 = zipWith (++) (memberComponents TVec3) ["w", "a", "q"]
memberComponents _ = []

-- vi:fdm=marker
