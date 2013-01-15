{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker.Utils where

-- Imports {{{
import Prelude hiding (lookup)

import Control.Arrow (second)

import Data.Map (lookup)
import Data.Maybe (isJust, fromJust)
import GHC.Exts (sortWith)

import Text.Printf

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
qualToPos (QPixelwise tk) = tkpos tk
qualToPos (QBounded tk) = tkpos tk
qualToPos (QType _) = (-1,-1)

qualType ∷ [Qualifier] → Maybe Type
qualType qs | length types == 1 = Just $ head types
            | otherwise = Nothing
 where
  types = [ t | QType t ← qs ]

isConst ∷ Qualifier → Bool
isConst (QConst _) = True
isConst _ = False

declPostIdents ∷ DeclPost → [CIdent]
declPostIdents (Vars i) = i
declPostIdents (DecAss i _ _) = i
declPostIdents (DecFun i _ _) = [i]

declPostExp ∷ DeclPost → Maybe Exp
declPostExp (DecAss _ _ e) = Just e
declPostExp _ = Nothing

toCIdent ∷ Global a => a → CIdent
toCIdent v = CIdent (position v, ident v)

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

okForPixelQuals ∷ Type → [Type] → Bool
okForPixelQuals ret ts | length ts < 2 || ret /= TVec4 = False
                       | otherwise = ret == TVec4 && all isNum (leave 2 ts)

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
    alias = "", -- Hacky, but has to be empty (see renameExp|ECall)
    functionLocation = loc,
    retType = ret,
    pixelwise = False,
    paramVars = map buildAnonVar args,
    parameters = [],
    statements = []
  }
 where
  buildAnonVar ∷ Type → Variable
  buildAnonVar t = Variable "anonvar" ("anonvar",(-1,-1)) t Nothing

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
tryApplyType ∷ [Function] → [Type] → Maybe Type
tryApplyType funs args = if null legal
  then if null matches
    then Nothing
    else error $ printf "Illegal partial application with function \"%s\". It is only possible to create functions of type Image." (functionName $ fst $ head matches)
  else do
    -- Apply as many arguments as possible (shortest list left after application)
    let (fun, args') = head $ sortWith snd legal
    if null args'
      then Just (retType fun) -- Function swallowed all arguments
      else Just $ TFun (retType fun) args' -- Function partially applied
 where
  matches = map (second fromJust) $
              filter (isJust . snd) $ zip funs (map (`partialApp` args) funs)
  legal = filter (\(_,as) → allFloat as && (length as == 2 || null as)) matches

tryApply ∷ [Function] → [Type] → Maybe Function
tryApply funs args = if null legal
  then if null matches
    then Nothing
    else error $ printf "Illegal partial application with function \"%s\". It is only possible to create functions of type Image." (functionName $ fst $ head matches)
  else do
    -- Apply as many arguments as possible (shortest list left after application)
    let (fun, _) = head $ sortWith snd legal
    Just fun
 where
  matches = map (second fromJust) $
              filter (isJust . snd) $ zip funs (map (`partialApp` args) funs)
  legal = filter (\(_,as) → allFloat as && (length as == 2 || null as)) matches

-- Try to find a match where uncurrying the vector in the list might help with
-- finding a match in the list of functions.
tryUncurryType ∷ [Function] → [Type] → Maybe Type
tryUncurryType fs ts = fmap retType (tryUncurry fs ts)

tryUncurry ∷ [Function] → [Type] → Maybe Function
tryUncurry funs (t:[]) | isVec t = tryUncurry' funs
                       | otherwise = Nothing
 where
  tryUncurry' ∷ [Function] → Maybe Function
  tryUncurry' (f:fs) = if all isNum (map varType (paramVars f)) && length (map varType (paramVars f)) == vecLength t
    then Just f
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

isMat ∷ Type → Bool
isMat TMat2 = True
isMat TMat3 = True
isMat TMat4 = True
isMat _ = False


isNum ∷ Type → Bool
isNum TInt = True
isNum TFloat = True
isNum _ = False

isBool ∷ Type → Bool
isBool TBool = True
isBool _ = False

isBoolish ∷ Type → Bool
isBoolish t = isNum t || isBool t

vecLength ∷ Type → Int
vecLength TVec2 = 2
vecLength TVec3 = 3
vecLength TVec4 = 4

matSize ∷ Type → Int
matSize TMat2 = 2
matSize TMat3 = 3
matSize TMat4 = 4

mkVecType ∷ Int → Type
mkVecType 2 = TVec2
mkVecType 3 = TVec3
mkVecType 4 = TVec4
-- }}}
-- Type comparisons {{{
compAssType ∷ Type → Type → Maybe Type
compAssType TFloat TFloat = Just TFloat
compAssType TFloat TInt = Just TFloat
compAssType TInt TFloat = Just TFloat
compAssType TInt TInt = Just TInt
compAssType tl@(TFun {}) tr@(TFun {}) = mayhaps (tl == tr) tl
compAssType tl tr = mayhaps (isVec tl && (tl == tr || tr == TFloat)) tl

compNumType ∷ Type → Type → Maybe Type
compNumType TFloat TFloat = Just TFloat
compNumType TFloat TInt = Just TFloat
compNumType TInt TFloat = Just TFloat
compNumType tl tr | tl == tr = Just tr
                  | isMat tl = mayhaps (isVec tr && matSize tl == vecLength tr) tl
                  | isMat tr = mayhaps (isVec tl && matSize tr == vecLength tl) tr
                  | isVec tl = mayhaps (tl == tr || tr == TFloat) tl
                  | isVec tr = mayhaps (tl == tr || tl == TFloat) tr
                  | otherwise = Nothing

allFloat ∷ [Type] → Bool
allFloat ts = not $ any (/=TFloat) ts
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
