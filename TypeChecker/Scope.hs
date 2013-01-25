{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker.Scope where

import Prelude hiding (lookup)
import Control.Monad.State

import TypeChecker.Types hiding (functions, variables)
import FrontEnd.AbsGrammar
import TypeChecker.Utils

import Data.Map hiding (map)

import Text.Printf

data Scope = Scope {
  functions ∷ Map String [Function],
  typedefs ∷ Map String Typedef,
  variables ∷ Map String Variable,
  assigned ∷ [String]
}
 deriving (Show)

emptyScope ∷ Scope
emptyScope = Scope { functions = empty, typedefs = empty, variables = empty, assigned = [] }

builtInScope ∷ Scope
builtInScope = emptyScope { functions = builtInFuns, variables = builtInVars }

-- | Add a function to a scope
addFunction ∷ Function → Scope → Scope
addFunction fun scope = scope { functions = fs', assigned = as}
 where
  name = ident fun
  fs = functions scope
  fs' = if member name fs
          then adjust (fun:) name fs
          else insert name [fun] fs
  as = map variableName (paramVars fun) ++ assigned scope

lookupFunction ∷ String → [Scope] → Maybe [Function]
lookupFunction fun (s:ss) = lookup fun (functions s) `mplus` lookupFunction fun ss
lookupFunction _ [] = Nothing

addVariable ∷ Variable → Scope → Scope
addVariable v scope = scope { variables = vs }
 where
  name = ident v
  vs = insert name v $ variables scope

lookupVar ∷ String → [Scope] → Maybe Variable
lookupVar name (s:ss) =
  case lookup name (variables s) of
    Just v  → Just v
    Nothing → lookupVar name ss
lookupVar _ [] = Nothing

lookupVarFun ∷ String → [Scope] → Maybe Function
lookupVarFun name (s:ss) =
  case lookup name (variables s) of
    Just (Variable n l (TFun ret args) _) → Just $ buildAnonFunc n l ret args
    Nothing → lookupVarFun name ss
lookupVarFun _ [] = Nothing

addTypedef ∷ Typedef → Scope → Scope
addTypedef t s = s { typedefs = insert (ident t) t (typedefs s) }

lookupTypedef ∷ String → [Scope] → Maybe Typedef
lookupTypedef n (s:ss) =
  case lookup n (typedefs s) of
    Just t  → Just t
    Nothing → lookupTypedef n ss
lookupTypedef _ [] = Nothing

isAssigned ∷ String → [Scope] → Bool
isAssigned _ [] = False
isAssigned n (s:ss)
  | n == "fl_Resolution" = True -- HACKHACK
  | n `elem` assigned s = True
  | otherwise           = isAssigned n ss

setAssigned ∷ String → [Scope] → [Scope]
setAssigned _ [] = []
setAssigned n ss
  | True `elem` map (\s → n `elem` (keys . variables) s ++ (keys . functions) s) ss = (head ss)
    { assigned = n : assigned (head ss) } : setAssigned n (tail ss)
  | otherwise = ss

builtInVars ∷ Map String Variable
builtInVars = fromList [("fl_Resolution", Variable "fl_Resolution" ("predefined", (-1,-1)) TVec2 (Just (EFloat (CFloat "1.0"))))]

-- TODO: Matrix versions.
-- TODO: Pure matrix functions: matrixCompMult
-- TODO: Functions that use bvec: lessThan, lessThanEqual, greaterThan,
--        greaterThanEqual, equal, notEqual, not, any, all.
-- | Creates a Map of built in functions
builtInFuns ∷ Map String [Function]
builtInFuns = fromListWith (++) $ evalState (mapM buildFuns (
    map (mkStdFuns 1) [ -- genType f (genType)
      "radians", "degrees", "sin", "cos", "tan", "asin", "acos", "exp", "log",
      "exp2", "log2", "sqrt", "inversesqrt", "abs", "sign", "floor", "ceil",
      "fract", "atan"
      ] ++
    map (mkStdFuns 2) [ -- genType f (genType, genType)
      "atan", "pow", "mod", "min", "max", "step"
      ] ++
    map (mkStdFuns 3) [ -- genType f (genType, genType, genType)
      "clamp", "mix", "smoothstep"
      ] ++
    [
      ("float", map (\t → (TFloat, [("v",t)])) [TInt, TFloat]),
      ("int", map (\t → (TInt, [("v",t)])) [TInt, TFloat]),
      ("length", map (\t → (TFloat, [("v",t)])) vecs),
      ("distance", map (\t → (TFloat, [("p0",t),("p1",t)])) vecs),
      ("dot", map (\t → (TFloat, [("x",t), ("y",t)])) vecs),
      ("cross", [(TVec3, [("x", TVec3), ("y", TVec3)])]),
      ("normalize", map (\t → (t, [("x",t)])) vecs),
      ("faceforward", map (\t → (t, [("N",t), ("I",t), ("Nref",t)])) vecs),
      ("reflect", map (\t → (t, [("I",t), ("N",t)])) vecs),
      ("refract", map (\t → (t, [("I",t), ("N",t), ("eta",TFloat)])) vecs),
      ("mod", map (\t → (t, [("v",t), ("a", TFloat)])) vecs),
      ("min", map (\t → (t, [("v",t), ("a", t), ("b", TFloat)])) vecs),
      ("max", map (\t → (t, [("v",t), ("a", t), ("b", TFloat)])) vecs),
      ("mix", map (\t → (t, [("v",t), ("a", t), ("b", TFloat)])) vecs),
      ("clamp", map (\t → (t, [("v",t), ("a", TFloat)])) vecs),
      ("step", map (\t → (t, [("v",TFloat), ("a", t)])) vecs),
      ("smoothstep", map (\t → (t, [("v",TFloat), ("b", TFloat), ("a", t)])) vecs)
  ])) [1..]
 where
  vecs = [ TVec2, TVec3, TVec4 ]
  vecnums = TFloat : vecs
  var ∷ (String, Type) → Variable
  var (n, t) = Variable n ("predefined", (-1,-1)) t Nothing
  param ∷ (String, Type) → Param
  param (n, _) = ParamDec [] (CIdent ((-1,-1), n))
  mkStdFuns ∷ Int → String → (String, [(Type, [(String, Type)])])
  mkStdFuns n s = (s, stdFunTypes n)
  stdFunTypes ∷ Int → [(Type, [(String, Type)])]
  stdFunTypes n = map (\t → (t, replicate n ("a", t))) vecnums
  buildFuns ∷ (String, [(Type, [(String, Type)])]) → State [Int] (String, [Function])
  buildFuns (n, ts) = liftM ((,) n) $ mapM (buildFun n) ts
  buildFun ∷ String → (Type, [(String, Type)]) → State [Int] Function
  buildFun n (rt, ts) = do
    i ← gets head
    modify tail
    return $ TypeChecker.Types.Function n (printf "_x%03d%s" i n) ("predefined", (-1,-1)) rt False (map var ts) (map param ts) []

