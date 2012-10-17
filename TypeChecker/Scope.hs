{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module TypeChecker.Scope where

import TypeChecker.Types hiding (functions, variables, typedefs)
import FrontEnd.AbsGrammar

import Data.Map hiding (map)

data Scope = Scope {
  functions ∷ Map String [Function],
  typedefs ∷ Map String Typedef,
  variables ∷ Map String Variable
}
 deriving (Show)

emptyScope ∷ Scope
emptyScope = Scope { functions = empty, typedefs = empty, variables = empty }

builtInScope ∷ Scope
builtInScope = Scope { functions = builtInFuns, typedefs = empty, variables = empty }

-- | Add a function to a scope
addFunction ∷ Function → Scope → Scope
addFunction fun scope = scope { functions = fs' }
 where
  name = ident fun
  fs = functions scope
  fs' = if member name fs
          then adjust (fun:) name fs
          else insert name [fun] fs

addVariable ∷ Variable → Scope → Scope
addVariable v scope = scope { variables = vs }
 where
  name = ident v
  vs = insert name v $ variables scope

addTypedef ∷ Typedef → Scope → Scope
addTypedef t s = s { typedefs = insert (typedefName t) t (typedefs s) }

lookupTypedef ∷ String → [Scope] → Maybe Typedef
lookupTypedef n (s:ss) =
  case Data.Map.lookup n (typedefs s) of
    Just t  → Just t
    Nothing → lookupTypedef n ss
lookupTypedef _ [] = Nothing

-- | Creates a Map of built in functions
builtInFuns ∷ Map String [Function]
builtInFuns = fromList $ map buildFuns [
    ("pow", [(TFloat, [("b", TFloat), ("n", TFloat)])]),
    ("sin", [(TFloat, [("phi", TFloat)])]),
    ("cos", [(TFloat, [("phi", TFloat)])]),
    ("dot", [(TVec4, [("x", TVec4), ("y", TVec4)])
            ,(TVec3, [("x", TVec3), ("y", TVec3)])
            ,(TVec2, [("x", TVec2), ("y", TVec2)])
            ])
  ]
 where
  var ∷ (String, Type) → Variable
  var (n, t) = Variable n ("predefined", (-1,-1)) t
  param ∷ (String, Type) → Param
  param (n, _) = ParamDec [] (CIdent ((-1,-1), n))
  buildFuns ∷ (String, [(Type, [(String, Type)])]) → (String, [Function])
  buildFuns (n, ts) = (n, map (buildFun n) ts)
  buildFun ∷ String → (Type, [(String, Type)]) → Function
  buildFun n (rt, ts) =
    TypeChecker.Types.Function n ("predefined", (-1,-1)) rt (map var ts) (map param ts) []

