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
    Just (Variable n l (TFun ret args)) → Just $ buildAnonFunc n l ret args
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

-- | Creates a Map of built in functions
builtInFuns ∷ Map String [Function]
builtInFuns = fromList $ evalState (mapM buildFuns [
    ("pow", map (\t → (t, [("b",t), ("n",t)])) vecs ++
            map (\t → (t, [("b",t), ("n",TFloat)])) (TFloat : vecs)
    ),
    ("length", map (\t → (TFloat, [("v",t)])) vecs),
    ("min", map (\t → (t, [("a", t), ("b", t)])) vecnums ),
    ("sin", [(TFloat, [("phi", TFloat)])]),
    ("cos", [(TFloat, [("phi", TFloat)])]),
    ("dot", [(TVec4, [("x", TVec4), ("y", TVec4)])
            ,(TVec3, [("x", TVec3), ("y", TVec3)])
            ,(TVec2, [("x", TVec2), ("y", TVec2)])
            ])
  ]) [1..]
 where
  vecs = [ TVec2, TVec3, TVec4 ]
  vecnums = TFloat : vecs
  var ∷ (String, Type) → Variable
  var (n, t) = Variable n ("predefined", (-1,-1)) t
  param ∷ (String, Type) → Param
  param (n, _) = ParamDec [] (CIdent ((-1,-1), n))
  buildFuns ∷ (String, [(Type, [(String, Type)])]) → State [Int] (String, [Function])
  buildFuns (n, ts) = liftM ((,) n) $ mapM (buildFun n) ts
  buildFun ∷ String → (Type, [(String, Type)]) → State [Int] Function
  buildFun n (rt, ts) = do
    i ← gets head
    modify tail
    return $ TypeChecker.Types.Function n (printf "_x%02d%s" i n) ("predefined", (-1,-1)) rt (map var ts) (map param ts) []

