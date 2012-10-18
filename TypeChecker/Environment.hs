{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker.Environment where

import TypeChecker.Types hiding (typedefs)
import FrontEnd.AbsGrammar
import qualified TypeChecker.Scope as Scope
import Compiler (Options)

import Text.Printf
import Data.List (intercalate)
import Data.Map hiding (map)

-- | Environment during type checking
data Environment = Env {
  scopes  ∷ [Scope.Scope],
  checkedBlobs ∷ Map FilePath Blob,

  options   ∷ Options,
  currentFile ∷ FilePath,
  currentFunction ∷ Function
}

-- | Creates an empty environment based on a set of options
buildEnv ∷ Options → Environment
buildEnv opts = Env {
  scopes = [Scope.emptyScope],
  checkedBlobs = empty,
  options = opts,
  currentFile = "",
  currentFunction = Null
}

-- | Pushes a new, empty scope to the environment
pushScope ∷ Environment → Environment
pushScope env = env { scopes = Scope.emptyScope:scopes env }

-- | Pop off a scope from the environment
popScope ∷ Environment → Environment
popScope env = env { scopes = tail $ scopes env }

-- Show environment {{{
-- Below is various show functions, used for displaying the state
instance Show Environment where
 show env = printf "Type definitions: %s\nFunctions: %s\nVariables:%s"
   (showTypes $ reverse (scopes env))
   (showFuns $ reverse (scopes env))
   (showVars $ reverse (scopes env))

showTypes ∷ [Scope.Scope] → String
showTypes scs = showTypes' scs 0
 where
  showTypes' ∷ [Scope.Scope] → Int → String
  showTypes' (s:ss) l = showTypesLevel (Scope.typedefs s) l ++ showTypes' ss (l+1)
  showTypes' [] _ = ""

showTypesLevel ∷ Map String Typedef → Int → String
showTypesLevel typs l | not (Data.Map.null typs) = newline ++ intercalate newline (map showDef $ toList typs)
                      | otherwise = ""
 where
  newline = '\n':concat (replicate l "> ")

showDef ∷ (String,Typedef) → String
showDef (name,Typedef _ _ (TFun ret args)) = name ++ " = " ++ showTFun ret args
showDef (name,t) = name ++ " = " ++ show (typedefType t)

showFuns ∷ [Scope.Scope] → String
showFuns scope = showFuns' scope 0
 where
  showFuns' ∷ [Scope.Scope] → Int → String
  showFuns' (s:sc) l = showFunsLevel (Scope.functions s) l ++ showFuns' sc (l+1)
  showFuns' [] _ = ""

showFunsLevel ∷ Map String [Function] → Int → String
showFunsLevel funMap l = fold reducer "" funMap
 where
  reducer ∷ [Function] → String → String
  reducer funs p = p ++ newline ++ intercalate newline (map showFunction funs)
  newline = '\n':concat (replicate l "> ")

showTFun :: Type → [Type] → String
showTFun ret args = intercalate " -> " $ map show (args ++ [ret])

showFunctionType ∷ Function → String
showFunctionType fun = intercalate " -> " $ map show (args ++ ret)
 where
  ret  = [retType fun]
  args = map varType $ paramVars fun

showFunction ∷ Function → String
showFunction f = printf "%s :: %s " (ident f) (showFunctionType f)

showVars ∷ [Scope.Scope] → String
showVars scope = showVars' scope 0
 where
  showVars' ∷ [Scope.Scope] → Int → String
  showVars' (s:sc) l = showVarsLevel (elems (Scope.variables s)) l ++ showVars' sc (l+1)
  showVars' [] _ = ""

showVarsLevel ∷ [Variable] → Int → String
showVarsLevel [] _ = ""
showVarsLevel vars l = newline ++ intercalate newline (map showVar vars)
 where
  newline = '\n':concat (replicate l "> ")

showVar ∷ Variable → String
showVar var = printf "%s: %s" (ident var) (show $ varType var)
-- }}}
