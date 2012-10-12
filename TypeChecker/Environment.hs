{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker.Environment where

import TypeChecker.Types
import TypeChecker.Utils
import qualified TypeChecker.Scope as Scope
import Compiler (Options)

import Text.Printf
import Data.List (intercalate)
import Data.Map hiding (map)

-- | Environment during type checking
data Environment = Env {
  scopes  ∷ [Scope.Scope],
  options   ∷ Options,
  currentFile ∷ FilePath,
  currentFunction ∷ String
}

-- | Creates an empty environment based on a set of options
buildEnv ∷ Options → Environment
buildEnv opts = Env {
  scopes = [Scope.Scope empty empty],
  options = opts,
  currentFile = "",
  currentFunction = ""
}

-- | Pushes a new, empty scope to the environment
pushScope ∷ Environment → Environment
pushScope env = env { scopes = Scope.emptyScope:scopes env }

-- | Pop off a scope from the environment
popScope ∷ Environment → Environment
popScope env = env { scopes = tail $ scopes env }

-- Below is various show functions, used for displaying the state
instance Show Environment where
 show env = printf "Functions: %s\nVariables:%s"
   (showFuns $ reverse (scopes env))
   (showVars $ reverse (scopes env))

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


showFunctionType ∷ Function → String
showFunctionType fun = intercalate " -> " $ map show (args ++ ret)
 where
  ret  = [retType fun]
  args = map paramType (parameters fun)

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

