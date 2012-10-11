{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker.Environment where

import TypeChecker.Types
import TypeChecker.Scope
import Compiler (Options)

import Text.Printf
import Data.List (intercalate)
import Data.Map hiding (map)

-- | Environment during type checking
data Environment = Env {
  scopes  ∷ [Scope],
  options   ∷ Options,
  currentFile ∷ FilePath,
  currentFunction ∷ String
}

-- | Creates an empty environment based on a set of options
buildEnv ∷ Options → Environment
buildEnv opts = Env {
  scopes = [Scope empty empty],
  options = opts,
  currentFile = "",
  currentFunction = ""
}

-- | Pushes a new, empty scope to the environment
pushScope ∷ Environment → Environment
pushScope env = env { scopes = emptyScope:scopes env }

-- | Pop off a scope from the environment
popScope ∷ Environment → Environment
popScope env = env { scopes = tail $ scopes env }

-- Below is various show functions, used for displaying the state
instance Show Environment where
 show env = printf "Functions: %s\nVariables:%s"
   (showFuns $ reverse (scopes env))
   (showVars $ reverse (scopes env))

showFunctionType ∷ Function → String
showFunctionType (_, _, ret, args) = intercalate " -> " $ map show (args ++ [ret])

showFunction ∷ String → Function → String
showFunction n f = printf "%s :: %s "n (showFunctionType f)

showVars ∷ [Scope] → String
showVars scope = showVars' scope 0
 where
  showVars' ∷ [Scope] → Int → String
  showVars' (s:sc) l = showVarsLevel (toList (variables s)) l ++ showVars' sc (l+1)
  showVars' [] _ = ""

showVarsLevel ∷ [(String,Variable)] → Int → String
showVarsLevel [] _ = ""
showVarsLevel vars l = newline ++ intercalate newline (map (uncurry showVar) vars)
 where
  newline = '\n':concat (replicate l "> ")

showVar ∷ String → Variable → String
showVar n (_,_,t) = printf "%s: %s" n $ show t

showFuns ∷ [Scope] → String
showFuns scope = showFuns' scope 0
 where
  showFuns' ∷ [Scope] → Int → String
  showFuns' (s:sc) l = showFunsLevel (functions s) l ++ showFuns' sc (l+1)
  showFuns' [] _ = ""

showFunsLevel ∷ Map String [Function] → Int → String
showFunsLevel funMap l = foldrWithKey reducer "" funMap
 where
  reducer n fs p = p ++ newline ++ intercalate newline (map (showFunction n) fs)
  newline = '\n':concat (replicate l "> ")

