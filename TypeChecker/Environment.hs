{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module TypeChecker.Environment where

import TypeChecker

-- | Environment during type checking
data Environment = Env {
  scopes  ∷ [Scope],
  options   ∷ Options,
  currentFile ∷ FilePath
}

instance Show Environment where
 show env = printf "Functions: %s\nVariables:%s"
   (showFuns (scopes env))
   (showVars (scopes env))

showFunctionType ∷ Function → String
showFunctionType (_, _, ret, args) = intercalate " -> " $ map show (args ++ [ret])

showFunction ∷ String → Function → String
showFunction n f = printf "%s :: %s "n (showFunctionType f)

showVars ∷ [Scope] → String
showVars scope = showVars' scope 2
 where
  showVars' ∷ [Scope] → Int → String
  showVars' (s:sc) l = showVarsLevel (toList (variables s)) l ++ showVars' sc (l+2)
  showVars' [] _ = ""

showVarsLevel ∷ [(String,Variable)] → Int → String
showVarsLevel vars l = newline ++ intercalate newline (map (uncurry showVar) vars)
 where
  newline = '\n':replicate l ' '

showVar ∷ String → Variable → String
showVar n (_,_,t) = printf "%s: %s" n $ show t

showFuns ∷ [Scope] → String
showFuns scope = showFuns' scope 2
 where
  showFuns' ∷ [Scope] → Int → String
  showFuns' (s:sc) l = showFunsLevel (functions s) l ++ showFuns' sc (l+2)
  showFuns' [] _ = ""

showFunsLevel ∷ Map String [Function] → Int → String
showFunsLevel funMap l = foldrWithKey reducer "" funMap
 where
  reducer n fs p = p ++ newline ++ intercalate newline (map (showFunction n) fs)
  newline = '\n':replicate l ' '

data Scope = Scope {
  functions ∷ Map String [Function],
  variables ∷ Map String Variable
}
 deriving (Show)
