{-
 -      This file is part of Bilder.
 -
 -   Bilder is free software: you can redistribute it and/or modify
 -   it under the terms of the GNU Lesser General Public License as published by
 -   the Free Software Foundation, either version 3 of the License, or
 -   (at your option) any later version.
 -
 -   Bilder is distributed in the hope that it will be useful,
 -   but WITHOUT ANY WARRANTY; without even the implied warranty of
 -   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 -   GNU Lesser General Public License for more details.
 -
 -   You should have received a copy of the GNU Lesser General Public License
 -   along with Bilder.  If not, see <http://www.gnu.org/licenses/>.
 -
 -   Copyright © 2012-2013 Filip Lundborg
 -   Copyright © 2012-2013 Ingemar Ådahl
 -
 -}
{-# LANGUAGE UnicodeSyntax #-}

module TypeChecker.Environment where

import TypeChecker.Types
import FrontEnd.AbsGrammar
import qualified TypeChecker.Scope as Scope
import Compiler (Options)
import CompilerTypes

import Text.Printf
import Data.List (intercalate)
import Data.Map hiding (map)

type Aliases = Map String String

-- | Environment during type checking
data Environment = Env {
  -- Scoping, each level in list represents scope level
  scopes  ∷ [Scope.Scope],

  -- Renaming
  renamed ∷ Map FilePath Source,
  aliases ∷ [Aliases], -- List levels correspond to scoping
  freeAliases ∷ [Int],

  options   ∷ Options,
  warnings ∷ [(Location, String)],
  currentFile ∷ FilePath,
  currentFunction ∷ Function
}

-- | Creates an empty environment based on a set of options
buildEnv ∷ Options → Environment
buildEnv opts = Env {
  scopes = [Scope.emptyScope],

  renamed = empty,
  aliases = [],
  freeAliases = [1..],

  options = opts,
  warnings = [],
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
 show env = printf "Warnings:\n%s\nType definitions: %s\nFunctions: %s\nVariables:%s\nAliases:%s"
   (intercalate "\n" (map show (warnings env)))
   (showTypes $ reverse (scopes env))
   (showFuns $ reverse (scopes env))
   (showVars $ reverse (scopes env))
   (show (aliases env))

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
showFuns scope = showFuns' (tail scope) 0
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
-- vi:fdm=marker
