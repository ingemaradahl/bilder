{-# LANGUAGE UnicodeSyntax  #-}

module TypeChecker.TCM.Errors where

import TypeChecker.TCM
import TypeChecker.Utils
import TypeChecker.Types
import TypeChecker.Environment

import Control.Monad.Trans.State hiding (state)
import Control.Monad.Error

import CompilerError
import CompilerTypes
import FrontEnd.AbsGrammar

import Text.Printf

-- | Error w
paramExpectedTypeError ∷ Param → Type → TCM a
paramExpectedTypeError p t = do
  func ← gets currentFunction
  typeError (paramToPos p) $
    printf ("Couldn't match expected type %s\n" ++
            "            with actual type %s\n" ++
            "in assignment of parameter \"%s\"\n" ++
            "in declaration of function \"%s\"")
    (showParamType p)
    (show t)
    (paramToString p)
    (ident func)

paramNoTypeGiven ∷ Param → TCM a
paramNoTypeGiven p = do
  func ← gets currentFunction
  typeError (paramToPos p) $
    printf ("No type given for         \"%s\"\n" ++
            "in delaration of function \"%s\"\n" ++
            "          Qualifiers given: %s")
    (paramToString p)
    (ident func)
    (show (paramQualifiers p))

functionDefinedError ∷ Function → Function → TCM a
functionDefinedError defined adding =
  typeError (snd (location adding)) $ -- TODO use location in CompilerError
    printf ("Function \"%s\"\n" ++
            " with type %s already defined in %s")
    (ident defined)
    (showFunctionType defined)
    (show (location defined))



-- | Throw a type error
typeError ∷ Position → String → TCM a
typeError = absError TypeError

compileError ∷ Position → String → TCM a
compileError = absError CompileError

absError ∷ (Position → FilePath → String → CompilerError) → Position → String → TCM a
absError e p s = do
  state ← get
  let f = currentFile state
  throwError $ e p f (printf "%s\n\n%s" s (show state))

showParamType ∷ Param → String
showParamType p = case paramType p of
                    Just a → show a
                    Nothing → "{TYPE ERROR}"
 where
  paramType ∷ Param → Maybe Type
  paramType (ParamDec qs _) = qualType qs
  paramType (ParamDefault qs _ _) = qualType qs

