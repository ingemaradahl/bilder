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
import FrontEnd.Instances

import Data.List (intercalate)

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

noFunctionFound ∷ CIdent → [Type] → TCM a
noFunctionFound f args =
  typeError (cIdentToPos f) $
    printf ("No function \"%s\" found matching argument list\n" ++
            "              %s")
    (cIdentToString f)
    (show args)

noVarFound ∷ CIdent → TCM a
noVarFound cid =
  typeError (cIdentToPos cid) $
    printf "No variable \"%s\" found" (cIdentToString cid)

returnMismatch ∷ Position → Type → TCM a
returnMismatch pos inferred = do
  expected ← gets currentFunction
  typeError pos $
    printf ("Couldn't match expected type %s\n" ++
            "            with actual type %s\n" ++
            "in return expression in function \"%s\"")
    (show (retType expected))
    (show inferred)
    (ident expected)

typedefError ∷ Typedef → Typedef → TCM a
typedefError defined proposed =
  typeError pos $
    printf ("Trying to define a new type alias \"%s\"\n" ++
            "                         with type %s\n" ++
            "while type alias \"%s\" = %s is already present")
    name
    (show $ typedefType proposed)
    name
    (show $ typedefType defined)
 where
  pos = position proposed
  name = ident defined

typedefNotFoundError ∷ TypeIdent → TCM a
typedefNotFoundError tid =
  typeError pos $
    printf "Type definition %s not found"
    (typeIdentToString tid)
 where
  pos = typeIdentToPos tid

invalidQualList ∷ [Qualifier] → TCM a
invalidQualList qs =
  syntaxError pos $
    printf "Invalid qualifier list: %s"
    (show qs)
 where
  pos = qualToPos $ head qs

qualsNoTypeGiven ∷ [Qualifier] → TCM a
qualsNoTypeGiven qs =
  syntaxError pos $
    printf "No type given in qualifier list: %s"
    (show qs)
 where
  pos = qualToPos $ head qs

pixelQualsOnImageonly ∷ CIdent → TCM a
pixelQualsOnImageonly cid =
  typeError pos $
    printf ("In definition of function '%s',\n" ++
            "Pixel layout qualifier only allowed on image-like functions")
    (cIdentToString cid)
 where
  pos = cIdentToPos cid


decAssError ∷ CIdent → Type → Type → TCM a
decAssError cid inferred expected =
  typeError (cIdentToPos cid) $
    printf ("Couldn't match expected type %s\n" ++
            "            with actual type %s\n" ++
            "  in declaration of variable %s")
    (show expected)
    (show inferred)
    (cIdentToString cid)

noTypeConstructorError ∷ Type → [Type] → TCM a
noTypeConstructorError t ts =
  typeError (-1,-1) $ -- TODO
    printf ("No type constructor for %s found,\n" ++
            "matching argument types %s")
    (show t)
    (show ts)

badConditional ∷ Type → Position → TCM a
badConditional t pos =
  typeError pos $
    printf "Bad conditional with type %s" (show t)

badCondTypes ∷ Token a => a → Type → Type → TCM b
badCondTypes tk tl tr =
  typeError (tkpos tk) $
    printf ("Couldn't match %s and %s\n" ++
            "in conditional %s")
    (show tl)
    (show tr)
    (tkident tk)

badBinaryTypes ∷ Token a => a → Type → Type → TCM b
badBinaryTypes tk tl tr =
  typeError (tkpos tk) $
    printf ("Couldn't match %s and %s\n" ++
            " in expression %s")
    (show tl)
    (show tr)
    (tkident tk)

wrongVectorComponents ∷ CIdent → Type → TCM a
wrongVectorComponents cid t =
  typeError (cIdentToPos cid) $
    printf ("Could not match range of expected components %s\n" ++
            "                      with actual components %s")
    (intercalate ", " (memberComponents t))
    (cIdentToString cid)

vectorTooBig ∷ CIdent → Int → TCM a
vectorTooBig cid s =
  typeError (cIdentToPos cid) $
    printf "Vector components %s form a new vector of unhandled size %d"
    (cIdentToString cid)
    s

typeMismatch ∷ Position → Type → Type → TCM b
typeMismatch pos ta tb =
  typeError pos $
    printf "Types %s and %s does not match."
    (show ta)
    (show tb)

numExpected ∷ Token a => a → TCM b
numExpected tk =
  typeError (tkpos tk) $
    printf "Numerical value expected in '%s' operator"
    (tkident tk)

numOrBoolExpected ∷ Token a => a → TCM b
numOrBoolExpected tk =
  typeError (tkpos tk) $
    printf "Numerical or boolean value expected in '%s' operator"
    (tkident tk)

expTypeMismatch ∷ Token a => a → Type → Type → TCM b
expTypeMismatch tk expected actual =
  typeError (tkpos tk) $
    printf ("Couldn't match expected type %s\n" ++
            "            with actual type %s\n" ++
            "in expression %s")
    (show expected)
    (show actual)
    (tkident tk)

notSupportedError ∷ Token a => a → TCM b
notSupportedError tk =
  typeError (tkpos tk) $
    printf "%s operator not supported"
    (tkident tk)

lhsMustBeVar ∷ Token a => a → TCM b
lhsMustBeVar tk =
  typeError (tkpos tk) $
  printf "Left hand side in '%s' assignment must be a variable"
  (tkident tk)


noReturnError ∷ Function → TCM a
noReturnError f =
  typeError (-1,-1) $
    printf "Function %s is missing a definite return expression" (ident f)

noFunctionQualifiers ∷ CIdent → TCM a
noFunctionQualifiers cid =
  typeError (cIdentToPos cid) $
    printf "Function declarations can't take any qualifiers other than types and pixel layout definition."

trailingSemicolon ∷ CIdent → TCM a
trailingSemicolon cid =
  syntaxError (cIdentToPos cid) $
    printf "Top level function declarations must not have trailing semicolon."

notAssigned ∷ CIdent → TCM a
notAssigned cid =
  compileError (cIdentToPos cid) $
    printf "Variable \"%s\" used before assignment." (cIdentToString cid)

externalInMainOnly ∷ CIdent → TCM a
externalInMainOnly cid =
  typeError (cIdentToPos cid) $
    printf "Externals may only declared in main function"

-- | Throw a type error
typeError ∷ Position → String → TCM a
typeError = absError TypeError

compileError ∷ Position → String → TCM a
compileError = absError CompileError

syntaxError ∷ Position → String → TCM a
syntaxError = absError SyntaxError

debugError ∷ String → TCM a
debugError = absError CompileError (-1,-1)

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
  paramType (ParamDefault qs _ _ _) = qualType qs

warning ∷ Position → String → TCM ()
warning p m = do
  file ← gets currentFile
  let loc = (file, p)
  modify (\st → st { warnings = (loc,m) : warnings st })

-- adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
