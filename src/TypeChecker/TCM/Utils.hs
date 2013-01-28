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

module TypeChecker.TCM.Utils where

import Utils

import TypeChecker.TCM
import TypeChecker.TCM.Errors
import TypeChecker.Environment as Env
import qualified TypeChecker.Scope as Scope
import TypeChecker.Types as TC hiding (functions, variables)
import TypeChecker.Types.Blob (Blob, Blob (Blob))
import qualified TypeChecker.Types.Blob as Blob (functions, typedefs, variables)
import TypeChecker.Utils

import CompilerTypes

import FrontEnd.AbsGrammar as Abs

import Control.Monad.Trans.State
import Control.Monad.Error
import Control.Applicative

import Prelude hiding (lookup)
import Data.Map hiding (null)
import Data.Tree

import Text.Printf

-- | Adds a function to the environment, and makes sure there are no duplicates
addFunction ∷ Function → TCM ()
addFunction fun = do
  (s:ss) ← gets scopes
  let a = lookup name (Scope.functions s) >>= (\fs → mayhaps (fun `elem` fs) fs)
  case a of
    Just (f:_) → functionDefinedError f fun
    Nothing → modify (\st → st { scopes = Scope.addFunction fun s : ss })
 where
  name = ident fun ∷ String

lookupFunction ∷ String → TCM [Function]
lookupFunction f = do
  scs ← gets scopes
  case Scope.lookupFunction f scs of
    Just [] → maybe (return []) (return . pure) $ Scope.lookupVarFun f scs
    Just fs → return fs
    Nothing → maybe (return []) (return . pure) $ Scope.lookupVarFun f scs

-- | Adds a type definition to the environment
addTypedef ∷ Typedef → TCM ()
addTypedef def = do
  scs ← gets scopes
  def' ← Typedef (ident def) (location def) <$> filterTDef (typedefType def)
  case Scope.lookupTypedef (ident def') scs of
    Just t' → unless (t == typedefType t') (typedefError t' def')
    Nothing → modify (\st → st { scopes = Scope.addTypedef def' (head scs):tail scs })
 where
  t = uncurryType $ typedefType def

addTypeIdentTypedef ∷ TypeIdent → Type → TCM ()
addTypeIdentTypedef tid typeFunc = do
  file ← gets currentFile
  let def = Typedef name (file, pos) t
  addTypedef def
 where
  t = uncurryType typeFunc
  name = typeIdentToString tid
  pos = typeIdentToPos tid

lookupTypedef ∷ TypeIdent → TCM Typedef
lookupTypedef tid = do
  scs ← gets scopes
  case Scope.lookupTypedef name scs of
    Just t → return t
    Nothing → typedefNotFoundError tid
 where
  name = typeIdentToString tid

-- | Adds a variable to the current scope, making sure there are no duplicates
addVariable ∷ Variable → TCM ()
addVariable var = do
  (s:ss) ← gets scopes
  case lookup name (Scope.variables s) of
    Just _ → typeError pos $ printf "variable '%s' already defined" name
    Nothing → modify (\st → st { scopes = Scope.addVariable var s : ss } )
 where
  pos = position var ∷ Position
  name = ident var ∷ String

-- | Add a variable to the current scope, using currentFile from the state
addCIdentVariable ∷ CIdent → Type → Maybe Exp → TCM ()
addCIdentVariable cid t me = cIdentToVariable cid (uncurryType t) me >>= addVariable

cIdentToVariable ∷ CIdent → Type → Maybe Exp → TCM Variable
cIdentToVariable cid t me = Variable n <$> ((,) <$> gets currentFile <*> pure p) <*> pure t <*> pure me
 where
  n = cIdentToString cid
  p = cIdentToPos cid

-- | Lookup a variable denoted by it's name. May throw error
lookupVar ∷ CIdent → TCM Variable
lookupVar cid = do
  scs ← gets scopes
  case Scope.lookupVar name scs of
    Just v → return v
    Nothing → noVarFound cid
 where
  name = cIdentToString cid

-- | Lookup the possible types of a variable (or a function)
--   Never returns [] - instead it throws noVarFound.
lookupVarTypes ∷ CIdent → TCM [Type]
lookupVarTypes cid = do
  scs ← gets scopes
  -- check for a Variable with that name.
  case Scope.lookupVar name scs of
    Just v  → return [varType v]
    Nothing → case Scope.lookupFunction name scs of
      Just f  → return $ Prelude.map funType f
      Nothing → noVarFound cid
 where
  name = cIdentToString cid
  funType f = TFun (retType f) (Prelude.map varType $ paramVars f)

-- | Sets which file is currently checked
updateFile ∷ FilePath → TCM ()
updateFile f = modify (\st → st { currentFile = f })

-- | Sets which function is currently checked
updateFunction ∷ Function → TCM ()
updateFunction f = modify (\st → st { currentFunction = f })

-- | Pushes a new scope to the environment
pushScope ∷ TCM ()
pushScope = modify Env.pushScope

-- | Pops a scope off the environment
popScope ∷ TCM ()
popScope = modify Env.popScope

clearScope ∷ TCM ()
clearScope = modify (\st → st { scopes = [Scope.emptyScope, Scope.builtInScope] })

newFile ∷ FilePath → TCM ()
newFile f = updateFile f >> clearScope

makeBlob ∷ Map String [Function] → TCM Blob
makeBlob funs = do
  scope ← gets $ head . scopes
  file ← gets currentFile
  return $ Blob file funs (Scope.typedefs scope) (Scope.variables scope)

initScope ∷ [Tree Blob] → TCM ()
initScope = mapM_ (addBlob . rootLabel)
 where
  addBlob ∷ Blob → TCM ()
  addBlob b = do
    mergeFunctions (Blob.functions b)
    mergeVariables (Blob.variables b)
    mergeTypedefs (Blob.typedefs b)

mergeFunctions ∷ Map String [Function] → TCM ()
mergeFunctions funs = sequence_ [ mapM addFunction fs | (_,fs) ← toList funs]

mergeFunctions' ∷ Map String Function → TCM ()
mergeFunctions' funs = mapM_ (addFunction . snd) $ toList funs

mergeVariables ∷ Map String Variable → TCM ()
mergeVariables vars = do
  mapM_ addVariable $ elems vars
  mapM_ setAssigned $ keys vars

mergeTypedefs ∷ Map String Typedef → TCM ()
mergeTypedefs defs = mapM_ (addTypedef . snd) $ toList defs

tcFun ∷ Toplevel → TCM Function
tcFun (Abs.Function qs cident params stms) = do
  params' ← mapM paramToVar params
  retType' ← verifyQualsType qs >>= filterTDef
  sequence_ [ unless (isQType q || isQPixel q) $ noFunctionQualifiers cident | q ← qs ]
  unless (length (Prelude.filter isQPixel qs) <= 1) $ invalidQualList qs
  when (not (okForPixelQuals retType' (Prelude.map varType params')) && any isQPixel qs) $ pixelQualsOnImageonly cident
  file ← gets currentFile
  let fun = TC.Function {
    functionName = cIdentToString cident,
    alias = "",
    functionLocation = (file, cIdentToPos cident),
    retType = retType',
    pixelwise = any isQPixelWise qs,
    paramVars = params',
    parameters = params,
    statements = stms
  }
  updateFunction fun
  return fun
tcFun _ = compileError (-1,-1) "Non-function given as argument to tcFun"

verifyQuals ∷ [Qualifier] → TCM ()
verifyQuals qs = unless (null dups) $ invalidQualList qs
 where
  dups = duplicatesWith eq qs
  eq ∷ Qualifier → Qualifier → Bool
  eq (QExternal {}) (QExternal {}) = True
  eq (QConst {}) (QConst {}) = True
  eq (QPixelwise {}) (QPixelwise {}) = True
  eq (QBounded {}) (QBounded {}) = True
  eq a b = a == b

verifyQualsType ∷ [Qualifier] → TCM Type
verifyQualsType qs = verifyQuals qs >> maybe (qualsNoTypeGiven qs) return (qualType qs)

isQType ∷ Qualifier → Bool
isQType (QType {}) = True
isQType _ = False

isQPixel ∷ Qualifier → Bool
isQPixel q = isQPixelWise q || isQBounded q

isQPixelWise ∷ Qualifier → Bool
isQPixelWise (QPixelwise {}) = True
isQPixelWise _ = False

isQBounded ∷ Qualifier → Bool
isQBounded (QBounded {}) = True
isQBounded _ = False

isExternal ∷ [Qualifier] → Bool
isExternal [] = False
isExternal ((QExternal {}):_) = True
isExternal (_:rest) = isExternal rest

paramExp ∷ Param → TCM Exp
paramExp (ParamDefault _ _ _ e) = return e
paramExp p = compileError (paramToPos p)
  "Trying to find expression on non-expression parameter declaration"

paramType ∷ Param → TCM Type
paramType p = verifyQuals qs >> maybe (paramNoTypeGiven p) (return . uncurryType) (qualType qs)
 where
  qs = paramToQuals p

paramToVar ∷ Param → TCM Variable
paramToVar p = do
  varTyp ← paramType p >>= filterTDef
  file ← gets currentFile
  return $ Variable (paramToString p) (file, paramToPos p) varTyp Nothing

filterTDef ∷ Type → TCM Type
filterTDef (TDefined tid) = lookupTypedef tid >>= (filterTDef . typedefType)
filterTDef (TFun t ts) = TFun <$> filterTDef t <*> mapM filterTDef ts
filterTDef (TFunc tl arr tr) = TFunc <$> filterTDef tl <*> pure arr <*> filterTDef tr
filterTDef (TArray t) = TArray <$> filterTDef t
filterTDef (TConst t) = TConst <$> filterTDef t
filterTDef t = pure t

isAssigned ∷ CIdent → TCM Bool
isAssigned cid = Scope.isAssigned name <$> gets scopes
 where
  name = cIdentToString cid

setCIdentAssigned ∷ CIdent → TCM ()
setCIdentAssigned cid = do
  ss ← gets scopes
  let scs = Scope.setAssigned name ss
  modify (\s → s { scopes = scs })
 where
  name = cIdentToString cid

setAssigned ∷ String → TCM ()
setAssigned s = setCIdentAssigned (CIdent ((0,0),s))

denyExternals ∷ [Qualifier] → CIdent → TCM ()
denyExternals qs cid = do
  fun ← gets currentFunction
  let funName = case fun of
                  (TC.Function {}) → ident fun
                  _ → "undefined"
  when (funName /= "main" && isExternal qs) $ externalInMainOnly cid
