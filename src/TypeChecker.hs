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

module TypeChecker where

-- Imports {{{
import Prelude hiding (lookup)

import Control.Monad
import Control.Monad.Trans.State
import Control.Applicative hiding (empty)

import Data.Tree
import Data.Map (Map, elems, insertWith, empty, fromList, toList)
import Data.Foldable (fold)

import Text.Printf

import Utils

import TypeChecker.TCM
import TypeChecker.TCM.Errors
import TypeChecker.TCM.Utils

import TypeChecker.Inferring
import TypeChecker.Renamer (rename, strip)

import TypeChecker.Utils
import TypeChecker.Environment hiding (pushScope, popScope)
import qualified TypeChecker.Scope as Scope (functions)
import TypeChecker.Types as Types
import TypeChecker.Types.Blob

import Compiler hiding (Environment, Env, options, buildEnv, warnings)
import CompilerTypes
import FrontEnd.AbsGrammar as Abs
import FrontEnd.Instances
import CompilerError
-- }}}

-- | Typechecks the given abstract source and annotates the syntax tree. Also
-- performs renaming of identifiers, making all variable and function names
-- unique
typeCheck ∷ Options → Tree (FilePath, AbsTree) → CError ([Warning],Source)
typeCheck opts tree = do
  (sourceTree, st) ← runStateT
    (traverse checkFile tree >>= (\t → check t >> traverse rename t)) (buildEnv opts)
  return (warnings st, renameFunctions (fold (fmap strip sourceTree)))
 where
  check srcTree = unless (rootLabel srcTree `exports` main) noEntryPoint
  rootFile = (fst . rootLabel) tree
  loc = (rootFile,(-1,-1))
  main = Types.Function "main" "" loc TVec4 False [x,y] [] []
  x = Variable "x" loc TFloat Nothing
  y = Variable "y" loc TFloat Nothing
  noEntryPoint ∷ TCM a
  noEntryPoint = typeError (-1,-1) $
    printf ("No entrypoint \"main\" of " ++
      "type Float,Float found in %s")
      rootFile

renameFunctions ∷ Source → Source
renameFunctions src = src {
    Types.functions = fromList (map (\(_,fun) → (alias fun, fun { functionName = alias fun })) $ toList (Types.functions src))
  }

checkFile ∷ (FilePath, AbsTree) → [Tree Blob] → TCM Blob
checkFile (file, tree) children = do
  newFile file

  initScope children
  pushScope

  addTypedefs tree
  checkTopDecls tree
  addFunctions tree

  -- Return blob with new annotated functions
  checkFunctions >>= makeBlob

-- Type definitions {{{
-- | Adds type definitions to the to the state
addTypedefs ∷ AbsTree → TCM ()
addTypedefs (AbsTree tree) = sequence_
  [ filterTDef typ >>= addTypeIdentTypedef name | (TypeDef _ name _ typ) ← tree ]

-- }}}
-- Functions {{{
-- | Adds function type definitions to the state
addFunctions ∷ AbsTree → TCM ()
addFunctions (AbsTree tree) = do
  -- Check for top level functions with trailing semicolons.
  sequence_ [ trailingSemicolon cid | Abs.TopDecl (Dec _ (DecFun cid _ _)) ← tree ]

  -- Checks function parameter types, and adds function to environment
  funs ← sequence [ tcFun f | f@(Abs.Function {}) ← tree ]
  mapM_ addFunction funs

-- | Check functions for typing errors, returning the type annotated functions
checkFunctions ∷ TCM (Map String [Function])
checkFunctions = do
  funs ← gets (Scope.functions . head . scopes)
    >>= mapM checkFunction . concat . Data.Map.elems
  return $ foldr (\f p → insertWith (++) (ident f) [f] p) empty funs

-- | Checks a function for type errors, and returns the annotated function
checkFunction ∷ Function → TCM Function
checkFunction fun = do
  oldfun ← gets currentFunction
  pushScope
  updateFunction fun

  -- Add parameters to scope
  mapM_ addVariable (paramVars fun)

  -- Check that eventual parameter assignments are correct
  mapM_ checkParam (zip (parameters fun) (paramVars fun))

  -- Check and annotate statements
  statements' ← checkStatements (statements fun)

  unless (checkReturns statements') $ noReturnError fun

  popScope
  updateFunction oldfun
  return fun { statements = statements' }

-- }}}
-- Parameters {{{
checkParam ∷ (Param, Variable) → TCM ()
checkParam (p,v) = checkParam' p >>= flip unless error'
 where
  checkParam' ∷ Param → TCM Bool
  checkParam' (ParamDefault _ _ _ e) = liftM (varType v ==) (inferExp e)
  checkParam' _  = return True
  error' = paramExp p >>= inferExp >>= paramExpectedTypeError p
-- }}}
-- Statements {{{
checkStatements ∷ [Stm] → TCM [Stm]
checkStatements (s:[]) = sequence [checkStatement s]
checkStatements (s:ss) | returns s = warn >> sequence [checkStatement s]
                       | otherwise = (:) <$> checkStatement s <*> checkStatements ss
 where
  warn ∷ TCM ()
  warn = modify (\st → st { warnings = warnings st ++
    [((currentFile st, stmPos $ head ss), "Unreachable statements: " ++ show ss)] })
checkStatements [] = return []


checkStatement ∷ Stm → TCM Stm
checkStatement s@(SVoidReturn {}) = return $ SType TVoid s
checkStatement s@(SReturn (TkReturn (pos,_)) e) = do
  fun ← gets currentFunction
  t ← inferExp e
  if retType fun == t
    then return $ SType t s
    else returnMismatch pos t
checkStatement (SDecl decl@(Dec qs (DecFun cid ps stms))) = do
  currFun ← gets currentFunction
  tdecl@(TFun rt at) ← checkDecl decl
  ps' ← mapM paramToVar ps

  when (not (okForPixelQuals rt at) && any isQPixel qs) $ pixelQualsOnImageonly cid

  addCIdentVariable cid tdecl Nothing
  -- Check the declared function.
  file ← gets currentFile
  let fun = Types.Function {
    functionName = cIdentToString cid,
    alias = "",
    functionLocation = (file, cIdentToPos cid),
    retType = rt,
    pixelwise = okForPixelQuals rt at && (any isQPixelWise qs || (pixelwise currFun && none isQBounded qs)),
    paramVars = ps',
    parameters = ps,
    statements = stms
  }
  addFunction fun
  fun' ← checkFunction fun

  let ibool = if pixelwise fun then ITrue else IFalse

  setCIdentAssigned cid

  return $ SFunDecl cid tdecl ibool ps (statements fun')
checkStatement s@(SDecl decl) = SType <$> checkDecl decl <*> pure s
checkStatement (SIf tk@(TkIf (pos,_)) cond stm) = do
  condt ← inferExp cond >>= filterTDef
  unless (condt `elem` [TInt,TFloat,TBool]) $ badConditional condt pos
  SType condt <$> SIf tk cond <$> scopeCheckStatement stm
checkStatement (SIfElse tkif@(TkIf (pos,_)) econd strue tkelse sfalse) = do
  tecond ← inferExp econd >>= filterTDef
  unless (tecond `elem` [TInt,TFloat,TBool]) $ badConditional tecond pos
  SType tecond <$> (SIfElse tkif econd <$> scopeCheckStatement strue <*> pure tkelse <*> scopeCheckStatement sfalse)
checkStatement (SBlock stms) = do
  pushScope
  stms' ← checkStatements stms
  popScope
  return $ SBlock stms'
checkStatement s@(SExp e) = SType <$> inferExp e <*> pure s
checkStatement (SWhile tk e s) = do
  te ← inferExp e
  unless (te `elem` [TBool,TInt,TFloat]) $ badConditional te (tkpos tk)
  SType te <$> SWhile tk e <$> scopeCheckStatement s
checkStatement (SDoWhile tkdo s tkwhile e) = do
  te ← inferExp e
  unless (te `elem` [TBool,TInt,TFloat]) $ badConditional te (tkpos tkwhile)
  SType te <$> (SDoWhile tkdo <$> scopeCheckStatement s <*> pure tkwhile <*> pure e)
checkStatement (SFor tk fdecls econs eloop stm) = do
  mapM_ checkForDecl fdecls
  tecons ← mapM inferExp econs
  sequence_ [ unless (t `elem` [TBool,TInt,TFloat]) $ badConditional t (tkpos tk) | t ← tecons ]
  mapM_ inferExp eloop
  SFor tk fdecls econs eloop <$> scopeCheckStatement stm
checkStatement s = debugError $ show s ++ " NOT DEFINED"

scopeCheckStatement ∷ Stm → TCM Stm
scopeCheckStatement s = do
  pushScope
  s' ← checkStatement s
  popScope
  return s'
-- }}}
-- Declarations {{{
checkDecl ∷ Decl → TCM Type
checkDecl (Dec qs (DecFun cid ps _)) = do
  t ← verifyQualsType qs >>= filterTDef
  sequence_ [ unless (isQType q || isQPixel q) $ noFunctionQualifiers cid | q ← qs ]
  tps ← mapM paramType ps >>= mapM filterTDef
  when (not (okForPixelQuals t tps) && any isQPixel qs) $ pixelQualsOnImageonly cid
  return (TFun t tps)
checkDecl (Dec qs post) = do
  t ← liftM uncurryType $ verifyQualsType qs >>= filterTDef

  sequence_ [ addCIdentVariable cid t (declPostExp post) >> denyExternals qs cid | cid ← declPostIdents post ]

  -- if it's external - it's already assigned
  when (isExternal qs) $ mapM_ setCIdentAssigned $ declPostIdents post

  -- Check that inferred type and declared type matches
  expT ← checkDecAss post
  maybe (return ())
    (\a → unless (t == a) $ decAssError (head $ declPostIdents post) a t) expT

  return t

checkTopDecls ∷ AbsTree → TCM ()
checkTopDecls (AbsTree tree) =
  sequence_ [ checkDecl d >> mapM_ setCIdentAssigned (declPostIdents post) | (TopDecl d@(Dec _ post)) ← tree ]

checkForDecl ∷ ForDecl → TCM Type
checkForDecl (FDecl decl) = checkDecl decl
checkForDecl (FExp e) = inferExp e

checkDecAss ∷ DeclPost → TCM (Maybe Type)
checkDecAss (Vars _) = return Nothing
checkDecAss (DecAss cids _ e) =
  mapM_ setCIdentAssigned cids >> fmap Just (inferExp e)

-- }}}

-- vi:fdm=marker
