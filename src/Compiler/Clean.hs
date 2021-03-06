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

module Compiler.Clean where

import Compiler.Simple.Types
import Compiler.Simple.AbsSimple
import Compiler.Simple.Utils

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative

import Data.List
import Data.Maybe
import qualified Data.Map as Map

data Dep =
    Var String
  | Fun String
  | None
 deriving (Ord,Eq,Show)

type DepList = [(Dep, [Dep])]

type Dependencies = [Dep]

type Functions = Map.Map String Function
type Variables = Map.Map String Variable

-- | Cleans all the functions in all shaders from unnecessary statements.
clean ∷ Shader → Shader
clean sh = sh {
    variables = Map.filterWithKey (\k _ → k `elem` referenced) (variables sh),
    functions = newFuns,
    inputs = Map.filterWithKey (\k _ → k `elem` referenced) (inputs sh)
  }
 where
  newFuns = dropArgs $ dropFuns (Map.map (cleanFun (variables sh)) (functions sh))
  referenced = usedVars (concatMap statements (Map.elems $ functions sh)) ++ -- variables in functions
                concatMap varUsedVars (Map.elems $ variables sh) -- expressions in top level variables

dropFuns ∷ Functions → Functions
dropFuns funs = Map.filterWithKey (\k _ → k `elem` findCalls mainf ["main"]) funs
 where
  mainf = fromJust $ Map.lookup "main" funs
  findCalls ∷ Function → [String] → [String]
  findCalls fun known =
    let refs = map functionName $ mapMaybe (`Map.lookup` funs) (nub $ calls $ statements fun) in
    let new = refs \\ known in
    let known' = new ++ known in
    let toCheck = mapMaybe (`Map.lookup` funs) new in
    nub (concatMap (`findCalls` known) toCheck) ++ known'

-- | Removes unreferenced function arguments.
dropArgs ∷ Functions → Functions
dropArgs funs = Map.foldlWithKey drops funs funs
 where
  drops ∷ Functions → String → Function → Functions
  drops fs name _ = Map.insert name f' fs'
   where
    f = fromJust (Map.lookup name fs)
    (f', args') = dropFunArgs f
    fs' = Map.map replaceStms fs
    replaceStms fun = fun { statements = map (mapStmExp (replaceCalls name args')) (statements fun) }

replaceCalls ∷ String → [Bool] → Exp → Exp
replaceCalls name keep ecall@(ECall ident es) =
  if name == ident
    then ECall ident kept
    else ecall
 where
  es' = map (replaceCalls name keep) es
  kept = [ e | (e, k) ← zip es' keep, k ]
replaceCalls name keep e = mapExp (replaceCalls name keep) e

dropFunArgs ∷ Function → (Function, [Bool])
dropFunArgs fun = (fun', map keep args)
 where
  isCoordinate a = length a == 5 && ("x" `isSuffixOf` a || "y" `isSuffixOf` a)
  args = parameters fun
  referenced = usedVars $ statements fun
  keep a = variableName a `elem` referenced || isCoordinate (variableName a)
  fun' = fun { parameters = filter keep args }

-- | Removes unnecessary statements from a function.
cleanFun ∷ Variables → Function → Function
cleanFun globs fun = fun { statements = cleaned }
 where
  stms = reverse $ statements fun
  a = assignedVars stms
  b = "gl_FragColor":"fl_Resolution":Map.keys globs
  cleaned = evalState (needed (head stms) (tail stms)) (map Var $ a `intersect` b)

-- | Returns a list of needed statements given a starting statement.
needed ∷ Stm → [Stm] → State Dependencies [Stm]
needed start stms = do
  -- add dependencies for the start statement.
  mapM_ (uncurry addDeps) $ stmDeps start
  (++[start]) <$> foldM isNeeded [] stms

isNeeded ∷ [Stm] → Stm → State Dependencies [Stm]
isNeeded p stm = do
  -- clean inner statements
  stm' <- case stm of
    -- ds, cs and ls need not be cleaned - cs and ls may only change variables declared in ds
    (SFor ds cs ls ss) → SFor ds cs ls <$> foldM isNeeded [] (reverse ss)
    -- some with e
    (SWhile e ss) → SWhile e <$> foldM isNeeded [] (reverse ss)
    (SDoWhile ss e) → SDoWhile <$> foldM isNeeded [] (reverse ss) <*> pure e
    (SIf e ss) → SIf e <$> foldM isNeeded [] (reverse ss)
    (SIfElse e trues falses) → SIfElse e <$> foldM isNeeded [] (reverse trues) <*> foldM isNeeded [] (reverse falses)
    _ → return stm

  let stmdeps = stmDeps stm'
  deps ← get
  if isReturn stm' || hasECall stm' || True `elem` [a `elem` deps | a ← affected stmdeps]
    then do
      mapM_ (uncurry addDeps) stmdeps
      return $ stm':p
    else return p
 where
  affected ∷ DepList → [Dep]
  affected = map fst

isReturn ∷ Stm → Bool
isReturn = foldStm isret False
 where
  isret ∷ Bool → Stm → Bool
  isret _ (SReturn {}) = True
  isret pr _ = pr

-- TODO: (more complex solution instead of hasECall) if the function called
--    have assignments to global variables, do not remove it.
hasECall ∷ Stm → Bool
hasECall s = True `elem` execWriter (mapStmExpM hasecall s)
 where
  hasecall ∷ Exp → Writer [Bool] Exp
  hasecall e@(ECall {}) = tell [True] >> return e
  hasecall e = mapExpM hasecall e

addDeps ∷ Dep → [Dep] → State Dependencies ()
addDeps _ = mapM_ add

addAffected ∷ Dep → [Dep] → State Dependencies ()
addAffected d _ = add d

addBoth ∷ Dep → [Dep] → State Dependencies ()
addBoth d ds = mapM_ add (d : ds)

add ∷ Dep → State Dependencies ()
add d = get >>= \ds → put $ nub (d:ds)

-- Dependency calculation {{{
-- | Returns a list of all affected variables and their dependencies.
stmDeps ∷ Stm → DepList
stmDeps (SDecl var) = [(Var (variableName var), [])]
stmDeps (SDeclAss var e) =
  (Var (variableName var), concatMap snd (expDeps e)) : onlyAssDeps (expDeps e)
stmDeps (SExp e) = expDeps e
stmDeps (SWhile e ss) = expDeps e ++ concatMap stmDeps ss
stmDeps (SDoWhile ss e) = concatMap stmDeps ss ++ expDeps e
stmDeps (SFor decls cones loopes ss) =
  concatMap stmDeps (decls ++ reverse ss) ++ concatMap expDeps (cones ++ loopes)
stmDeps (SReturn e) = expDeps e
stmDeps (SVoidReturn) = []
stmDeps (SIf e ss) = expDeps e ++ concatMap stmDeps ss
stmDeps (SIfElse e trues falses) = expDeps e ++ concatMap stmDeps (trues ++ falses)
stmDeps (SBreak) = []
stmDeps (SContinue) = []
stmDeps (SDiscard) = []

expDeps ∷ Exp → DepList
expDeps (EAss el er) = expAss el er
expDeps (EAssAdd el er) = expAss el er
expDeps (EAssSub el er) = expAss el er
expDeps (EAssMul el er) = expAss el er
expDeps (EAssDiv el er) = expAss el er
expDeps (EAssMod el er) = expAss el er
expDeps (EAssBWAnd el er) = expAss el er
expDeps (EAssBWXOR el er) = expAss el er
expDeps (EAssBWOR el er) = expAss el er
expDeps (ECond ec et ef) = concatMap expDeps [ec,et,ef]
expDeps (EOR el er) = concatMap expDeps [el,er]
expDeps (EXOR el er) = concatMap expDeps [el,er]
expDeps (EAnd el er) = concatMap expDeps [el,er]
expDeps (EBWOR el er) = concatMap expDeps [el,er]
expDeps (EBWXOR el er) = concatMap expDeps [el,er]
expDeps (EBWAnd el er) = concatMap expDeps [el,er]
expDeps (EEqual el er) = concatMap expDeps [el,er]
expDeps (ENEqual el er) = concatMap expDeps [el,er]
expDeps (ELt el er) = concatMap expDeps [el,er]
expDeps (EGt el er) = concatMap expDeps [el,er]
expDeps (ELEt el er) = concatMap expDeps [el,er]
expDeps (EGEt el er) = concatMap expDeps [el,er]
expDeps (EBWShiftLeft el er) = concatMap expDeps [el,er]
expDeps (EBWShiftRight el er) = concatMap expDeps [el,er]
expDeps (EAdd el er) = concatMap expDeps [el,er]
expDeps (ESub el er) = concatMap expDeps [el,er]
expDeps (EMul el er) = concatMap expDeps [el,er]
expDeps (EDiv el er) = concatMap expDeps [el,er]
expDeps (EMod el er) = concatMap expDeps [el,er]
expDeps (ENeg e) = expDeps e
expDeps (ENegSign e) = expDeps e
expDeps (EComplement e) = expDeps e
expDeps (EPos e) = expDeps e
expDeps (EPreInc e) = expDeps e
expDeps (EPreDec e) = expDeps e
expDeps (EPostInc e) = expDeps e
expDeps (EPostDec e) = expDeps e
-- Only non "real" members (e.g. xyz).
expDeps (EMember e _) = (None, concatMap snd e') : e'
 where
  e' = expDeps e
-- Only built in member calls (e.g. map).
expDeps (EMemberCall e _ es) =
  (None, concatMap snd es') : es'
 where
  es' = concatMap expDeps (e:es)
expDeps (ECall ident es) = (None, Var ident : Fun ident : concatMap (concatMap snd) es') : concatMap onlyAssDeps es'
 where
  es' = map expDeps es
expDeps (ETypeCall _ es) = concatMap expDeps es
expDeps (EVar ident) = [(None, [Var ident])]
expDeps (EIndex ei@(EIndex {}) e) = expDeps ei ++ expDeps e
expDeps (EIndex (EVar ident) e) = (None, Var ident : concatMap snd e') : e'
 where
  e' = expDeps e
expDeps (EFloat {}) = []
expDeps (EInt _) = []
expDeps (ETrue) = []
expDeps (EFalse) = []

expAss ∷ Exp → Exp → DepList
expAss (EMember (EVar ident) _) e =
  (Var ident, concatMap snd (expDeps e)) : onlyAssDeps (expDeps e)
expAss (EVar ident) e =
  (Var ident, concatMap snd (expDeps e)) : onlyAssDeps (expDeps e)
expAss (EIndex (EIndex (EVar ident) e1) e2) e =
  (Var ident, concatMap snd e') : onlyAssDeps e'
 where
  e' = expDeps e1 ++ expDeps e2 ++ expDeps e
expAss (EIndex (EVar ident) e) el =
  (Var ident, concatMap snd (el'++e')) : onlyAssDeps el' ++ e'
 where
  e' = expDeps e
  el' = expDeps el
expAss el er =
  -- el depends on all er's dependencies
  map (\(a,c) → (a,c++desper)) el'
  -- and the whole assignment depends on all dependencies in er'
  ++ er'
 where
  el' = expDeps el
  er' = expDeps er
  desper = concatMap snd er'

onlyAssDeps ∷ DepList → DepList
onlyAssDeps = filter ((/=) None. fst)
--}}}

-- vi:fdm=marker
