{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Clean where

import Compiler.Simple.Types
import Compiler.Simple.AbsSimple
import Compiler.Simple.Utils

import Control.Monad.State
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
clean sh = sh { variables = dropVars (variables sh) newFuns, functions = newFuns }
 where
  newFuns = dropFuns (Map.map (cleanFun (variables sh)) (functions sh))

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

dropVars ∷ Variables → Functions → Variables
dropVars vs funs = Map.filterWithKey (\k _ → k `elem` references) vs
 where
  references ∷ [String]
  references = nub $ concatMap (usedVars . statements) (Map.elems funs)

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
  let stmdeps = stmDeps stm
  deps ← get
  if isReturn stm || True `elem` [a `elem` deps | a ← affected stmdeps]
    then do
      mapM_ (uncurry addDeps) stmdeps
      return $ stm:p
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
  concatMap stmDeps (decls ++ ss) ++ concatMap expDeps (cones ++ loopes)
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
expDeps (EIndex ident e) = (None, Var ident : concatMap snd e') : e'
 where
  e' = expDeps e
expDeps (EFloat {}) = []
expDeps (EInt _) = []
expDeps (ETrue) = []
expDeps (EFalse) = []

expAss ∷ Exp → Exp → DepList
expAss (EVar ident) e =
  (Var ident, concatMap snd (expDeps e)) : onlyAssDeps (expDeps e)
expAss (EIndex ident e) el =
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
