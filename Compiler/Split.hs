{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Split where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Control.Arrow

import Data.Maybe
import qualified Data.Map as Map
import Data.List (nub, intercalate)

import Compiler.Utils
import TypeChecker.Utils

import FrontEnd.PrintGrammar

import Compiler.Dependencies (stmDeps, expDeps, Dep(Fun), DepList)

import TypeChecker.Types as Types hiding (
    functionName
  , statements
  , retType
  , paramVars
  , functions
  , variables
  )
import qualified TypeChecker.Types as Source (Source(functions), Source(variables))
import qualified TypeChecker.Types as Function (
    functionName
  , statements
  , retType
  , paramVars
  )

import FrontEnd.AbsGrammar

import Text.Printf

type Chunk = ([(SlimFun, [Stm])], String, SlimFun)

data SlimFun = SlimFun {
    functionName ∷ String
  , retType ∷ Type
  , args ∷ [SlimVar]
  , statements ∷ [Stm]
}
 deriving (Eq)

instance Show SlimFun where
 show (SlimFun name ret params stms) = printf "%s %s(%s)\n{%s\n}"
  (show ret)
  name
  (show params)
  (concatMap printTree stms)

data SlimVar = SlimVar {
    varName ∷ String
  , varType ∷ Type
}
 deriving (Show, Eq)

data Shader = Shader {
    funs ∷ Map.Map String SlimFun
  , vars ∷ Map.Map String SlimVar
  , output ∷ String
  , inputs ∷ [SlimVar]
}

instance Show Shader where
  show (Shader fs vs o is) =
    printf "-x- Shader (in %s) (out %s)\n -- Variables -------------\n%s\n -- Functions ----------\n%s\n"
      (show is)
      (show o)
      (show vs)
      (show fs)


data St = St {
    functions ∷ Map.Map String SlimFun
  , variables ∷ Map.Map String SlimVar
  , currentFun ∷ SlimFun
  , gobbled ∷ [(SlimFun, [Stm])]
  , freeRefs ∷ [Int]
  , chunks ∷ [Chunk]
  , dependencies ∷ [Dep]
}

pushFun ∷ SlimFun → State St ()
pushFun f = modify (\st → st { gobbled = (f,[]):gobbled st, currentFun = f})

popFun ∷ State St ()
popFun = do
  modify (\st → st { gobbled = tail (gobbled st)})
  -- i don't even
  modify (\st → st { currentFun = fst (head (gobbled st)) })

addStm ∷ Stm → State St ()
addStm stm = do
  gobs ← gets gobbled
  f ← gets currentFun
  stejt ← gets gobbled
  if null gobs
    then modify (\st → st { gobbled = [(f, [stm])]})
    else unless (f == fst (head gobs)) (error $ printf "TRIST FEL VA? ⊃:\nska lägga till:%s \n\ncurrentFun\n%s\n\ngobblefun:\n%s\n\nstate:\n%s" (show stm) (show f) (show (fst (head gobs))) (intercalate "\n" (map (show . first functionName) stejt))) >> 
          modify (\st → st { gobbled = (f, stm:snd (head gobs)):tail (gobbled st)})

getFun ∷ String → State St SlimFun
getFun s = liftM (fromJust . Map.lookup s) $ gets functions

getFunMaybe ∷ String → State St (Maybe SlimFun)
getFunMaybe s = liftM (Map.lookup s) $ gets functions

gather ∷ Monoid a => (Exp → Writer a Exp) → [Stm] → a
gather f ss = execWriter (mapM_ (mapStmExpM f) ss)

-- Find all references in this functions
refs ∷ SlimFun → [(String, Type)]
refs f = gather collect (statements f)
 where
  collect ∷ Exp → Writer [(String, Type)] Exp
  collect e@(EVarType cid t) = tell [(cIdentToString cid, t)] >> return e
  collect e = return e

-- Get a list of all calls made
calls ∷ [Stm] → [String]
calls = nub . gather collect
 where
  collect ∷ Exp → Writer [String] Exp
  collect e@(ECall cid _) = tell [cIdentToString cid] >> return e
  collect e = return e

-- Get a list of all variables references
usedVars ∷ [Stm] → [String]
usedVars = nub . gather collect
 where
  collect ∷ Exp → Writer [String] Exp
  collect e@(EVar cid) = tell [cIdentToString cid] >> return e
  collect e = return e

-- Strips arguments not needed
stripArgs ∷ SlimFun → SlimFun
stripArgs f = f { args = filter
                          (\v → varName v `notElem` usedVars (statements f))
                          (args f)
                }

stripFun ∷ Function → SlimFun
stripFun f = SlimFun {
    functionName = Function.functionName f
  , retType = Function.retType f
  , args = map stripVar $ Function.paramVars f
  , statements = Function.statements f
}

stripVar ∷ Variable → SlimVar
stripVar (Variable name _ t) = SlimVar name t


splitSource ∷ Source → [Shader]
splitSource src = evalState (split mainFun)
  St {
      functions = Map.map stripFun $ Source.functions src
    , variables = Map.map stripVar $ Source.variables src
    , currentFun = mainFun
    , gobbled = []
    , freeRefs = [1..]
    , chunks = []
    , dependencies = []
  }
 where
  mainFun ∷ SlimFun
  mainFun = stripFun $ fromJust $ Map.lookup "main" (Source.functions src)

newRef ∷ String → State St String
newRef s = do
  newId ← gets (head . freeRefs)
  modify (\st → st { freeRefs = tail (freeRefs st)})
  return $ printf "img%03d%s" newId s

split ∷ SlimFun → State St [Shader]
split fun = do
  mainShd ← collectMain fun
  shaders ← gets chunks >>= mapM buildShader

  repeatSplit $ mainShd:shaders

repeatSplit ∷ [Shader] → State St [Shader]
repeatSplit = return -- TODO check for additional partial applications..

buildShader ∷ Chunk → State St Shader
buildShader (stms,ref,fun) = do
  let fs = Map.fromList $ (functionName fun, fun) : map ((functionName &&& stripArgs) . buildFun) stms
  -- fetch missing stuff from state
  return Shader {
      funs = fs
    , vars = Map.empty --TODO
    , output = ref
    , inputs = nub $ concat [ findExternals (statements f) | (_,f) ← Map.toList fs ]
  }

buildFun ∷ (SlimFun, [Stm]) → SlimFun
buildFun (f, ss) = f { statements = ss }

collectMain ∷ SlimFun → State St Shader
collectMain fun = do
  modify (\st → st { gobbled = [(fun,[])], currentFun = fun })
  mapM_ gobbleStm (statements fun)
  gets (snd . head . gobbled) >>= buildMain fun

buildMain ∷ SlimFun → [Stm] → State St Shader
buildMain oldMain stms = do
  let mainFun = oldMain { statements = reverse stms }

  modify (\st → st { gobbled = [(mainFun,stms)], dependencies = [] })
  mapM_ (uncurry addBoth) $ stmDeps $ head stms


  stms' ← depends []
  let fs' = Map.fromList $ map (\(f, st) → (functionName f, f { statements = st})) stms'
  let mainFun' = fromJust $ Map.lookup "main" fs'

  return Shader {
      funs = fs'
    , vars = Map.empty
    , inputs = findExternals (statements mainFun')
    , output = "result_image"
  }

findExternals ∷ [Stm] → [SlimVar]
findExternals ss = execWriter (mapM_  (mapStmM findExternal) ss >>
  mapM_ (mapStmExpM findEVarTypes ) ss)

findExternal ∷ Stm → Writer [SlimVar] Stm
findExternal s@(SDecl (Dec qs (Vars [cid]))) | any isExternal qs =
  tell [SlimVar (cIdentToString cid) (qualsToType qs)] >> return s
findExternal s = mapStmM findExternal s

isExternal ∷ Qualifier → Bool
isExternal (QExternal _) = True
isExternal _ = False

findEVarTypes ∷ Exp → Writer [SlimVar] Exp
findEVarTypes e@(EVarType cid t) = tell [SlimVar (cIdentToString cid) t] >>
  return e
findEVarTypes e = return e

collectRewrite ∷ SlimFun → State St ()
collectRewrite fun = do
  pushFun fun
  mapM_ gobbleStm (statements fun)
  popFun

gobbleStm ∷ Stm → State St ()
gobbleStm stm = mapStmExpM gobble stm >>= addStm

gobble ∷ Exp → State St Exp
gobble (EPartCall cid es _) = do
  f ← getFun name
  d ← depends es
  -- add a SReturn to the main function.
  let d' = map (\(fun, ss) → (f, addSReturn fun ss name es)) d
  r ← newRef name
  addChunk (d',r,f)
  return (EVarType (CIdent ((0,0),r)) TImage)
 where
  name = cIdentToString cid
gobble e@(ECall cid _) = do
  fun ← getFunMaybe (cIdentToString cid)
  case fun of
    Nothing → return e
    Just f  → collectRewrite f >> return e
gobble e = return e

addChunk ∷ Chunk → State St ()
addChunk c = modify (\st → st { chunks = c:chunks st })

createsImg ∷ Stm → Bool
createsImg (SDecl (Dec qs (DecAss _ _ (EPartCall {})))) = qualsToType qs == imgType
createsImg (SExp (EAss _ _ (EPartCall {}))) = True
createsImg (SType t s) = t == imgType && createsImg s
createsImg _ = False

branches ∷ Exp → Bool
branches = isJust . branchTarget

branchTarget ∷ Exp → Maybe String
branchTarget = foldExp f Nothing
 where
  f ∷ Maybe String → Exp → Maybe String
  f p (ECall cid _) = p `mplus` Just (cIdentToString cid)
  f p _ = p

imgType ∷ Type
imgType = TFun TVec4 [TFloat, TFloat]

-- Dependency helpers {{{
depends ∷ [Exp] → State St [(SlimFun, [Stm])]
depends es = do
  -- add all initial dependencies.
  mapM_ (mapM_ (uncurry addDeps) . expDeps) es

  -- find all needed dependencies
  gb ← gets gobbled
  deps ← mapM (\(f,stms) → (,) <$> pure f <*> foldM isNeeded [] stms) gb
  (deps ++) <$> neededFuns

-- | Adds a return-statement calling the given function.
addSReturn ∷ SlimFun → [Stm] → String → [Exp] → [Stm]
addSReturn f stms n es =
  if functionName f == "main"
    then stms ++ [SReturn (TkReturn ((0,0),"return")) ecall]
    else stms
 where
  ecall = ECall cid (es ++ map fattenVar (args f))
  cid = CIdent ((0,0),n)
  fattenVar ∷ SlimVar → Exp
  fattenVar s = EVar (CIdent ((0,0),varName s))

-- | Returns all the functions that the state depends on.
neededFuns ∷ State St [(SlimFun, [Stm])]
neededFuns = do
  deps ← gets dependencies >>= filterM isFun
  sequence [
    getFun f >>= (\fun → return (fun, statements fun))
    | (Fun f) ← deps
    ]
 where
  isFun ∷ Dep → State St Bool
  isFun (Fun name) = do
    -- handles both "made up"-functions (saturated partial applications)
    --    and built in functions.
    fun ← getFunMaybe name
    case fun of
      Nothing → return False
      Just _  → return True
  isFun _ = return False

addDeps ∷ Dep → [Dep] → State St ()
addDeps _ = mapM_ add

addAffected ∷ Dep → [Dep] → State St ()
addAffected d _ = add d

addBoth ∷ Dep → [Dep] → State St ()
addBoth d ds = mapM_ add (d : ds)

add ∷ Dep → State St ()
add d = do
  deps ← gets dependencies
  modify (\s → s { dependencies = nub $ d : deps })

isNeeded ∷ [Stm] → Stm → State St [Stm]
isNeeded p stm = do
  let stmdeps = stmDeps stm
  deps ← gets dependencies
  if isReturn stm || True `elem` [a `elem` deps | a ← affected stmdeps]
    then do
      mapM_ (uncurry addDeps) stmdeps
      return $ stm:p
    else return p
 where
  affected ∷ DepList → [Dep]
  affected = map fst
  isReturn ∷ Stm → Bool
  isReturn (SType _ s) = isReturn s
  isReturn (SReturn {}) = True
  isReturn _ = False
