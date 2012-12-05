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

import Compiler.Dependencies (stmDeps, expDeps, Dep(Fun, Var), DepList)

import TypeChecker.Types as Types hiding (
    functionName
  , statements
  , retType
  , paramVars
  , functions
  , variables
  , varType
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
 show (SlimFun name ret params stms) = printf "%s %s(%s)\n{%s\n}\n"
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
  , gobbledFuns ∷ Map.Map String SlimFun
  , inlineAssigns ∷ Map.Map String [Stm]
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

splitShader ∷ Shader → State St [Shader]
splitShader sh = do
  cfree ← gets freeRefs
  let (ss, sta) = runState (split mainFun) St {
        functions = funs sh
      , variables = vars sh
      , currentFun = mainFun
      , gobbled = []
      , gobbledFuns = Map.empty
      , inlineAssigns = Map.empty
      , freeRefs = cfree
      , chunks = []
      , dependencies = []
    }
  modify (\st → st { freeRefs = freeRefs sta })
  return $ (head ss) { output = output sh } : tail ss
 where
  mainFun ∷ SlimFun
  mainFun = fromJust $ Map.lookup "main" (funs sh)

splitSource ∷ Source → [Shader]
splitSource src = evalState (split mainFun)
  St {
      functions = Map.map stripFun $ Source.functions src
    , variables = Map.map stripVar $ Source.variables src
    , currentFun = mainFun
    , gobbled = []
    , gobbledFuns = Map.empty
    , inlineAssigns = Map.empty
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
  modify (\st → st { dependencies = [] })
  shaders ← gets chunks >>= mapM buildShader

  liftM concat $ sequence [
    if hasImages s
      then splitShader s
      else return [s]
    | s ← mainShd:shaders
    ]

hasImages ∷ Shader → Bool
hasImages sh = True `elem` map createsImg stms
 where
  stms = concatMap (statements . snd) $ (Map.toList . funs) sh

buildShader ∷ Chunk → State St Shader
buildShader (gs,ref,fun) = do
  -- find all functions that will form the new "main".
  inlinable ← addAssignments $ reverse $ dropWhile (\(f,_) → functionName f /= "main") (reverse gs)
  let fs = Map.fromList $ (functionName fun, fun) : map ((functionName &&& stripArgs) . buildFun) gs
      mainFun = (fromJust $ Map.lookup "main" fs) { statements = concatMap snd (reverse inlinable) }
      -- fetch the rest of the functions that are not to be inlined.
      restFuns = map (\(f,_) → (functionName f, f)) (takeWhile (\(f,_) → functionName f /= "main") (reverse gs))

  return Shader {
      funs = Map.fromList $ ("main", mainFun) : restFuns
    , vars = Map.empty
    , output = ref
    , inputs = nub $ findExternals (statements mainFun)
  }

-- | Adds declerations and assignments for a functions arguments
--    needed when inlining the function into main.
addAssignments ∷ [(SlimFun, [Stm])] → State St [(SlimFun, [Stm])]
addAssignments gs = do
  is ← gets inlineAssigns
  sequence [
      if functionName f == "main"
        then return (f, ss)
        else case Map.lookup (functionName f) is of
          Nothing → return (f, ss)
          Just assigns → return (f, assigns ++ ss)
    | (f, ss) ← gs ]

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

  -- get rid of statements nothing depends on.
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

collectRewrite ∷ SlimFun → [Exp] → State St ()
collectRewrite fun es = do
  pushFun fun
  mapM_ gobbleStm (statements fun)
  -- store them for later use
  storeGobbledFun (functionName fun) assigns
  popFun
 where
  assigns = zipWith mkAss es (args fun)
  tkass = TkAss ((0,0),"=")
  mkAss ∷ Exp → SlimVar → Stm
  mkAss e v = SDecl (Dec [QType (varType v)] (DecAss [CIdent ((0,0),varName v)] tkass e))

storeGobbledFun ∷ String → [Stm] → State St ()
storeGobbledFun n ss = do
  gs ← gets gobbled
  let (fun, stms) = head $ filter ((==n) . functionName . fst) gs
  -- store gobbled version.
  gfs ← gets gobbledFuns
  modify (\st → st { gobbledFuns = Map.insert n (fun { statements = reverse stms }) gfs })
  -- store declarations + assignments needed for inlining.
  ifs ← gets inlineAssigns
  modify (\st → st { inlineAssigns = Map.insert n ss ifs })

gobbleStm ∷ Stm → State St ()
gobbleStm stm = mapStmExpM gobble stm >>= addStm

depFun ∷ SlimFun → State St ()
depFun f = do
  -- add the actual function.
  add (Fun name) >> add (Var name)
  mapM_ (uncurry addBoth) $ concatMap stmDeps (statements f)
  mapM_ (mapStmExpM addDepFun) (statements f)
 where
  name = functionName f

addDepFun ∷ Exp → State St Exp
addDepFun e@(EPartCall cid _ _) = do
  f ← getFun (cIdentToString cid)
  depFun f
  return e
addDepFun e = return e

gobble ∷ Exp → State St Exp
gobble (EPartCall cid es _) = do
  -- add a dependency to the called function (and all its dependencies).
  f ← getFun name

  modify (\st → st { dependencies = [] })
  depFun f

  -- calculate all the needed (already gobbled) statements.
  d ← depends es

  -- add a SReturn to the top function.
  let mainFun = fst $ head $ filter (\(fun, _) → functionName fun == "main") d
      d' = (\(fun, ss) → (fun, addSReturn mainFun ss name es)) (head d) : tail d
  r ← newRef name
  addChunk (d',r,f)
  return (EVarType (CIdent ((0,0),r)) TImage)
 where
  name = cIdentToString cid
gobble e@(ECall cid es) = do
  -- add dependencies for all the arguments passed to the function call.
  mapM_ (uncurry addBoth) $ concatMap expDeps es

  fun ← getFunMaybe (cIdentToString cid)
  case fun of
    Nothing → return e
    Just f  → collectRewrite f es >> return e
gobble e = mapExpM gobble e

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

  -- add all needed functions and map through them to get rid of unneeded stuff.
  (deps ++) <$> neededFuns >>= mapM (\(f,stms) → (,) <$> pure f <*> foldM isNeeded [] (reverse stms))

-- | Adds a return-statement calling the given function.
addSReturn ∷ SlimFun → [Stm] → String → [Exp] → [Stm]
addSReturn f stms n es = stms ++ [SReturn (TkReturn ((0,0),"return")) ecall]
 where
  ecall = ECall cid (es ++ map fattenVar (args f))
  cid = CIdent ((0,0),n)
  fattenVar ∷ SlimVar → Exp
  fattenVar s = EVar (CIdent ((0,0),varName s))

-- | Returns all the functions that the state depends on.
neededFuns ∷ State St [(SlimFun, [Stm])]
neededFuns = do
  deps ← gets dependencies >>= filterM isFun
  sequence [ do
    f' ← getFun f
    gfs ← gets gobbledFuns
    -- if it creates an image it has been gobbled and the new version should be used.
    if True `elem` map createsImg (statements f')
      then case Map.lookup f gfs of
        Just gf → return (gf, statements gf)
        -- unless it's an nestled partial application - then it's not yet gobbled.
        Nothing → return (f', statements f')
      else return (f', statements f')
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
  isReturn = foldStm isret False
   where
    isret ∷ Bool → Stm → Bool
    isret _ (SReturn {}) = True
    isret pr _ = pr
