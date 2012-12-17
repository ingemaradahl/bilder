{-# LANGUAGE UnicodeSyntax #-}

module Compiler.SimpleInliner where

import Prelude hiding (mapM)

import Control.Monad.State hiding (mapM)

import Compiler.Simple.AbsSimple
import Compiler.Simple.Types
import Compiler.Simple.Utils
import Utils

import qualified Data.Map as Map
import Data.Maybe
import Data.Traversable


-- Inlines function only concisting of SReturn exp
simpleInline ∷ Shader → Shader
simpleInline ss = execState (mapM simpleInlineFun (functions ss)) ss

simpleInlineFun ∷ Function → State Shader ()
simpleInlineFun f = do
  stms ← mapM (mapStmExpM simpleInlineExp) $ statements f
  modify (\st → st { functions = Map.insert name (f { statements = stms}) (functions st) })
 where
  name = functionName f

simpleInlineExp ∷ Exp → State Shader Exp
simpleInlineExp e@(ECall f es) = do
  es' ← mapM simpleInlineExp es
  funs ← gets functions
  case Map.lookup f funs >>= (\x → mayhaps (simpleEnough x) x) of
    Just fun → do
      let (SReturn ex) = head (statements fun)
      return $ replaceVars
                  (Map.fromList $ zip (map variableName (parameters fun)) es')
                  ex
    Nothing → return e
simpleInlineExp e = mapExpM simpleInlineExp e

replaceVars ∷ Map.Map String Exp → Exp → Exp
replaceVars trans = replace
 where
  replace (EVar v) = fromMaybe (EVar v) (Map.lookup v trans)
  replace (ECall n es) = fromMaybe (ECall n (map (replaceVars trans) es)) (Map.lookup n trans >>= (\(EVar v) → Just $ ECall v (map (replaceVars trans) es)))
  replace e = mapExp replace e

-- Sanity check
simpleEnough ∷ Function → Bool
simpleEnough f | countStms f == 1 =
  case head (statements f) of
    SReturn _ → True
    _ → False
simpleEnough _ = False

countStms ∷ Function → Int
countStms = length . statements
