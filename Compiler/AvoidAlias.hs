{-# LANGUAGE UnicodeSyntax #-}

module Compiler.AvoidAlias where

import qualified Data.Map as Map

import Compiler.Simple.AbsSimple
import Compiler.Simple.Types
import Compiler.Simple.Utils

-- | Strips the code of unneccecary variable aliases
avoidAlias ∷ Shader → Shader
avoidAlias sh = sh { functions = Map.map replaceFun (functions sh) }

replaceFun ∷ Function → Function
replaceFun fun = fun { statements = replaceStm (statements fun)}

replaceStm ∷ [Stm] → [Stm]
replaceStm (SDecl v:ss) | variableName v `notElem` usedVars ss =
  expandStm replaceStm ss
replaceStm (SDeclAss vl (EVar vr):ss) | vr `notElem` usedVars ss =
  let ss' = replaceVar (variableName vl) vr ss in
  expandStm replaceStm ss'
replaceStm (s:ss) = s:replaceStm ss
replaceStm [] = []

replaceVar ∷ String → String → [Stm] → [Stm]
replaceVar find new = map (mapStmExp replace)
 where
  replace ∷ Exp → Exp
  replace (EVar s) | s == find = EVar new
  replace (ECall s es) | s == find = ECall new $ map replace es
  replace e = mapExp replace e
