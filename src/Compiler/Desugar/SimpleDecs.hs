{-# LANGUAGE UnicodeSyntax #-}

module Compiler.Desugar.SimpleDecs where

import qualified Data.Map as Map (map)

import Compiler.Utils

import TypeChecker.Types
import FrontEnd.AbsGrammar

simpleDecs ∷ Source → Source
simpleDecs src = src { functions = Map.map simplifyFun (functions src)}

simplifyFun ∷ Function → Function
simplifyFun fun = fun { statements = simpleDec (statements fun )}

simpleDec ∷ [Stm] → [Stm]
simpleDec (SDecl (Dec qs ps):ss) = newDec qs ps ++ simpleDec ss
simpleDec ss = expandStm simpleDec ss

newDec ∷ [Qualifier] → DeclPost → [Stm]
newDec qs (Vars cids) = map (makeDec qs) cids
newDec qs (DecAss cids tk ex) = map (makeDec qs) (init cids) ++ [makeDecAss qs tk ex (last cids)]

makeDec ∷ [Qualifier] → CIdent → Stm
makeDec qs cid = SDecl (Dec qs (Vars [cid]))

makeDecAss ∷ [Qualifier] → TkAss → Exp → CIdent → Stm
makeDecAss qs tk ex cid = SDecl (Dec qs (DecAss [cid] tk ex))
