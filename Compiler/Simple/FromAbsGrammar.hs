{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses #-}

module Compiler.Simple.FromAbsGrammar where

import Compiler.Simple.AbsSimple
import qualified FrontEnd.AbsGrammar as G

import TypeChecker.Utils
import Data.Maybe

buildVariable ∷ [G.Qualifier] → G.CIdent → Variable
buildVariable qs cid = Variable
  (cIdentToString cid)
  (translate $ fromJust $ qualType qs)
  (any isConst qs)

stripBlock ∷ G.Stm → [Stm]
stripBlock (G.SBlock ss) = map translate ss
stripBlock s = [translate s]

class Translate a b where
  translate ∷ a → b

instance Translate G.Type Type where
 translate G.TInt = TInt
 translate G.TFloat = TFloat
 translate G.TBool = TBool
 translate G.TVoid = TVoid
 translate G.TVec2 = TVec2
 translate G.TVec3 = TVec3
 translate G.TVec4 = TVec4
 translate (G.TFun G.TVec4 [G.TFloat, G.TFloat]) = TSampler
 translate G.TImage = TSampler
 translate t = error $ "Not implemented: " ++ show t

instance Translate G.ForDecl Stm where
  translate (G.FDecl (G.Dec _ (G.Vars {}))) = error $
    "compiler error -- " ++
    "Only variable assignment allowed in first expression in for declaration"
  translate (G.FDecl (G.Dec qs (G.DecAss cids _ e))) | length cids > 1 = error $
    "compiler error -- " ++
    "Only one assignment allowed in first expression in for declaration"
                                                     | otherwise =
    SDeclAss (buildVariable qs (head cids)) (translate e)
  translate (G.FExp e) = SExp $ translate e

instance Translate G.Stm Stm where
  translate (G.SType _ s) = translate s
  translate (G.SDecl (G.Dec qs (G.Vars [cid]))) = SDecl (buildVariable qs cid)
  translate (G.SDecl (G.Dec qs (G.DecAss [cid] _ e))) =
    SDeclAss (buildVariable qs cid) (translate e)
  translate (G.SExp e) = SExp $ translate e
  translate (G.SWhile _ e s) = SWhile (translate e) (stripBlock s)
  translate (G.SDoWhile _ s _ e) = SDoWhile (stripBlock s) (translate e)
  translate (G.SFor _ fdec el er s) = SFor (map translate fdec)
    (map translate el) (map translate er) (stripBlock s)
  translate (G.SReturn _ e) = SReturn (translate e)
  translate (G.SVoidReturn _) = SVoidReturn
  translate (G.SIf _ e s) = SIf (translate e) (stripBlock s)
  translate (G.SIfElse _ e st _ sf) = SIfElse (translate e)
    (stripBlock st) (stripBlock sf)
  translate (G.SBreak _) = SBreak
  translate (G.SContinue _) = SContinue
  translate (G.SDiscard _) = SDiscard


instance Translate G.Exp Exp where
  --translate (G.1 el _ er) = 1 (translate el) (translate er)
  translate (G.EAss el _ er) = EAss (translate el) (translate er)
  translate (G.EAssAdd el _ er) = EAssAdd (translate el) (translate er)
  translate (G.EAssSub el _ er) = EAssSub (translate el) (translate er)
  translate (G.EAssMul el _ er) = EAssMul (translate el) (translate er)
  translate (G.EAssDiv el _ er) = EAssDiv (translate el) (translate er)
  translate (G.EAssMod el _ er) = EAssMod (translate el) (translate er)
  translate (G.EAssBWAnd el _ er) = EAssBWAnd (translate el) (translate er)
  translate (G.EAssBWXOR el _ er) = EAssBWXOR (translate el) (translate er)
  translate (G.EAssBWOR el _ er) = EAssBWOR (translate el) (translate er)
  translate (G.ECond ec _ et _ ef) = ECond (translate ec) (translate et) (translate ef)
  translate (G.EOR el _ er) = EOR (translate el) (translate er)
  translate (G.EXOR el _ er) = EXOR (translate el) (translate er)
  translate (G.EAnd el _ er) = EAnd (translate el) (translate er)
  translate (G.EBWOR el _ er) = EBWOR (translate el) (translate er)
  translate (G.EBWXOR el _ er) = EBWXOR (translate el) (translate er)
  translate (G.EBWAnd el _ er) = EBWAnd (translate el) (translate er)
  translate (G.EEqual el _ er) = EEqual (translate el) (translate er)
  translate (G.ENEqual el _ er) = ENEqual (translate el) (translate er)
  translate (G.ELt el _ er) = ELt (translate el) (translate er)
  translate (G.EGt el _ er) = EGt (translate el) (translate er)
  translate (G.ELEt el _ er) = ELEt (translate el) (translate er)
  translate (G.EGEt el _ er) = EGEt (translate el) (translate er)
  translate (G.EBWShiftLeft el _ er) = EBWShiftLeft (translate el) (translate er)
  translate (G.EBWShiftRight el _ er) = EBWShiftRight (translate el) (translate er)
  translate (G.EAdd el _ er) = EAdd (translate el) (translate er)
  translate (G.ESub el _ er) = ESub (translate el) (translate er)
  translate (G.EMul el _ er) = EMul (translate el) (translate er)
  translate (G.EDiv el _ er) = EDiv (translate el) (translate er)
  translate (G.EMod el _ er) = EMod (translate el) (translate er)
  translate (G.ENeg _ e) = ENeg (translate e)
  translate (G.ENegSign _ e) = ENegSign (translate e)
  translate (G.EComplement _ e) = EComplement (translate e)
  translate (G.EPos _ e) = EPos (translate e)
  translate (G.EPreInc _ e) = EPreInc (translate e)
  translate (G.EPreDec _ e) = EPreDec (translate e)
  translate (G.ECall cid es) = ECall (cIdentToString cid) (map translate es)
  translate (G.ETypeCall t es) = ETypeCall (translate t) (map translate es)
  translate (G.EFloat (G.CFloat s)) = EFloat (read s ∷ Float)
  translate (G.EMemberCall e cid es) = EMemberCall (translate e)
    (cIdentToString cid) (map translate es)
  translate (G.EMember e cid) = EMember (translate e) (cIdentToString cid)
  translate (G.EVar cid) = EVar (cIdentToString cid)
  translate (G.EVarType cid _) = EVar (cIdentToString cid)
  translate e = error $ "Not implemented " ++ show e
{-
 -EPostInc
 -EPostDec
 -EMember
 -EMemberCall
 -ECall
 -ETypeCall
 -EVar
 -EIndex
 -EFloat
 -EInt
 -ETrue
 -EFalse
 -EPartCall
 -ECurryCall
 -EVarType
 -}

