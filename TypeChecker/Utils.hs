{-# LANGUAGE UnicodeSyntax, FlexibleInstances #-}

module TypeChecker.Utils where

import FrontEnd.AbsGrammar
import CompilerError (Position)

paramType ∷ Param → Type
paramType (ConstParamDec _ t i) = idToType i t
paramType (ConstParamDefault _ t i _) = idToType i t
paramType (ParamDec t i) = idToType i t
paramType (ParamDefault t i _) = idToType i t

idToType ∷ Id → Type → Type
idToType (IdEmptyArray _) t = TArray t
idToType (IdArray _ _) t = TArray t
idToType _ t = t

idToCIdent ∷ Id → CIdent
idToCIdent (IdEmptyArray c) = c
idToCIdent (IdArray c _) = c
idToCIdent (Ident c) = c

cIdentToString ∷ CIdent → String
cIdentToString (CIdent (_,s)) = s

cIdentToPos ∷ CIdent → Position
cIdentToPos (CIdent (pos,_)) = pos

paramToId ∷ Param → Id
paramToId (ConstParamDec _ _ i) = i
paramToId (ConstParamDefault _ _ i _) = i
paramToId (ParamDec _ i) = i
paramToId (ParamDefault _ i _) = i

paramToString ∷ Param → String
paramToString = cIdentToString . idToCIdent . paramToId

paramToPos ∷ Param → Position
paramToPos = cIdentToPos . idToCIdent . paramToId
