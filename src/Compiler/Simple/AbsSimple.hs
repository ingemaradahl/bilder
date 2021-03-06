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

module Compiler.Simple.AbsSimple where

type Ident = String

data Variable = Variable {
      variableName ∷ String
    , variableType ∷ Type
    , const ∷ Bool
    , value ∷ Maybe Exp
  }
  deriving (Eq,Ord,Show)

data Function = Function {
      functionName ∷ String
    , returnType ∷ Type
    , pixelwise ∷ Bool
    , parameters ∷ [Variable]
    , statements ∷ [Stm]
  }
  deriving (Eq,Ord,Show)

data Struct = Struct {
      structName ∷ String
    , declarations ∷ [Variable]
  }
  deriving (Eq,Ord,Show)

data Stm =
   SDecl Variable
 | SDeclAss Variable Exp
 | SStruct Struct
 | SExp Exp
 | SWhile Exp [Stm]
 | SDoWhile [Stm] Exp
 | SFor [Stm] [Exp] [Exp] [Stm] -- First Stm must be SDeclAss or SExp.
 | SReturn Exp
 | SVoidReturn
 | SIf Exp [Stm]
 | SIfElse Exp [Stm] [Stm]
 | SBreak
 | SContinue
 | SDiscard
  deriving (Eq,Ord,Show)

data Exp =
   EAss Exp Exp
 | EAssAdd Exp Exp
 | EAssSub Exp Exp
 | EAssMul Exp Exp
 | EAssDiv Exp Exp
 | EAssMod Exp Exp
 | EAssBWAnd Exp Exp
 | EAssBWXOR Exp Exp
 | EAssBWOR Exp Exp
 | ECond Exp Exp Exp
 | EOR Exp Exp
 | EXOR Exp Exp
 | EAnd Exp Exp
 | EBWOR Exp Exp
 | EBWXOR Exp Exp
 | EBWAnd Exp Exp
 | EEqual Exp Exp
 | ENEqual Exp Exp
 | ELt Exp Exp
 | EGt Exp Exp
 | ELEt Exp Exp
 | EGEt Exp Exp
 | EBWShiftLeft Exp Exp
 | EBWShiftRight Exp Exp
 | EAdd Exp Exp
 | ESub Exp Exp
 | EMul Exp Exp
 | EDiv Exp Exp
 | EMod Exp Exp
 | ENeg Exp
 | ENegSign Exp
 | EComplement Exp
 | EPos Exp
 | EPreInc Exp
 | EPreDec Exp
 | EPostInc Exp
 | EPostDec Exp
 | EMember Exp Ident
 | EMemberCall Exp Ident [Exp]
 | ECall Ident [Exp]
 | ETypeCall Type [Exp]
 | EVar Ident
 | EIndex Exp Exp
 | EFloat Float
 | EInt Integer
 | ETrue
 | EFalse
  deriving (Eq,Ord,Show)

data Type =
   TVoid
 | TFloat
 | TBool
 | TInt
 | TVec2
 | TVec3
 | TVec4
 | TMat2
 | TMat3
 | TMat4
 | TSampler
 | TStruct Ident
  deriving (Eq,Ord,Show)
