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

module TypeChecker.Types.Blob where

import Data.Map (Map, toList)
import Data.List (intercalate)
import Text.Printf

import TypeChecker.Types

data Blob = Blob {
  filename ∷ FilePath,
  functions ∷ Map String [Function],
  typedefs ∷ Map String Typedef,
  variables ∷ Map String Variable
}

instance Show Blob where
  show (Blob file funs types vars) = printf (
    "# %s ###########\n" ++
    "Typedefs:\n%s" ++
    "Functions:\n%s" ++
    "Variables:\n%s")
    file
    ((concatMap (\(k,v) → printf "  %s: %s\n" k (show $ typedefType v)) $ toList types) :: String)
    ((concatMap (\(_,v) → printf "  %s\n" $ intercalate "\n  " $ map show v) $ toList funs) :: String)
    ((concatMap (\(k,v) → printf "  %s: %s\n" k (show v)) $ toList vars) :: String)

