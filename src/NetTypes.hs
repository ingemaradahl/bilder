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
{-# LANGUAGE UnicodeSyntax #-}

module NetTypes where

import Text.JSON

import Control.Applicative

data NetFile = NetFile {
    name ∷ String
  , contents ∷ String
  }
 deriving (Show)

instance JSON NetFile where
  showJSON (NetFile n c) = makeObj [
      ("name", showJSON n)
    , ("data", showJSON c)
    ]
  readJSON (JSObject obj) =
    NetFile <$> lu "name" <*> lu "data"
   where
    lu i = maybe (fail $ "no such element: " ++ i)
      readJSON (lookup i (fromJSObject obj))
  readJSON s = fail $ "what is this? i don't even: " ++ show s

