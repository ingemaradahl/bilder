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
{-# Language UnicodeSyntax #-}

module Utils where

import Control.Monad

import Data.Monoid
import Data.Tree

mayhaps ∷ Bool → a → Maybe a
mayhaps True  v = Just v
mayhaps False _ = Nothing

duplicates ∷ Eq a => [a] → [a]
duplicates [] = []
duplicates (x:xs)
  | x `elem` xs = x:duplicates (filter (/= x) xs)
  | otherwise   = duplicates xs

duplicatesWith ∷ (a → a → Bool) → [a] → [a]
duplicatesWith _ [] = []
duplicatesWith f (x:xs)
 | elemBy f x xs = x:duplicatesWith f (filter (not . f x) xs)
 | otherwise     = duplicatesWith f xs

elemBy ∷ (a → a → Bool) → a → [a] → Bool
elemBy _ _ [] = False
elemBy f y (x:xs)
 | f y x = True
 | otherwise = elemBy f y xs

traverse ∷ Monad m => (a → [Tree b] → m b) → Tree a → m (Tree b)
traverse f (Node r bs) = do
  sub ← mapM (traverse f) bs
  root ← f r sub
  return $ Node root sub

depthFold ∷ (Monad m, Monoid b) => (a → b → m b) → Tree a → m b
depthFold f (Node r ts) = liftM mconcat (mapM (depthFold f) ts) >>= f r

none ∷ (a → Bool) → [a] → Bool
none f = not . any f

leave ∷ Int → [a] → [a]
leave n xs = drop (length xs - n) xs
