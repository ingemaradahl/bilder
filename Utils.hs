{-# Language UnicodeSyntax #-}

module Utils where

import Control.Monad

import Data.Monoid
import Data.Tree

class Mongoid a where
  (¿) ∷ a → a → a

instance Mongoid (Maybe a) where
  Nothing ¿ perhaps = perhaps
  Just v  ¿ _       = Just v

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

