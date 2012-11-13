{-# LANGUAGE UnicodeSyntax #-}

module Simple.Types where

import qualified Simple.AbsSimple as S


data SimpleSource = SimpleSource {
    functions ∷ [S.Function]
  , variables ∷ [S.Variable]
  , structs ∷ [S.Struct]
}
 deriving (Show)
