{-# LANGUAGE UnicodeSyntax #-}

module Simple.Types where

import qualified Simple.AbsSimple as S


data SimpleBlob = SimpleBlob {
    functions ∷ [S.Function]
  , variables ∷ [S.Variable]
  , structs ∷ [S.Struct]
}
 deriving (Show)
