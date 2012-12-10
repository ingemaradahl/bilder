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

