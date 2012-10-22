{-# LANGUAGE UnicodeSyntax  #-}

module CompilerTypes where

type Position = (Int, Int)
type Location = (FilePath,Position)
type Warning = (Location, String)

