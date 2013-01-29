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

import System.Environment (getArgs)

import Text.Printf
import Text.JSON

import CompilerError
import CompilerTypes
import FrontEnd
import Compiler hiding (compile)
import TypeChecker

import Control.Monad.State

import qualified Data.Map as Map
import Data.Maybe

-- Compilation chain
compile ∷ Options → IO ()
compile o = do
  p ← parseHead o
  let warnBlobs = p >>= typeCheck o
  case warnBlobs of
    Pass (warns, blobs) → printWarnings warns >> printResult (compileTree o blobs)
    Fail e → print e

printWarnings ∷ [Warning] → IO ()
printWarnings ws = putStrLn $ unlines $ map (\((file, (l,c)), msg) →
  printf "Warning: %s:%s, column %s\n  %s" file (show l) (show c) msg) ws

parseArgs ∷ [String] → State Options Bool
parseArgs [] = return False
parseArgs ("-P":fp:rest) = do
  modify (\s → s { preludeFile = fp })
  parseArgs rest
parseArgs (fp:_) = do
  modify (\s → s { inputFile = fp })
  return True

printHelp ∷ IO ()
printHelp = putStrLn "usage: bildc <filter>"

printResult ∷ CError JSValue → IO ()
printResult r = case r of
  Pass (JSObject o) → do
    let obj = fromJSObject o
    printGraph $ (snd . head) obj
    printShaders $ (snd . last) obj
  Fail e → putStrLn $ "FAIL:\n" ++ show e

printGraph ∷ JSValue → IO ()
printGraph (JSObject o) = putStrLn $ printf "graph:\n\n%s\n\n" (encode o)

printShaders ∷ JSValue → IO ()
printShaders (JSObject o) = do
  putStrLn $ printf "shader %s:\n\n" name
  putStrLn shader
 where
  m = Map.fromList $ fromJSObject o
  name = lookdown "name"
  shader = lookdown "shader"
  lookdown ∷ String → String
  lookdown n = (\(JSString s) → fromJSString s) (fromJust $ Map.lookup n m)
printShaders (JSArray os) = mapM_ printShaders os

printShaders o = error $ "what: " ++ show o

main ∷ IO ()
main = do
  args ← getArgs
  case runState (parseArgs args) (Options "" "include/Prelude.bild") of
    (False, _) → printHelp
    (_, os) → compile os

