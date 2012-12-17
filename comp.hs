{-# LANGUAGE UnicodeSyntax #-}

import System.Environment (getArgs)

import Text.Printf
import Text.JSON

import CompilerError
import CompilerTypes
import FrontEnd
import Compiler hiding (compile)
import TypeChecker

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

parseArgs ∷ [String] → Maybe Options
parseArgs [] = Nothing
parseArgs s  = Just $ Options $ head s

printHelp ∷ IO ()
printHelp = putStrLn "HELP"

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
main = fmap parseArgs getArgs >>= maybe printHelp compile

