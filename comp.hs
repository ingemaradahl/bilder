{-# LANGUAGE UnicodeSyntax #-}

import System.Environment (getArgs)

import Data.List (intercalate)
import Text.Printf

import CompilerError
import CompilerTypes
import FrontEnd
import Compiler hiding (compile)
import TypeChecker

import FrontEnd.AbsGLSL
import FrontEnd.PrintGLSL

-- Compilation chain
compile ∷ Options → IO ()
compile o = do
  p ← parseHead o
  let warnBlobs = p >>= typeCheck o
  case warnBlobs of
    Pass (warns, blobs) → printWarnings warns >> printResult (compileTree o blobs)
    Fail e → print e

printWarnings ∷ [Warning] → IO ()
printWarnings ws = putStrLn $ intercalate "\n" $ map (\((file, (l,c)), msg) →
  printf "Warning: %s:%s, column %s\n  %s" file (show l) (show c) msg) ws

parseArgs ∷ [String] → Maybe Options
parseArgs [] = Nothing
parseArgs s  = Just $ Options $ head s

printHelp ∷ IO ()
printHelp = putStrLn "HELP"

printResult ∷ CError [Tree] → IO ()
printResult r = case r of
  Pass ss → sequence_ [ do
      putStrLn $ printTree s
      putStrLn "--------------------\n"
    | s ← ss ]
  Fail e → putStrLn $ "FAIL:\n" ++ show e

main ∷ IO ()
main = fmap parseArgs getArgs >>= maybe printHelp compile

