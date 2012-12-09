{-# LANGUAGE UnicodeSyntax #-}

import System.Environment (getArgs)

import Text.Printf

import CompilerError
import CompilerTypes
import FrontEnd
import Compiler hiding (compile)
import TypeChecker

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

printResult ∷ CError (String, [(String, String)]) → IO ()
printResult r = case r of
  Pass (g, ss) → do
    putStrLn $ printf "Graph:\n%s\n" g
    sequence_ [ do
      putStrLn $ printf "file %s:\n" (fst s)
      mapM_ putStrLn (lines $ snd s)
      putStrLn "--------------------\n"
      | s ← ss ]
  Fail e → putStrLn $ "FAIL:\n" ++ show e

main ∷ IO ()
main = fmap parseArgs getArgs >>= maybe printHelp compile

