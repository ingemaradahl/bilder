{-# LANGUAGE UnicodeSyntax #-}

import System.Environment (getArgs)

import Data.List (intercalate)
import Text.Printf

import CompilerError
import CompilerTypes
import FrontEnd
import Compiler hiding (blobs)
import TypeChecker

-- Compilation chain
compile ∷ Options → IO ()
compile o = do
  p ← parseHead o
  let warnBlobs = p >>= typeCheck o
  case warnBlobs of
    Pass (warnings, blobs) → printWarnings warnings >> printResult (compileTree o blobs)
    Fail e → print e

printWarnings ∷ [Warning] → IO ()
printWarnings ws = putStrLn $ intercalate "\n" $ map (\((file, (l,c)), msg) →
  printf "Warning: %s:%s, column %s\n  %s" file (show l) (show c) msg) ws

parseArgs ∷ [String] → Maybe Options
parseArgs [] = Nothing
parseArgs s  = Just $ Options $ head s

printHelp ∷ IO ()
printHelp = putStrLn "HELP"

printResult ∷ CError [String] → IO ()
printResult r = case r of
  Pass s → putStrLn $ intercalate "\n" s
  Fail e → putStrLn $ "FAIL:\n" ++ show e

main ∷ IO ()
main = fmap parseArgs getArgs >>= maybe printHelp compile

