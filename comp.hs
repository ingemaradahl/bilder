{-# LANGUAGE UnicodeSyntax #-}

import Control.Applicative
import Control.Exception (try)

import System.IO
import System.FilePath
import System.Environment (getArgs)
import System.Exit (exitFailure)

import Data.List (intersperse)

import CompilerError
import FrontEnd
import Compiler hiding (compile)

-- Compilation chain
compile ∷ String → Options → CErr [String]
compile src o = parse src >>= typeCheck o >>= compileTree o

parseArgs ∷ [String] → Maybe Options
parseArgs [] = Nothing
parseArgs s  = Just $ Options $ head s

printHelp ∷ IO ()
printHelp = putStrLn "HELP"

printResult ∷ CErr [String] → IO ()
printResult r = case r of
  Pass s -> putStrLn $ "PASS " ++ (concat . intersperse "\n") s
  Fail e -> putStrLn $ "FAIL: " ++ show e

readSource ∷ FilePath → IO String
readSource file = do
  inp ← try (readFile file) ∷ IO (Either IOError String)
  case inp of
    -- TODO: actually check which error occurs
    Left e → putStrLn (show e) >> printHelp >> exitFailure
    Right v → return v

main ∷ IO ()
main = do
  opts ← fmap parseArgs getArgs
  let src = fmap (readSource . inputFile) opts
  case pure (,) <*> src <*> opts of
    Just (src, op) → src >>= (printResult . flip compile op)
    Nothing → printHelp

