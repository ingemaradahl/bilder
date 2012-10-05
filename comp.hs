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
import FrontEnd.AbsGrammar
import Compiler hiding (compile)
import TypeChecker

-- Compilation chain
compile ∷ Options → IO ()
compile o = parseHead (inputFile o) >>= printResult . ((=<<) $ compileTree o) . ((=<<) $ typeCheck o)

parseArgs ∷ [String] → Maybe Options
parseArgs [] = Nothing
parseArgs s  = Just $ Options $ head s

printHelp ∷ IO ()
printHelp = putStrLn "HELP"

printResult ∷ CError [String] → IO ()
printResult r = case r of
  Pass s -> putStrLn $ (concat . intersperse "\n") s
  Fail e -> putStrLn $ "FAIL: " ++ show e

main ∷ IO ()
main = fmap parseArgs getArgs >>= maybe printHelp compile

