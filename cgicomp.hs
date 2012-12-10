{-# LANGUAGE UnicodeSyntax #-}

import Text.Printf

import CompilerError
import CompilerTypes
import FrontEnd
import Compiler hiding (compile)

import TypeChecker

import Network.CGI
import NetTypes

import Text.JSON

-- | Index page - a form where one can submit JSON containing shaders.
index ∷ CGI CGIResult
index = output "<html><body><form method=\"post\" action=\"/\">Shader:<br/><textarea rows=\"30\" cols=\"100\" name=\"files\" /></textarea></br><input type=\"submit\" /></form></body></html>"

-- | Parses given JSON dict, compiles containing files and return the
--    compiled shaders in a new JSON dict.
handleRequest ∷ CGI CGIResult
handleRequest = do
  files ← getInput "files"
  case files of
    Nothing → index
    Just fs →
      case decode fs ∷ Result [NetFile] of
        (Ok fs')  → do
          setHeader "Content-Type" "application/json"
          liftIO (compileFiles fs') >>= output
        (Error s) → output $ "error: " ++ show s

main ∷ IO ()
main = runCGI (handleErrors handleRequest)

-- | Compiles a list of Files to a JSON string or an error.
compileFiles ∷ [NetFile] → IO String
compileFiles fs = do
  p ← parseNetHead os fs
  let warnBlobs = p >>= typeCheck os
  case warnBlobs of
    Pass (warns, blobs) → formResult warns (compileTree os blobs)
    Fail e → return $ printf "error when compiling: %s" (show e)
 where
  os = Options "zeh cloud, awh yeah"

data Compiled = Compiled {
    warnings ∷ [Warning]
  , errors ∷ [String]
  , compiledData ∷ String
  }

instance JSON Compiled where
  showJSON (Compiled ws es c) = makeObj [
      ("warnings", showJSON ws)
    , ("errors", showJSON es)
    , ("data", showJSON c)
    ]
  readJSON _ = error "show only"

formResult ∷ [Warning] → CError String → IO String
formResult ws r = case r of
  Pass s → return $ encode $ Compiled ws [] s
  Fail e → return $ encode $ Compiled ws [show e] ""
