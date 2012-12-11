{-# LANGUAGE UnicodeSyntax #-}

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
        (Error s) → output $ encode $ Compiled [] (Just $ UnknownError $ "Unable to decode JSON: " ++ show s) (makeObj [])

main ∷ IO ()
main = runCGI (handleErrors handleRequest)

-- | Compiles a list of Files to a JSON string or an error.
compileFiles ∷ [NetFile] → IO String
compileFiles fs = do
  p ← parseNetHead os fs
  let warnBlobs = p >>= typeCheck os
  case warnBlobs of
    Pass (warns, blobs) → formResult warns (compileTree os blobs)
    Fail e → return $ encode $ Compiled [] (Just e) (makeObj [])
 where
  os = Options "zeh cloud, awh yeah"

data Compiled = Compiled {
    warnings ∷ [Warning]
  , cerror ∷ Maybe CompilerError
  , compiledData ∷ JSValue
  }

instance JSON CompilerError where
  showJSON (SyntaxError (l,c) f s) = makeObj [
      ("line", showJSON l)
    , ("column", showJSON c)
    , ("file", showJSON f)
    , ("type", showJSON "syntax")
    , ("message", showJSON s)
    ]
  showJSON (TypeError (l,c) f s) = makeObj [
      ("line", showJSON l)
    , ("column", showJSON c)
    , ("file", showJSON f)
    , ("type", showJSON "type")
    , ("message", showJSON s)
    ]
  showJSON (CompileError (l,c) f s) = makeObj [
      ("line", showJSON l)
    , ("column", showJSON c)
    , ("file", showJSON f)
    , ("type", showJSON "compiler")
    , ("message", showJSON s)
    ]
  showJSON (UnknownError s) = makeObj [
      ("message", showJSON s)
    , ("type", showJSON "unknown")
    ]
  readJSON _ = error "CompilerError: show only"

instance JSON Compiled where
  showJSON (Compiled ws e c) = makeObj [
      ("warnings", showJSON ws)
    , ("error", maybe (makeObj []) showJSON e)
    , ("data", showJSON c)
    ]
  readJSON _ = error "Compiled: show only"

formResult ∷ [Warning] → CError JSValue → IO String
formResult ws r = case r of
  Pass s → return $ encode $ Compiled ws Nothing s
  Fail e → return $ encode $ Compiled ws (Just e) (makeObj [])
