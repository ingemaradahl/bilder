{-# LANGUAGE UnicodeSyntax #-}

import CompilerError
import CompilerTypes
import FrontEnd
import Compiler hiding (compile)

import Control.Exception

import TypeChecker

import Network.CGI
import NetTypes

import Data.Char
import qualified Data.Map as Map

import Text.JSON

data Compiled = Compiled {
    warnings ∷ [Warning]
  , cerror ∷ Maybe CompilerError
  , compiledData ∷ JSValue
  }

-- | Index page - a form where one can submit JSON containing shaders.
index ∷ CGI CGIResult
index = output "<html><body><form method=\"post\" action=\"/\">Shader:<br/><textarea rows=\"30\" cols=\"100\" name=\"files\" /></textarea></br><input type=\"submit\" /></form></body></html>"

-- | Parses given JSON dict, compiles containing files and return the
--    compiled shaders in a new JSON dict.
handleRequest ∷ CGI CGIResult
handleRequest = do
  ins ← getInputs
  setHeader "Content-Type" "application/json"
  liftIO (compileFiles $ parseFiles ins) >>= output

-- | Parses <name>[<num>][<field>] to (<num>, <field>)
parseName ∷ String → Maybe (Int, String)
parseName s =
  if all (==True) (map isNumber num) && length num > 0
    then Just (read num, field)
    else Nothing
 where
  num = takeWhile (/=']') $ drop 1 $ dropWhile (/='[') s
  field = reverse $ takeWhile (/='[') $ drop 1 $ reverse s

updateTuple ∷ String → String → (String, String) → (String, String)
updateTuple "name" value = \(_, d) → (value, d)
updateTuple "data" value = \(n, _) → (n, value)

-- (Name, Data)
pair ∷ [(String, String)] → Map.Map Int (String, String)
pair ((n,d):[]) =
  case parseName n of
    Nothing → error "UNABLE TO PARSE OH MY GOOOD"
    Just (i, f) → Map.fromList [(i, updateTuple f d ("", ""))]
pair ((n,d):ss) =
  case parseName n of
    Nothing → error "OH MY GOOOOD UNABLE TO PARSE"
    Just (i, f) →
      case Map.lookup i rest of
        Nothing  → Map.insert i (updateTuple f d ("", "")) rest
        Just cur → Map.insert i (updateTuple f d cur) rest
 where
  rest = pair ss

parseFiles ∷ [(String, String)] → [NetFile]
parseFiles ss = map (uncurry NetFile) $ Map.elems $ pair ss

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

handleException ∷ Exception a => a → CGI CGIResult
handleException e = do
  setHeader "Content-Type" "application/json"
  output $ encode $ Compiled [] (Just exc) (makeObj [])
 where
  exc = UnknownError $ "Exception in compiler: " ++ show e

handleIOException ∷ Control.Exception.SomeException → IO ()
handleIOException e = putStrLn ("Exception in compiler: " ++ show e)

main ∷ IO ()
main = Control.Exception.catch 
  (runCGI (catchCGI handleRequest handleException))
  handleIOException 
