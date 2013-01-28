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

import CompilerError
import CompilerTypes
import FrontEnd
import Compiler hiding (compile)

import Control.DeepSeq
import Control.Exception

import TypeChecker

import Network.CGI
import Network.CGI.Protocol
import Network.CGI.Monad
import Control.Monad
import Control.Monad.Writer
import Control.Monad.Reader
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BS
import NetTypes

import Data.Char
import qualified Data.Map as Map

import Text.JSON

data Compiled = Compiled {
    warnings ∷ [Warning]
  , cerror ∷ Maybe CompilerError
  , compiledData ∷ JSValue
  }

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
  showJSON (SyntaxError (l,c) f s) = absJSON l c f "syntax" s
  showJSON (TypeError (l,c) f s) = absJSON l c f "type" s
  showJSON (CompileError (l,c) f s) = absJSON l c f "compiler" s
  showJSON (UnknownError s) = makeObj [
      ("message", showJSON s)
    , ("type", showJSON "unknown")
    ]
  readJSON _ = error "CompilerError: show only"

absJSON ∷ Int → Int → FilePath → String → String → JSValue
absJSON l c f t s = makeObj [
    ("line", showJSON l)
  , ("column", showJSON c)
  , ("file", showJSON f)
  , ("type", showJSON t)
  , ("message", showJSON s)
  ]

instance JSON Compiled where
  showJSON (Compiled ws e c) = makeObj [
      ("warnings", showJSON ws)
    , ("error", maybe (makeObj []) showJSON e)
    , ("data", showJSON c)
    ]
  readJSON _ = error "Compiled: show only"

formResult ∷ [Warning] → CError JSValue → IO String
formResult ws r = case r of
  Pass s → s `seq` return $ encode $ Compiled ws Nothing s
  Fail e → return $ encode $ Compiled ws (Just e) (makeObj [])

handleException ∷ Control.Exception.SomeException → IO BS.ByteString
handleException e = return $ BS.pack $
  "Content-Type: application/json\r\n\r\n" ++
  encode (Compiled [] (Just exc) (makeObj []))
 where
  exc = UnknownError $ "Exception in compiler: " ++ show e

main ∷ IO ()
main = do
  env ← getCGIVars
  hSetBinaryMode stdin True
  inp ← BS.hGetContents stdin
  outp ← Control.Exception.catch (runCGIEnvFPS env inp (runCGIT' handleRequest)) handleException
  BS.hPut stdout outp
  hFlush stdout

runCGIT' :: CGI CGIResult -> CGIRequest -> IO (Headers, CGIResult)
runCGIT' (CGIT c) r = do
  aoeu ← (liftM (uncurry (flip (,))) . runWriterT . runReaderT c) r
  case snd aoeu of
    CGIOutput bs → BS.unpack bs `deepseq` return aoeu
    _ → return aoeu
