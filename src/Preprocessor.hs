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

module Preprocessor where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Error

import Text.Regex
import Data.Maybe (catMaybes)
import Data.List (sortBy)

import Data.Map (member, insert, delete, toList)

import Text.Printf (printf)
import Text.Regex.Posix
import Text.ParserCombinators.Parsec

import CompilerError

import Parser hiding (parse)

import Data.Function (on)

-- | Reads and processes file.
readAndProcessFile ∷ FilePath → PM String
readAndProcessFile f = do
  modify (\s → s {currentFile = f})
  src ← liftIO (readFile f)
  mapM_ readChild (lines src)
  preprocess src

readChild ∷ String → PM ()
readChild line =
  case extractFileName line of
    Nothing   → return ()
    Just file → void $ readAndProcessFile file

-- | Preprocess the given source code.
preprocess ∷ String → PM String
preprocess src = do
  ls ← mapM processLine (zip [1..] $ lines src)
  ifs ← gets ifStack
  unless (ifs == []) $ syntaxError "unmatched if-statement."
  return $ (concat . catMaybes) ls

-- | Extracts the filename from an import statement.
extractFileName ∷ String → Maybe String
extractFileName line = if null ms then Nothing else Just $ head ms
 where
  (_,_,_,ms) = line =~ "import[ ]+\"([^\"]+)\"" ∷ (String,String,String,[String])

-- | Preprocesses given a line of source code
processLine ∷ (Int, String) → PM (Maybe String)
processLine (n, line) = do
  let line' = map (\c → if c == '\t' then ' ' else c) line
  modify (\s → s { currentLine = n })
  if isDirective line'
    then case extractDeclarative line' of
      ("define", val)  → define val
      ("undef", name)  → undefine name
      ("ifdef", name)  → isDefined name >>= pushAction
      ("ifndef", name) → isDefined name >>= (pushAction . not)
      ("else", _)      → popAction >>= (pushAction . not)
      ("endif", _)     → void popAction
      (decl, _) → void $ warning (printf "unknown declarative: %s." decl)
      >> return (Just $ "//" ++ line' ++ "\n")
    else do
      ifs ← gets ifStack
      if all (==True) ifs
        then liftM (Just . (++ "\n")) (subMacros line')
        else return $ Just ("//" ++ line' ++ "\n")
 where
    isDirective ∷ String → Bool
    isDirective = (=="#") . take 1 . strip
    extractDeclarative ∷ String → (String, String)
    extractDeclarative = stripTuple . break (==' ') . strip . drop 1 . strip
    stripTuple (a, b) = (strip a, strip b)

-- | Substitutes all macros in given string
subMacros ∷ String → PM String
subMacros s = do
  ms ← gets defines
  foldM sub s $ reverse $ sortBy (compare `on` (length . fst)) (toList ms)

-- Replaces all macros in a line.
sub ∷ String → (String, ([String], String)) → PM String
sub t (k, (as, v)) =
  if null as
    then return $ subRegex (mkRegex k) t v
    else repeatRun t
 where
  repeatRun ∷ String → PM String
  repeatRun s = do
    res ← runFunMacro s
    res2 ← runFunMacro res
    if res /= res2 then repeatRun res2 else return res
  runFunMacro ∷ String → PM String
  runFunMacro line = do
    res ← findFunMacro k line
    case res of
      Nothing → return line
      Just (bef, _, margs, rest) → do
         when (length margs /= length as) (syntaxError "uneven amount of arguments")
         return $ bef ++ foldl replaceArgs v (zip as margs) ++ rest
  replaceArgs ∷ String → (String, String) → String
  replaceArgs p (m,a) = subRegex (mkRegex m) p a

-- finds macro from the right to the left
--  Returns (before, macro, args, after)
findFunMacro ∷ String → String → PM (Maybe (String, String, [String], String))
findFunMacro name line =
  case findLastName name line of
    Nothing → return Nothing
    Just ln → do
      let (bef, stuff) = splitAt ln line
      (mname, args, rest) ← parseFunMacro stuff
      return $ Just (bef, mname, args, rest)

findLastName ∷ String → String → Maybe Int
findLastName name line =
  if pos == -1
    then Nothing
    else Just (length line - length name - pos)
 where
  re = reverse name ++ "\\W"
  (pos,_) = reverse line =~ re ∷ (Int,Int)

-- | Parsing of used macros (parsing must start at the beginning of a macro
--  and will return (macro, rest).
parseFunMacro ∷ String → PM (String, [String], String)
parseFunMacro line = case parse pm "" line of
  Left err → syntaxError $ "unable to preprocess line: " ++ show err
  Right (name, args, rest) → return (name, args, rest)

pm ∷ Parser (String, [String], String)
pm = do
  name ← many1 letter
  char '('
  args ← sepBy parseArg (do many space; char ','; many space)
  char ')'
  rest ← many anyChar
  return (name, if args == [""] then [] else args, rest)

parseArg ∷ Parser String
parseArg =
  do
    char '('
    arg ← parseArg
    char ')'
    return arg
  <|> many (noneOf "(),")

-- | Checks if a variable is declared.
isDefined ∷ String → PM Bool
isDefined name = do
  defs ← gets defines
  return $ member name defs

-- | Adds a definiton writing over existing ones with the same name
define ∷ String → PM ()
define line = do
  (name, args, value) ← parseDefine line
  value' ← subMacros value
  isDefined name >>= flip when (warning $ printf "%s already defined." name)
  defs ← gets defines
  modify (\s → s { defines = insert name (map strip args, value') defs })

-- | Parsing of defines (with and without arguments).
parseDefine ∷ String → PM (String, [String], String)
parseDefine line =
  case parse macro "" line of
    Left err  → syntaxError $ printf "unable to parse define directive: %s\n%s" line (show err)
    Right res → return res

macro ∷ Parser (String, [String], String)
macro = do
  name ← many1 letter
  args ← do
      char '('
      as ← macroArgs
      char ')'
      return as
    <|> return []
  many space
  tks ← many anyChar
  return (name, args, tks)

macroArgs ∷ Parser [String]
macroArgs = do
  many space
  arg ← many1 letter
  args ← do
      char ','
      macroArgs
    <|> return []
  return $ arg:args

-- | Deletes a definition
undefine ∷ String → PM ()
undefine name = do
  isDefined name >>= flip unless (warning $ printf "%s not defined." name)
  defs ← gets defines
  modify (\st → st { defines = delete (strip name) defs })

strip ∷ String → String
strip = dropWhile (`elem` " \t")

-- | pushAction / popAction keeps actions to take (keep source line or not)
pushAction ∷ Bool → PM ()
pushAction action = do
  ifs ← gets ifStack
  modify (\st → st { ifStack = action : ifs })

popAction ∷ PM Bool
popAction = do
  ifs ← gets ifStack
  modify (\st → st { ifStack = drop 1 ifs })
  when (ifs == []) $ syntaxError "unmatched if-statement."
  return $ head ifs

-- | Errors and warnings
syntaxError ∷ String → PM a
syntaxError msg = do
  line ← gets currentLine
  f ← gets currentFile
  throwError $ SyntaxError (line, 0) f msg

warning ∷ String → PM ()
warning w = do
  ws ← gets warnings
  l ← gets currentLine
  modify (\st → st { warnings = ws ++ [((l, 0), w)] })
