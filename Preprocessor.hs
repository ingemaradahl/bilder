{-# LANGUAGE UnicodeSyntax #-}

module Preprocessor where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Error

import Text.Regex
import Data.Maybe (catMaybes)
import Data.List (sortBy)

import Data.Map (member, insert, delete, assocs)

import Text.Printf (printf)
import Text.Regex.Posix

import CompilerError

import Parser

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
  modify (\s → s { currentLine = n })
  if isDirective line
    then case extractDeclarative line of
      ("define", val)  → define val
      ("undef", name)  → undefine name
      ("ifdef", name)  → isDefined name >>= pushAction
      ("ifndef", name) → isDefined name >>= (pushAction . not)
      ("else", _)      → popAction >>= (pushAction . not)
      ("endif", _)     → void popAction
      (decl, _) → void $ warning (printf "unknown declarative: %s." decl)
      >> return (Just $ "//" ++ line ++ "\n")
    else do
      ifs ← gets ifStack
      if all (==True) ifs
        then liftM (Just . (++ "\n")) (subMacros line)
        else return $ Just ("//" ++ line ++ "\n")
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
  return $ foldr sub s $ sortBy (compare `on` (length . fst)) (assocs ms)
 where
    sub ∷ (String, String) → String → String
    sub (k, v) t = subRegex (mkRegex k) t v

-- | Checks if a variable is declared.
isDefined ∷ String → PM Bool
isDefined name = do
  defs ← gets defines
  return $ member name defs

-- | Adds a definiton writing over existing ones with the same name
define ∷ String → PM ()
define line = do
  line' ← subMacros line
  let (name, value) = (break (==' ') . strip) line'
  isDefined name >>= flip when (warning $ printf "%s already defined." name)
  defs ← gets defines
  modify (\s → s { defines = insert name (strip value) defs })

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
