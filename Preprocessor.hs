{-# LANGUAGE UnicodeSyntax #-}

module Preprocessor where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Error

import Text.Regex
import Data.Maybe (catMaybes)

import Data.Map (foldrWithKey, member, insert, delete)

import Text.Printf (printf)

import CompilerError

import Parser

-- | Reads and processes file.
readAndProcessFile ∷ FilePath → PM String
readAndProcessFile f = do
  modify (\s → s {currentFile = f})
  liftIO (readFile f) >>= preprocess

-- | Preprocess the given source code.
preprocess ∷ String → PM String
preprocess src = do
  ls ← mapM processLine (zip [1..] $ lines src)
  ifs ← gets ifStack
  unless (ifs == []) $ syntaxError "unmatched if-statement."
  return $ (concat . catMaybes) ls

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
      >> return Nothing
    else do
      k ← keep
      if k
        then liftM (Just . (++ "\n")) (subMacros line)
        else return Nothing
  where
    isDirective ∷ String → Bool
    isDirective = (=="#") . take 1 . dropWhile (==' ')
    extractDeclarative ∷ String → (String, String)
    extractDeclarative = stripTuple . break (==' ') . drop 1 . strip
    stripTuple (a, b) = (strip a, strip b)
    keep ∷ PM Bool
    keep = do
      ifs ← gets ifStack
      return $ all (==True) ifs

-- | Substitutes all macros in given string
subMacros ∷ String → PM String
subMacros s = do
  -- TODO: Sort macros by length.
  ms ← gets defines
  return $ foldrWithKey sub s ms
  where
    sub ∷ String → String → String → String
    sub k v t = subRegex (mkRegex k) t v

-- | Checks if a variable is declared.
isDefined ∷ String → PM Bool
isDefined name = do
  defs ← gets defines
  return $ member name defs

-- | Adds a definiton writing over existing ones with the same name
define ∷ String → PM ()
define line = do
--  liftIO $ putStrLn $ printf "defining macro %s" name
  isDefined name >>= flip when (warning $ printf "%s already defined." name)
  defs ← gets defines
  modify (\s → s { defines = insert name (strip value) defs })
  where
    (name, value) = (break (==' ') . strip) line

-- | Deletes a definition
undefine ∷ String → PM ()
undefine name = do
--  liftIO $ putStrLn $ printf "undefining macro %s" name
  isDefined name >>= flip unless (warning $ printf "%s not defined." name)
  defs ← gets defines
  modify (\st → st { defines = delete (strip name) defs })

strip ∷ String → String
strip = dropWhile (==' ')

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
