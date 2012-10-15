{-# LANGUAGE UnicodeSyntax #-}

module FrontEnd where

import System.FilePath

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Error

import Text.Regex.Posix
import Text.Regex
import Data.Tree
import Data.List ((\\))
import Data.Maybe (catMaybes)

import qualified Data.Map as Map

import FrontEnd.AbsGrammar
import FrontEnd.ParGrammar

import FrontEnd.ErrM
import Text.Printf (printf)

import Compiler hiding (options, buildEnv)
import CompilerError
import CompilerTypes
-- TODO: Use Location instead of Position.

-- | PM - A ParserMonad, keeps a state of which file has been imported
type PM a = StateT PPEnv (CErrorT IO) a

liftCError ∷ CError a → PM a
liftCError m = StateT (\s → case m of { Fail f → CErrorT $ return $ Fail f; Pass a → return (a,s) })

-- | Add a file to the state, and return the entire state
addFile ∷ FilePath → PM [FilePath]
--addFile f = (modify . (:)) f >> get
addFile f = do
  fs ← gets filepaths
  modify (\s → s { filepaths = f : fs })
  return $ f : fs

-- | Parse the tree of files
parseHead ∷ Options → IO (CError (Tree (FilePath, AbsTree)))
parseHead os = runCErrorT $ evalStateT (parseTree (inputFile os)) (buildEnv os)

-- | Recursively parse the files to be imported
parseTree ∷ FilePath → PM (Tree (FilePath, AbsTree))
parseTree f = do
  liftIO . putStrLn $ "reading " ++ f
  imported ← addFile f
  src ← readAndProcessFile f
  liftIO $ mapM putStrLn (lines src)
  --t ← readAndProcessFile f >>= liftCError . parse
  t ← (liftCError . parse) src
  liftM (Node (f, t)) (mapM parseTree (map (dir </>) (filterImports t) \\ imported))
 where
  dir = takeDirectory f

-- | Filter out the import statements from the syntax tree
filterImports ∷ AbsTree → [FilePath]
filterImports (AbsTree ts) = [ f | Import _ f ← ts ]

-- | Parse the given source code
parse ∷ String → CError AbsTree
parse src =
  case (pAbsTree . myLexer) src of
    Bad e → Fail $ parseErrM e
    Ok tree → return tree
  where
    parseErrM ∷ String → CompilerError
    parseErrM s | s =~ "line" ∷ Bool = SyntaxError (lineErr s,0) "file" s
                | otherwise = UnknownError s
    lineErr ∷ String → Int
    lineErr s = read (s =~ "[1-9]+" ∷ String) ∷ Int


-- | Preprocessor stuff
data PPEnv = PPEnv {
	defines ∷ Map.Map String String,
	ifStack ∷ [Bool],
  warnings ∷ [(Position, String)],
  filepaths ∷ [FilePath],
  currentFile ∷ FilePath,
  currentLine ∷ Int,
  options ∷ Options
}
 deriving (Show)

-- TODO: Take Options.
buildEnv ∷ Options → PPEnv
buildEnv os = PPEnv {
  ifStack = [],
  defines = Map.empty,
  warnings = [],
  filepaths = [],
  currentFile = "",
  currentLine = 0,
  options = os
}

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
      ("endif", _)     → popAction >> return ()
      (decl, _) → warning (printf "unknown declarative: %s." decl) >> return ()
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
  ms ← gets defines
  return $ Map.foldrWithKey sub s ms
  where
    sub ∷ String → String → String → String
    sub k v t = subRegex (mkRegex k) t v

-- | Checks if a variable is declared.
isDefined ∷ String → PM Bool
isDefined name = do
  defs ← gets defines
  return $ Map.member name defs

-- | Adds a definiton writing over existing ones with the same name
define ∷ String → PM ()
define line = do
--  liftIO $ putStrLn $ printf "defining macro %s" name
  isDefined name >>= flip when (warning $ printf "%s already defined." name)
  defs ← gets defines
  modify (\s → s { defines = Map.insert name (strip value) defs })
  where
    (name, value) = (break (==' ') . strip) line

-- | Deletes a definition
undefine ∷ String → PM ()
undefine name = do
--  liftIO $ putStrLn $ printf "undefining macro %s" name
  isDefined name >>= flip unless (warning $ printf "%s not defined." name)
  defs ← gets defines
  modify (\st → st { defines = Map.delete (strip name) defs })

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
