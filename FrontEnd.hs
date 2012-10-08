{-# LANGUAGE UnicodeSyntax #-}

module FrontEnd where

import System.FilePath

import Control.Monad.Trans
import Control.Monad.Trans.State

import Text.Regex.Posix
import Data.Tree
import Data.List ((\\))

import FrontEnd.AbsGrammar
import FrontEnd.ParGrammar

import FrontEnd.ErrM

import CompilerError

-- | PM - A ParserMonad, keeps a state of which file has been imported
type PM a = StateT [FilePath] (CErrorT IO) a

liftCError ∷ CError a → PM a
liftCError m = StateT (\s → case m of { Fail f → CErrorT $ return $ Fail f; Pass a → return (a,s) })

-- | Add a file to the state, and return the entire state
addFile ∷ FilePath → PM [FilePath]
addFile f = (modify . (:)) f >> get

-- | Parse the tree of files
parseHead ∷ FilePath → IO (CError (Tree (FilePath, AbsTree)))
parseHead f = runCErrorT $ evalStateT (parseTree f) []

-- | Recursively parse the files to be imported
parseTree ∷ FilePath → PM (Tree (FilePath, AbsTree))
parseTree f = do
  liftIO . putStrLn $ "reading " ++ f
  imported ← addFile f
  t ← liftIO (readFile f) >>= liftCError . parse
  mapM parseTree (map (dir </>) (filterImports t) \\ imported) >>= (return . Node (f,t))
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
    parseErrM s | s =~ "line" ∷ Bool = SyntaxError (lineErr s,0) s
                | otherwise = UnknownError s
    lineErr ∷ String → Int
    lineErr s = read (s =~ "[1-9]+" ∷ String) ∷ Int

