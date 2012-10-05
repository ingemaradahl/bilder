{-# LANGUAGE UnicodeSyntax #-}

module FrontEnd where

import System.IO
import System.FilePath
import System.Exit (exitFailure)

import Control.Exception (try)
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Error

import Text.Regex.Posix
import Data.Tree
import Data.List ((\\))

import FrontEnd.AbsGrammar
import FrontEnd.ParGrammar
import FrontEnd.LexGrammar

import FrontEnd.ErrM

import CompilerError

-- PM - ParserMonad
type PM a = StateT [FilePath] (CErrorT IO) a

liftCError :: CError a -> PM a
liftCError m = StateT (\s -> case m of { Fail f → CErrorT $ return $ Fail f; Pass a → return (a,s) })

fun ∷ FilePath → IO (CError (Tree (FilePath, AbsTree)))
fun f = runCErrorT $ evalStateT (parseTree f) []

addFile ∷ FilePath → PM [FilePath]
addFile f = (modify . (:)) f >> get

-- | Recursively parse the files to be imported
parseTree ∷ FilePath → PM (Tree (FilePath, AbsTree))
parseTree f = do
  liftIO . putStrLn $ "reading " ++ f
  imported ← addFile file
  t ← return f >>= liftIO . readFile >>= liftCError . parse
  mapM parseTree ((map ((++) $ dir ++ "/") (filterImports t)) \\ imported) >>= (return . Node (f,t))
 where
  dir = takeDirectory f
  file = concat [dir, "/", f]

--readSource ∷ FilePath → IO String
--readSource file = do
--  inp ← try (readFile file) ∷ IO (Either IOError String)
--  case inp of
--    -- TODO: actually check which error occurs
--    Left e → putStrLn (show e) >> exitFailure
--    Right v → return v

filterImports ∷ AbsTree → [FilePath]
filterImports (AbsTree ts) = [ f | Import _ f ← filter isImport ts ]
 where
  isImport ∷ Toplevel → Bool
  isImport (Import _ t) = True
  isImport _ = False

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

