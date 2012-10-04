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

example = AbsTree [Import (TImport ((1,1),"import")) "bepa.cepa",Import (TImport ((2,1),"import")) "more-filters.fl",Import (TImport ((3,1),"import")) "even-more.fl",Function TColor (CIdent ((5,7),"main")) [] [SDecl (Dec TInt (DefaultVars [Ident (CIdent ((7,7),"b"))] (EInt 0))),SFor (TFor ((8,3),"for")) [FDecl (Dec TInt (DefaultVars [Ident (CIdent ((8,12),"a"))] (EInt 0)))] [ELt (EVar (Ident (CIdent ((8,17),"i")))) (EInt 10)] [EPostInc (EVar (Ident (CIdent ((8,23),"i"))))] (SBlock [SExp (EPostInc (EVar (Ident (CIdent ((11,5),"b")))))]),SReturn (TReturn ((13,3),"return")) (EVar (Ident (CIdent ((13,10),"b"))))]]


-- PM - ParserMonad
type PM a = StateT [FilePath] (CErrorT IO) a

liftCError :: CError a -> PM a
liftCError m = StateT (\s -> case m of { Fail f → CErrorT $ return $ Fail f; Pass a → return (a,s) })

fun ∷ FilePath → IO (CError (Tree (FilePath, AbsTree)))
fun f = runCErrorT $ evalStateT (parseTree "./" f) []

addFile ∷ FilePath → PM [FilePath]
addFile f = (modify . (:)) f >> get

parseTree ∷ FilePath → FilePath → PM (Tree (FilePath, AbsTree))
parseTree d f = do
  liftIO . putStrLn $ "reading " ++ f
  imported ← addFile f
  t ← return f >>= liftIO . readFile >>= liftCError . parse
  mapM (parseTree d) ((filterImports t) \\ imported) >>= (return . Node (f,t))

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

