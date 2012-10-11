{-# LANGUAGE UnicodeSyntax #-}
-- TODO: All defines should be upper case?

module Preprocessor where

import Control.Monad.State

import qualified Data.Map as Map

import CompilerError

import Control.Monad.Error

{-
#define
#undef

#ifdef
#ifndef
#else
#endif

#pragmas
-}

data If = Keep | Throw
 deriving (Show)

data PPEnv = PPEnv {
	defines ∷ Map.Map String String,
	ifStack ∷ [If],
  warnings ∷ [(Position, String)]
}
 deriving (Show)


emptyEnv ∷ PPEnv
emptyEnv = PPEnv {
  ifStack = [],
  defines = Map.empty,
  warnings = []
}

type PPM a = StateT PPEnv CError a

-- | Adds a directive to the environment
--addDirective ∷ String → PPM ()
--addDirective dir = do
--	dirs <- gets directives
--	modify (\st → st { directives = dirs ++ [dir] })
--

-- | Toggles the top item on ifStack (used when processing #else)
--toggleTop ∷ PPM ()
--toggleTop = do
--  action ← popStack
--  modify (\st → st { ifStack = case action of { Keep → Throw; Throw → Keep } })

pushStack ∷ If → PPM ()
pushStack action = do
  ifs ← gets ifStack
  modify (\st → st { ifStack = action : ifs })

-- TODO: Return If instead. Need CError for that.
popStack ∷ PPM ()
popStack = do
  ifs ← gets ifStack
  modify (\st → st { ifStack = drop 1 ifs })


-- | Preprocesses given a line of source code
processLine ∷ String → PPM String
processLine line = do
  case isDirective line of
    -- TODO: Replace all defines.
    -- TODO: Only return if current If-state is keep.
    False → return line
    True → case (extractDeclarative line) of
      ("define", val) → addDefine val >> return ""
      ("undef", name) → delDefine name >> return ("DELETING \"" ++ name ++ "\"")
--      ("ifdef", name) → processIfDef name >> return ""
      _ -> return ""
  where
    isDirective ∷ String → Bool
    isDirective = (=="#") . take 1 . dropWhile (==' ')
    extractDeclarative ∷ String → (String, String)
    -- TODO: Strip both strings in the tuple here.
    extractDeclarative = break (==' ') . drop 1 . strip

-- | Checks if a variable is declared.
isDefined ∷ String → PPM Bool
isDefined name = do
  defs ← gets defines
  return $ Map.member name defs

processIfDef ∷ String -> PPM ()
processIfDef name = do
  isdef ← isDefined name
  if isdef
    then undefined
    else undefined

-- | Adds a definiton writing over existing ones with the same name
-- TODO: Rename to define.
-- TODO: Warning when already declared.
addDefine ∷ String → PPM ()
addDefine line = do
  isDefined name >>= (flip when $ warning (0, 0) "Defining an already defined macro.")
  defs ← gets defines
  modify (\st → st { defines = Map.insert name (strip value) defs })
  where
    (name, value) = (break (==' ') . strip) line

-- | Deletes a definition
delDefine ∷ String → PPM ()
delDefine name = do
  defs ← gets defines
  modify (\st → st { defines = Map.delete (strip name) defs })

strip ∷ String → String
strip = dropWhile (==' ')

-- | Adds a pragma
--addPragma ∷ String → String → PPM ()
--addPragma name pragma = undefined

-- | Preprocess the given source code.
preprocess ∷ String → PPM String
preprocess src = do
	ls ← mapM processLine (lines src)
	return $ concat ls


-- | Errors and warnings
syntaxError ∷ Position → String → PPM a
syntaxError pos msg = throwError $ SyntaxError pos msg

warning ∷ Position → String → PPM ()
warning p w = do
  ws ← gets warnings
  modify (\st → st { warnings = ws ++ [(p, w)] })


-- | Testing of stuff with stuff.
--testPreprocess ∷ FilePath → IO String
--testPreprocess f = do
--	src ← readFile f
--	return src

main ∷ IO ()
----main = testPreprocess "../testcases/preprocessor.fl" >>= putStrLn
main = do
	src ← readFile "../testcases/preprocessor.fl"
	let ret = runStateT (preprocess src) emptyEnv
	print $ show ret
