{-# LANGUAGE UnicodeSyntax #-}

module Compiler where

import Prelude hiding (mapM)

import Control.Monad.State hiding (mapM)
import Control.Monad.Reader hiding (mapM)

import Control.Arrow
import Control.Applicative
import qualified Data.Map as Map
import Data.Traversable

import Utils

import TypeChecker.Types (Source)

import Compiler.Simple.AbsSimple
import Compiler.Simple.Types
import Compiler.Simple.Utils
import Compiler.GLSL.Utils

import CompilerError
import Compiler.Utils (liftCError)

import qualified Compiler.Lifter as L
import Compiler.Desugar (desugar)
import Compiler.Split (splitSource)
import Compiler.Merge (mergeShaders)
import Compiler.Clean (clean)
import Compiler.SimpleInliner (simpleInline)
import Compiler.Simple (absToSimple, simpleToGLSL)
import Compiler.Texture2D (texture2D)
import Compiler.AvoidAlias (avoidAlias)

import Text.JSON


data Options = Options {
  inputFile ∷ FilePath
}
  deriving (Show)

data Environment = Env {
  options ∷ Options,
  warnings ∷ [String]
}

buildEnv ∷ Options → Environment
buildEnv opts = Env {
  options = opts,
  warnings = []
}

-- CPM - CompilerMonad: Alias for both Error and State monad
type CPM a = StateT Environment CError a

compileTree ∷ Options → Source → CError JSValue
compileTree opts src = evalStateT (compile src) (buildEnv opts)

lambdaLift ∷ Source → CPM Source
lambdaLift src = do
  env ← liftCError $ execStateT L.lambdaLift (L.buildEnv src)
  modify (\s → s {
      warnings = L.warnings env ++ warnings s
    })
  return $ L.source env

compile ∷ Source → CPM JSValue
compile src =
  liftM (
        desugar
    >>> splitSource       -- Split to [Shader] with AbsGrammar
    >>> absToSimple       -- Convert to SimpleGrammar
    >>> mergeShaders      -- Try to merge shaders based on sample count
    >>> map clean         -- Removes unnecessary statements and functions
    >>> map simpleInline  -- Performes simple inlining
    >>> map clean
    >>> map avoidAlias    -- Reduces unneccecary aliases, such as Image a = b;
    >>> map pixelMode     -- Set pixel mode of functions
    >>> map finalizeMain  -- Replace main-function with GLSL-variant
    >>> map clean
    >>> map avoidAlias    -- Reduces unneccecary aliases, such as Image a = b;
    >>> map renameBuiltin -- Re-renames built in functions
    >>> map addResUniform -- Adds resolution uniform fl_Resolution
    >>> map texture2D
    >>> simpleToGLSL      -- Translate to GLSL
  ) $ lambdaLift src


finalizeMain ∷ Shader → Shader
finalizeMain shd =
  shd { functions = Map.adjust reworkMain "main" (functions shd) }

reworkMain ∷ Function → Function
reworkMain (Function name _ px [x, y] stms) = Function name TVoid px [] (d:stms')
 where
  p = Variable "p" TVec2 True Nothing
  d = SDeclAss p $ if px then gl_FragCoord_xy else EDiv gl_FragCoord_xy fl_Resolution
  x' = EMember (EVar (variableName p)) "x"
  y' = EMember (EVar (variableName p)) "y"
  stms' = subReturn $ map (mapStmExp subXY) stms
  subXY ∷ Exp → Exp
  subXY (EVar s) | s == variableName x = x'
  subXY (EVar s) | s == variableName y = y'
  subXY e = mapExp subXY e
  subReturn ∷ [Stm] → [Stm]
  subReturn (SReturn e:s) = SExp (EAss gl_FragColor e):SVoidReturn:s
  subReturn ss = expandStm subReturn ss

renameBuiltin ∷ Shader → Shader
renameBuiltin sh = sh { functions =
    Map.map (\f → f { statements = map (mapStmExp cleanBuiltin) (statements f)})
    (functions sh)
  }

-- Adding argument and exit point conversion to pixelwise functions
pixelMode ∷ Shader → Shader
pixelMode s = s { functions = Map.map (\f → runReader (pixelModeF f) (f, s)) (functions s) }

pixelModeF ∷ Function → Reader (Function, Shader) Function
pixelModeF f | pixelwise f = do
  let stmHead = mkAss (leave 2 (parameters f))
  stmTail ← mapM (mapStmExpM rewriteExits) (statements f)
  return $ f { statements = stmHead ++ stmTail }
             | otherwise = return f
 where
  mkAss ∷ [Variable] → [Stm]
  mkAss (Variable x _ _ _:Variable y _ _ _:[]) =
    [SExp (EAssMul (EVar x) fl_Resolution_x), SExp (EAssMul (EVar y) fl_Resolution_y) ]

rewriteExits ∷ Exp → Reader (Function,Shader) Exp
rewriteExits (ECall n es) | length es >= 2 = do
  f ← asks (Map.lookup n . functions . snd)
  case f of
    Just fun →
      if (returnType fun == TVec4) && all isNum (leave 2 $ map variableType (parameters fun))
        then return $ ECall n $ take (length es - 2) es ++ divByRes (leave 2 es)
        else ECall n <$> mapM rewriteExits es
    Nothing → do
      ins  ← asks (Map.lookup n . inputs . snd)
      pars ← asks (lookup n . map (variableName &&& id) . parameters . fst)
      case ins `mplus` pars of
        Just (Variable _ TSampler _ _) → do
          unless (length es == 2) $ error "Should not happen! D:"
          return $ ECall n $ divByRes es
        Nothing → ECall n <$> mapM rewriteExits es
rewriteExits e = mapExpM rewriteExits e

divByRes ∷ [Exp] → [Exp]
divByRes (ex:ey:_) = [EDiv ex fl_Resolution_x, EDiv ey fl_Resolution_y]
--

-- Worlds ugliest hack below, don't read
cleanBuiltin ∷ Exp → Exp
cleanBuiltin (ECall s es) | matchesBuiltin s =
  ECall (drop 5 s) $ map (mapExp cleanBuiltin) es
cleanBuiltin e = mapExp cleanBuiltin e

matchesBuiltin ∷ String → Bool
matchesBuiltin ('_':'x':_) = True
matchesBuiltin _ = False

addResUniform ∷ Shader → Shader
addResUniform sh = sh { inputs =
    Map.insert "fl_Resolution" (Variable "fl_Resolution" TVec2 False Nothing) (inputs sh)
  }
