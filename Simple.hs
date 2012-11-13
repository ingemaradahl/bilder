{-# LANGUAGE UnicodeSyntax #-}

module Simple where

import qualified Simple.AbsSimple as S
import Simple.Types

import qualified FrontEnd.AbsGLSL as G
--import qualified FrontEnd.AbsGrammar as F


-- Simple to GLSL {{{
-- | Translates Simple to GLSL tree.
simpleToGLSL ∷ SimpleBlob → G.Tree
simpleToGLSL blob = G.Tree $
  map structToGLSL (structs blob) ++
  map (G.TopDecl . varToGLSLDecl) (variables blob) ++
  map funToPrototype (functions blob) ++
  map funToGLSL (functions blob)

funToPrototype ∷ S.Function → G.TopLevel
funToPrototype fun = G.FunctionPrototype
  (typeToGLSL (S.returnType fun)) -- return type
  (G.Ident $ S.functionName fun) -- name
  (map variableToGLSLParam (S.parameters fun)) -- parameters

funToGLSL ∷ S.Function → G.TopLevel
funToGLSL fun = G.Function
  (typeToGLSL (S.returnType fun)) -- return type
  (G.Ident $ S.functionName fun) -- name
  (map variableToGLSLParam (S.parameters fun)) -- parameters
  (concatMap stmToGLSL (S.statements fun)) -- statements

stmToGLSL ∷ S.Stm → [G.Stm]
stmToGLSL (S.SDecl v) = [G.SDecl (varToGLSLDecl v)]
stmToGLSL (S.SDeclAss v e) =
  [G.SDecl (varToGLSLDecl v),
   G.SExp (G.EAss (G.EVar (G.Ident $ S.variableName v)) (expToGLSL e))]
stmToGLSL (S.SStruct s) =
  [G.SDecl $ G.Struct (G.Ident $ S.structName s)
    [G.SVDecl (varToGLSLDecl v) | v ← S.declarations s]]
stmToGLSL (S.SExp e) = [G.SExp $ expToGLSL e]
stmToGLSL (S.SWhile e ss) =
  [G.SWhile (expToGLSL e) (G.SBlock $ concatMap stmToGLSL ss)]
stmToGLSL (S.SDoWhile ss e) =
  [G.SDoWhile (G.SBlock $ concatMap stmToGLSL ss) (expToGLSL e)]
stmToGLSL (S.SFor dss ec el ss)
  | not $ isForDecl dss && dss /= []  = error $ "compiler error - for statements must only contain SDeclAss or SExp: " ++ show dss
  | otherwise  = [G.SFor [] (map expToGLSL ec) (map expToGLSL el) (G.SBlock $ concatMap stmToGLSL ss)]
stmToGLSL (S.SReturn e) = [G.SReturn (expToGLSL e)]
stmToGLSL (S.SVoidReturn) = [G.SVoidReturn]
stmToGLSL (S.SIf e ss) = [G.SIf (expToGLSL e) (G.SBlock $ concatMap stmToGLSL ss)]
stmToGLSL (S.SIfElse e sst ssf) = [G.SIfElse (expToGLSL e) (G.SBlock $ concatMap stmToGLSL sst) (G.SBlock $ concatMap stmToGLSL ssf)]
stmToGLSL (S.SBreak) = [G.SBreak]
stmToGLSL (S.SContinue) = [G.SContinue]
stmToGLSL (S.SDiscard) = [G.SDiscard]

isForDecl ∷ [S.Stm] → Bool
isForDecl [] = True
isForDecl (S.SDeclAss {}:ss) = isForDecl ss
isForDecl (S.SExp {}:ss) = isForDecl ss
isForDecl _ = False

expToGLSL ∷ S.Exp → G.Exp
expToGLSL (S.EAss el er) = G.EAss (expToGLSL el) (expToGLSL er)
expToGLSL (S.EAssAdd el er) = G.EAssAdd (expToGLSL el) (expToGLSL er)
expToGLSL (S.EAssSub el er) = G.EAssSub (expToGLSL el) (expToGLSL er)
expToGLSL (S.EAssMul el er) = G.EAssMul (expToGLSL el) (expToGLSL er)
expToGLSL (S.EAssDiv el er) = G.EAssDiv (expToGLSL el) (expToGLSL er)
expToGLSL (S.EAssMod el er) = G.EAssMod (expToGLSL el) (expToGLSL er)
expToGLSL (S.EAssBWAnd el er) = G.EAssBWAnd (expToGLSL el) (expToGLSL er)
expToGLSL (S.EAssBWXOR el er) = G.EAssBWXOR (expToGLSL el) (expToGLSL er)
expToGLSL (S.EAssBWOR el er) = G.EAssBWOR (expToGLSL el) (expToGLSL er)
expToGLSL (S.ECond ec et ef) =
  G.ECond (expToGLSL ec) (expToGLSL et) (expToGLSL ef)
expToGLSL (S.EOR el er) = G.EOR (expToGLSL el) (expToGLSL er)
expToGLSL (S.EXOR el er) = G.EXOR (expToGLSL el) (expToGLSL er)
expToGLSL (S.EAnd el er) = G.EAnd (expToGLSL el) (expToGLSL er)
expToGLSL (S.EBWOR el er) = G.EBWOR (expToGLSL el) (expToGLSL er)
expToGLSL (S.EBWXOR el er) = G.EBWXOR (expToGLSL el) (expToGLSL er)
expToGLSL (S.EBWAnd el er) = G.EBWAnd (expToGLSL el) (expToGLSL er)
expToGLSL (S.EEqual el er) = G.EEqual (expToGLSL el) (expToGLSL er)
expToGLSL (S.ENEqual el er) = G.ENEqual (expToGLSL el) (expToGLSL er)
expToGLSL (S.ELt el er) = G.ELt (expToGLSL el) (expToGLSL er)
expToGLSL (S.EGt el er) = G.EGt (expToGLSL el) (expToGLSL er)
expToGLSL (S.ELEt el er) = G.ELEt (expToGLSL el) (expToGLSL er)
expToGLSL (S.EGEt el er) = G.EGEt (expToGLSL el) (expToGLSL er)
expToGLSL (S.EBWShiftLeft el er) =
  G.EBWShiftLeft (expToGLSL el) (expToGLSL er)
expToGLSL (S.EBWShiftRight el er) =
  G.EBWShiftRight (expToGLSL el) (expToGLSL er)
expToGLSL (S.EAdd el er) = G.EAdd (expToGLSL el) (expToGLSL er)
expToGLSL (S.ESub el er) = G.ESub (expToGLSL el) (expToGLSL er)
expToGLSL (S.EMul el er) = G.EMul (expToGLSL el) (expToGLSL er)
expToGLSL (S.EDiv el er) = G.EDiv (expToGLSL el) (expToGLSL er)
expToGLSL (S.EMod el er) = G.EMod (expToGLSL el) (expToGLSL er)
expToGLSL (S.ENeg e) = G.ENeg (expToGLSL e)
expToGLSL (S.ENegSign e) = G.ENegSign (expToGLSL e)
expToGLSL (S.EComplement e) = G.EComplement (expToGLSL e)
expToGLSL (S.EPos e) = G.EPos (expToGLSL e)
expToGLSL (S.EPreInc e) = G.EPreInc (expToGLSL e)
expToGLSL (S.EPreDec e) = G.EPreDec (expToGLSL e)
expToGLSL (S.EPostInc e) = G.EPostInc (expToGLSL e)
expToGLSL (S.EPostDec e) = G.EPostDec (expToGLSL e)
expToGLSL (S.EMember e i) = G.ESwizzler (expToGLSL e) (G.EVar $ G.Ident i)
expToGLSL (S.ECall i es) = G.ECall (G.Ident i) (map expToGLSL es)
expToGLSL (S.EVar i) = G.EVar (G.Ident i)
expToGLSL (S.EIndex i e) = G.EIndex (G.Ident i) (expToGLSL e)
expToGLSL (S.EFloat f) = G.EFloat (G.CFloat (show f))
expToGLSL (S.EInt i) = G.EInt i
expToGLSL (S.ETrue) = G.ETrue
expToGLSL (S.EFalse) = G.EFalse

variableToGLSLParam ∷ S.Variable → G.Param
variableToGLSLParam var = G.ParamDec
  (map paramQualsGLSL $ S.qualifiers var)
  (typeToGLSL $ S.variableType var)
  (G.Ident $ S.variableName var)

paramQualsGLSL ∷ S.Qualifier → G.ParamQualifiers
paramQualsGLSL (S.Const) = G.PQStorage G.QConst
paramQualsGLSL (S.External) = undefined

typeToGLSL ∷ S.Type → G.Type
typeToGLSL (S.TVoid) = G.TVoid
typeToGLSL (S.TFloat) = G.TFloat
typeToGLSL (S.TBool) = G.TBool
typeToGLSL (S.TInt) = G.TInt
typeToGLSL (S.TVec2) = G.TVec2
typeToGLSL (S.TVec3) = G.TVec3
typeToGLSL (S.TVec4) = G.TVec4
typeToGLSL (S.TMat2) = G.TMat2
typeToGLSL (S.TMat3) = G.TMat3
typeToGLSL (S.TMat4) = G.TMat4
typeToGLSL (S.TStruct i) = G.TStruct (G.Ident i)

declQualsGLSL ∷ S.Qualifier → G.DeclQualifiers
declQualsGLSL (S.Const) = G.DQStorage G.QConst
declQualsGLSL (S.External) = undefined

structToGLSL ∷ S.Struct → G.TopLevel
structToGLSL s = G.TopDecl $ G.Struct
  (G.Ident $ S.structName s)
  (map (G.SVDecl . varToGLSLDecl) (S.declarations s))

varToGLSLDecl ∷ S.Variable → G.Decl
varToGLSLDecl var = G.Declaration
  (map declQualsGLSL (S.qualifiers var)) -- qualifiers
  (typeToGLSL $ S.variableType var) -- type
  [G.Ident $ S.variableName var] -- names


-- }}}
-- FL to Simple {{{
-- | Translates FL tree to Simple.
--flToSimple ∷ F.AbsTree → S.AbsTree
--flToSimple _ = undefined

-- }}}

-- vi:fdm=marker
