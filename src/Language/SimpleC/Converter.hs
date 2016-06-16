{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DoAndIfThenElse #-}

{-
  This is a full converter from the C language
  Assumptions:
  1. The code compiles;
  2. The code can be parsed.

  The the main goal of this module
  is to consume the AST of the parse tree
  and produce:
  1. A suitable representation for interpretation
  and analysis:    
  2. Auxiliary 
-}
module Language.SimpleC.Converter where

import Language.C 
import Language.C.Pretty 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident
import qualified Language.SimpleC.AST as SC
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.State.Lazy

{-
State
  Godel_Numbering Function : String
  Symbol Table: Map Int Symbol
  Counter

data Symbol =
  TypeSymbol
  VarSymbol
-}
data ProcessorState node
  = ProcState {
    godel   :: Map (Int, Scope) Int 
  , syms    :: Map SC.SymId Symbol
  , counter :: Int
  , code :: SC.Program SC.SymId node
  , scope :: Scope
  } deriving Show

data Scope = Global | Local | None
  deriving (Show,Eq,Ord)

init_code :: SC.Program ident a
init_code = SC.Prog [] [] []

init_st :: ProcessorState a
init_st = ProcState M.empty M.empty 0 init_code Global 

data Symbol = TypeSym | VarSym Ident
  deriving Show
  
type ProcessorOp node val = State (ProcessorState node) val

-- | API
incCounter :: ProcessorOp a Int
incCounter = do
  p@ProcState{..} <- get
  let c' = counter + 1
  put p {counter = c'}
  return c'

setScope :: Scope -> ProcessorOp a () 
setScope _scope = do
  p@ProcState{..} <- get
  put p {scope = _scope}

-- | Add Declarations: Either a TypeDecl or a Decl
addDeclaration :: SC.Declaration SC.SymId a -> ProcessorOp a ()
addDeclaration d = do 
  p@ProcState{..} <- get
  let decls' = (SC.decls code)++[d]
      code' = code {SC.decls=decls'}
  put p {code = code'} 

addType :: SC.Type SC.SymId a -> ProcessorOp a ()
addType ty = do
  let d = SC.TypeDecl ty
  addDeclaration d

addDecl :: SC.Type SC.SymId a -> SC.DeclElem SC.SymId a -> ProcessorOp a ()
addDecl ty el =
  case el of
    SC.DeclElem (Just (SC.Declr Nothing [] Nothing [] _)) Nothing Nothing -> addType ty
    SC.DeclElem Nothing Nothing Nothing -> addType ty
    _ -> do
      let d = SC.Decl ty el
      addDeclaration d

addFunction :: SC.FunctionDef SC.SymId a -> ProcessorOp a ()
addFunction fun = do 
  p@ProcState{..} <- get
  let defs' = (SC.defs code)++[fun]
      code' = code {SC.defs=defs'}
  put p {code = code'} 

-- | Main Functions
processor :: CTranslationUnit a -> ProcessorState a
processor cprog = 
  let ((),st) = runState (process cprog) init_st
  in st

-- | Main processing class 
class Process a n v  where
  process :: a -> ProcessorOp n v

-- | One of the most important instances
instance Process Ident a SC.SymId where
  process i@(Ident str hash nodeinfo) = do
    p@ProcState{..} <- get
    case M.lookup (hash,scope) godel of
      Nothing -> do
        k <- incCounter
        p@ProcState{..} <- get
        let syms' = M.insert (SC.SymId k) (VarSym i) syms
            godel' = M.insert (hash,scope) k godel
        put p {syms=syms',godel=godel'}
        return $ SC.SymId k
      Just k  -> return $ SC.SymId k 

-- | Convert the 'C Translation Unit'
instance Process (CTranslationUnit a) a () where
  process (CTranslUnit cdecls n) =
    mapM_ (process :: CExternalDeclaration a -> ProcessorOp a ()) cdecls

instance Process (CExternalDeclaration a) a () where
  process cextdecl =
    case cextdecl of
      CDeclExt cdecl -> do
        setScope Global
        process cdecl
      CFDefExt cfun  -> do
        setScope Local
        process cfun
      CAsmExt cstr n -> error "TODO: Support CAsmExt"

instance Process (CDeclaration a) a () where
  process (CDecl cdeclspec cdeclrs n) = do
    ty <- toType cdeclspec
    if null cdeclrs
    then addType ty
    else do -- | Process the C declarators
      (declrs :: [SC.DeclElem SC.SymId a]) <- process cdeclrs 
      mapM_ (addDecl ty) declrs 

instance Process (CDeclaration a) a (SC.Declaration SC.SymId a) where
  process de@(CDecl cdeclspec cdeclrs n) = do
    ty <- toType cdeclspec
    if null cdeclrs
    then return $ SC.TypeDecl ty 
    else case cdeclrs of
      [cdeclr] -> do
        declr <- process cdeclr
        return $ normalizeDecl ty declr
      _ -> error $ "cant process CDeclaration with multiple declarators" 

instance Process (CDeclaration a) a [SC.Declaration SC.SymId a] where
  process de@(CDecl cdeclspec cdeclrs n) = do
    ty <- toType cdeclspec
    if null cdeclrs
    then return $ [SC.TypeDecl ty] 
    else do 
      (declrs :: [SC.DeclElem SC.SymId a]) <- process cdeclrs 
      return $ map (normalizeDecl ty) declrs

normalizeDecl :: SC.Type SC.SymId a -> SC.DeclElem SC.SymId a -> SC.Declaration SC.SymId a
normalizeDecl ty declr = 
  case declr of
    SC.DeclElem (Just (SC.Declr Nothing [] Nothing [] _)) Nothing Nothing -> SC.TypeDecl ty
    SC.DeclElem Nothing Nothing Nothing -> SC.TypeDecl ty
    _ -> SC.Decl ty declr

-- | CDeclarationSpecifier specifies a type
toType :: [CDeclarationSpecifier a] -> ProcessorOp a (SC.Type SC.SymId a)
toType decl_spec = do
  (st,ty,tyqual) <- foldM _toType ([],[],[]) decl_spec
  if null ty
  then error "no type specifier"
  else do 
    case st of
      []  -> return $ SC.Type SC.Auto tyqual ty
      [s] -> return $ SC.Type s       tyqual ty
      _   -> error "toType: more than one storage spec" 
     where
       _toType (st,ty,tyqual) d_spec = 
         case d_spec of
           CStorageSpec s -> do
             _s <- process s
             return (_s:st,ty,tyqual)
           CTypeSpec    t -> do
             _t <- process t
             return (st,_t:ty,tyqual)
           CTypeQual    q -> do
             _q <- process q
             return (st,ty,_q:tyqual)

-- | Process a Declaration Element
instance Process (SC.CDeclElem a) a (SC.DeclElem SC.SymId a) where
  process (mCDeclr, mCInit, mCSizeExpr) = do
    declr <- process mCDeclr 
    mInit <- process mCInit 
    mSizeExpr <- process mCSizeExpr
    return $ SC.DeclElem declr mInit mSizeExpr

-- | Process a Declarator
instance Process (CDeclarator a) a (SC.Declarator SC.SymId a) where
  process cDeclr =
    case cDeclr of 
      CDeclr mIdent cDerDeclr cStr cAttr a -> do
        attr <- process cAttr
        derDeclr <- process cDerDeclr
        case mIdent of
          Nothing ->
            return $ SC.Declr Nothing derDeclr cStr attr a 
          Just ident -> do
            p@ProcState{..} <- get
            if scope == None
            then return $ SC.Declr Nothing derDeclr cStr attr a 
            else do
              sym <- process ident
              return $ SC.Declr (Just sym) derDeclr cStr attr a 
   
-- | Process an Initializer
instance Process (CInitializer a) a (SC.Initializer SC.SymId a) where
  process cInit =
    case cInit of
      CInitExpr cExpr _ -> do
        expr <- process cExpr
        return $ SC.InitExpr expr 
      CInitList list _ -> do
        _list <- process list
        return $ SC.InitList _list

{- 
--  Overlaps with the more general one defined below
-- | Process an InitializerList Element
instance Process (SC.CInitializerListEl a) a (SC.InitializerListEl SC.SymId a) where
  process (cPartList,cInit) = do
    partList <- process cPartList
    init <- process cInit
    return (partList,init)
-}

-- | Process the 'C Part Designator'
instance Process (CPartDesignator a) a (SC.PartDesignator SC.SymId a) where
  process cPartDes = case cPartDes of
    CArrDesig cExpr n -> do
      expr <- process cExpr
      return $ SC.ArrDesig expr 
    CMemberDesig ident n -> do
      sym <- process ident
      return $ SC.MemberDesig sym  
    CRangeDesig cExpr cExpr' n -> do
      expr <- process cExpr
      expr' <- process cExpr'
      return $ SC.RangeDesig expr expr' 

-- | Process the 'C Derived Declarator'
instance Process (CDerivedDeclarator a) a (SC.DerivedDeclarator SC.SymId a) where
  process cDerDeclr = 
    case cDerDeclr of
      CPtrDeclr cTyQual _ -> do
        tyQual <- process cTyQual
        return $ SC.PtrDeclr tyQual
      CArrDeclr cTyQual cArrSize _ -> do
        tyQual <- process cTyQual
        arrSize <- process cArrSize
        return $ SC.ArrDeclr tyQual arrSize
      CFunDeclr eth cAttr _ -> do
        attrs <- process cAttr
        case eth of
          Left idents -> do
            syms <- process idents
            return $ SC.FunDeclr (Left syms) attrs
          Right (cdecls, b) -> do
            -- | Dont care about identifiers for pars 
            s@ProcState{..} <- get
            if scope == Local
            then do
              decls <- process cdecls
              return $ SC.FunDeclr (Right (decls,b)) attrs
            else do 
              let scope' = scope
              put s {scope = None}
              decls <- process cdecls
              put s {scope = scope'}
              return $ SC.FunDeclr (Right (decls,b)) attrs

-- | Process the 'C Array Size' 
instance Process (CArraySize a) a (SC.ArraySize SC.SymId a) where
  process cArrSize =
    case cArrSize of
      CNoArrSize b -> return $ SC.NoArrSize b
      CArrSize b cExpr -> do
        expr <- process cExpr
        return $ SC.ArrSize b expr

-- | Process the 'C Storage Specifier' 
instance Process (CStorageSpecifier a) a SC.StorageSpecifier where
  process cStorSpec = do 
    let storSpec = case cStorSpec of
          CAuto n     -> SC.Auto 
          CRegister n -> SC.Register 
          CStatic n   -> SC.Static 
          CExtern n   -> SC.Extern 
          CTypedef n  -> SC.Typedef
          CThread n   -> SC.Thread
    return storSpec 

-- | Convert the 'C Type Qualifier'
instance Process (CTypeQualifier a) a (SC.TypeQualifier SC.SymId a) where
  process cTypeQualifier = 
    case cTypeQualifier of
      CConstQual n  -> return $ SC.ConstQual  
      CVolatQual n  -> return $ SC.VolatQual 
      CRestrQual n  -> return $ SC.RestrQual 
      CInlineQual n -> return $ SC.InlineQual
      CAttrQual cAttribute -> do
         attr <- process cAttribute
         return $ SC.AttrQual attr 

-- | Convert the 'C Type Specifier'
instance Process (CTypeSpecifier a) a (SC.TypeSpecifier SC.SymId a) where
  process ctyspec = case ctyspec of
    CVoidType n    -> return $ SC.VoidType
    CCharType n    -> return $ SC.CharType
    CShortType n   -> return $ SC.ShortType
    CIntType n     -> return $ SC.IntType
    CLongType n    -> return $ SC.LongType
    CFloatType n   -> return $ SC.FloatType
    CDoubleType n  -> return $ SC.DoubleType
    CSignedType n  -> return $ SC.SignedType
    CUnsigType n   -> return $ SC.UnsigType
    CBoolType n     -> return $ SC.BoolType
    CComplexType n -> return $ SC.ComplexType
    CTypeDef ident n -> do
      sym <- process ident
      return $ SC.TypeDef sym n
    CSUType cStructureUnion n -> do
      structOrUnion <- process cStructureUnion
      return $ SC.SUType structOrUnion 
    CEnumType cEnumeration n -> do
      enum <- process cEnumeration
      return $ SC.EnumType enum n
    CTypeOfExpr cExpression n -> do
      expr <- process cExpression 
      return $ SC.TypeOfExpr expr n
    CTypeOfType cDeclaration n -> do
      decl <- process cDeclaration 
      return $ SC.TypeOfType decl n

-- | Process 'C StructureUnion'
instance Process (CStructureUnion a) a (SC.StructureUnion SC.SymId a) where
  process cStruct =
    case cStruct of
      CStruct tag mIdent mDecl cAttr n -> do
        sym <- process mIdent
        decl <- process mDecl
        attrs <- process cAttr
        return $ SC.Struct tag sym decl attrs n

-- | Process 'C Enumeration'
instance Process (CEnumeration a) a (SC.Enumeration SC.SymId a) where
  process (CEnum mIdent mAuxPair lCAttr n) = do
    mSym <- process mIdent
    auxPair <- process mAuxPair
    lAttr <- process lCAttr
    return $ SC.Enum mSym auxPair lAttr n
 
-- | Process 'C Attribute'
instance Process (CAttribute a) a (SC.Attribute SC.SymId a) where
  process cAttribute =
    case cAttribute of
      CAttr ident lCExpr n -> do
        sym <- process ident
        lExpr <- process lCExpr
        return $ SC.Attr sym lExpr 

-- | Convert the 'C Expression'
instance Process (CExpression a) a (SC.Expression SC.SymId a) where
  process cExpr = case cExpr of
     CAlignofExpr cExpr n -> do 
       expr <- process cExpr
       return $ SC.AlignofExpr expr
     CAlignofType cDecl n -> do
       decl <- process cDecl
       return $ SC.AlignofType decl
     CAssign op lhsExpr rhsExpr n -> do
       lhs <- process lhsExpr
       rhs <- process rhsExpr
       return $ SC.Assign op lhs rhs 
     CBinary op lhsExpr rhsExpr n -> do
       lhs <- process lhsExpr
       rhs <- process rhsExpr
       return $ SC.Binary op lhs rhs 
     CBuiltinExpr cBExpr -> do
       expr <- process cBExpr
       return $ SC.BuiltinExpr expr
     CCall fnExpr argsExpr n -> do 
       fn <- process fnExpr
       args <- process argsExpr
       return $ SC.Call fn args n
     CCast cDecl cExpr n -> do
       decl <- process cDecl
       expr <- process cExpr
       return $ SC.Cast decl expr
     CComma cExprs n -> do
       exprs <- process cExprs
       return $ SC.Comma exprs 
     CCompoundLit cDecl cInit n -> do
       decl <- process cDecl
       init <- process cInit
       return $ SC.CompoundLit decl init
     CCond condExpr mThenExpr elseExpr n -> do
       cond <- process condExpr
       mThen <- process mThenExpr
       _else <- process elseExpr
       return $ SC.Cond cond mThen _else
     CConst cConst -> do
       const <- process cConst
       return $ SC.Const const 
     CIndex lhsExpr rhsExpr n -> do 
       lhs <- process lhsExpr
       rhs <- process rhsExpr
       return $ SC.Index lhs rhs 
     CLabAddrExpr ident n -> do
       sym <- process ident 
       return $ SC.LabAddrExpr sym 
     CMember cExpr ident bool n -> do
       expr <- process cExpr 
       sym <- process ident
       return $ SC.Member expr sym bool
     CSizeofExpr cExpr n -> do 
       expr <- process cExpr
       return $ SC.SizeofExpr expr 
     CSizeofType cDecl n -> do
       decl <- process cDecl
       return $ SC.SizeofType decl 
     CStatExpr cStat n -> do
       stat <- process cStat
       return $ SC.StatExpr stat
     CUnary op cExpr n -> do
       expr <- process cExpr
       return $ SC.Unary op expr 
     CVar ident n -> do
       sym <- process ident
       return $ SC.Var sym
     _ -> error ("Expression not supported")
 
-- | Process the 'C BuiltinThing'
instance Process (CBuiltinThing a) a (SC.BuiltinThing SC.SymId a) where
  process cBuiltinThing = 
    case cBuiltinThing of
      CBuiltinVaArg cExpr cDecl n -> do
        expr <- process cExpr
        decl <- process cDecl
        return $ SC.BuiltinVaArg expr decl n
      CBuiltinOffsetOf cDecl lcPartDes n -> do
        decl <- process cDecl
        lpartDes <- process lcPartDes
        return $ SC.BuiltinOffsetOf decl lpartDes n 
      CBuiltinTypesCompatible cDecl _cDecl n -> do
        decl <- process cDecl
        _decl <- process _cDecl
        return $ SC.BuiltinTypesCompatible decl _decl n 

-- | Process the 'C Constant'
instance Process (CConstant a) a SC.Constant where
  process cConst = do 
    let const = case cConst of
          CIntConst cInteger n -> SC.IntConst cInteger
          CCharConst cChar n   -> SC.CharConst cChar 
          CFloatConst cFloat n -> SC.FloatConst cFloat 
          CStrConst cString n  -> SC.StrConst cString
    return const 

-- | Process the 'C Statement'
instance Process (CStatement a) a (SC.Statement SC.SymId a) where
  process cStat = case cStat of
    -- Break statement
    CBreak n -> return $ SC.Break n
    -- A statement of the form case expr : stmt
    CCase cExpr cStat n -> do 
      expr <- process cExpr
      stat <- process cStat
      return $ SC.Case expr stat n 
    -- A case range of the form case lower ... upper : stmt
    CCases cExpr _cExpr cStat n -> do
      expr <- process cExpr
      _expr <- process _cExpr
      stat <- process cStat
      return $ SC.Cases expr _expr stat n
    -- Continue statement
    CCont n -> return $ SC.Cont n 
    -- The default case default : stmt
    CDefault cStat n -> do
      stat <- process cStat
      return $ SC.Default stat n
    -- A simple statement, that is in C:
    -- evaluating an expression with side-effects
    -- and discarding the result
    CExpr mCExpr n -> do
      mExpr <- process mCExpr 
      return $ SC.Expr mExpr n
    -- For statement CFor init expr-2 expr-3 stmt,
    -- where init is either a declaration or initializing expression
    CFor cInit cExpr _cExpr cStat n -> do 
      init <- process cInit 
      expr <- process cExpr 
      _expr <- process _cExpr 
      stat <- process cStat 
      return $ SC.For init expr _expr stat n 
    -- Goto statement CGoto label
    CGoto ident n -> do
      sym <- process ident
      return $ SC.Goto sym n 
    -- Computed goto CGotoPtr labelExpr
    CGotoPtr cExpr n -> do
      expr <- process cExpr
      return $ SC.GotoPtr expr n
    -- Conditional statement CIf ifExpr thenStmt maybeElseStmt at
    CIf cCond cThen mCElse n -> do
      cond <- process cCond 
      thenS <- process cThen
      mElse  <- process mCElse
      return $ SC.If cond thenS mElse n
    -- An (attributed) label followed by a statement
    CLabel ident cStat lCAttr n -> do
      sym <- process ident
      stat <- process cStat
      lcAttr <- process lCAttr
      return $ SC.Label sym stat lcAttr n
    -- Return statement CReturn returnExpr
    CReturn mCExpr n -> do
      mExpr <- process mCExpr
      return $ SC.Return mExpr n
    -- Switch statement CSwitch selectorExpr switchStmt, 
    -- where switchStmt usually includes case, break and default statements
    CSwitch cExpr cStat n -> do
      expr <- process cExpr
      stat <- process cStat
      return $ SC.Switch expr stat n
    -- While or do-while statement CWhile guard stmt isDoWhile at
    CWhile cExpr cStat isDoWhile n -> do
      guard <- process cExpr
      stmt <- process cStat
      return $ SC.While guard stmt isDoWhile n
    -- Assembly statement	
    CAsm cAsmStmt n -> 
      error "CAsm statement is not support"
      -- CAsm (process cAsmStmt) () 
    -- Compound statement CCompound localLabels blockItems at
    CCompound idents cCompoundBlockItem n -> do
      syms <- process idents
      compoundBlockItem <- process cCompoundBlockItem
      return $ SC.Compound syms compoundBlockItem n

-- | Process the 'C CompoundBlockItem'
instance Process (CCompoundBlockItem a) a (SC.CompoundBlockItem SC.SymId a) where
  process cCompound = case cCompound of
    -- A statement
    CBlockStmt cStat -> do
      stat <- process cStat
      return $ SC.BlockStmt stat
    -- A local declaration
    CBlockDecl cDecl -> do
      decl <- process cDecl
      return $ SC.BlockDecl decl 
    -- A nested function (GNU C)
    CNestedFunDef cFunDef -> do
      fun <- process cFunDef
      return $ SC.NestedFunDef fun

-- | Process a 'C Function Definition'
instance Process (CFunctionDef a) a () where
  process cFun = do
    fun <- process cFun 
    addFunction fun 

-- | Process a 'C Function Definition'
instance Process (CFunctionDef a) a (SC.FunctionDef SC.SymId a) where
  process (CFunDef lCDeclSpec cDeclr lCDecl cStat n) = do
    ty <- toType lCDeclSpec
    sym <- process cDeclr
    pars <- process lCDecl
    body <- process cStat
    let fun = SC.FunDef ty sym pars body n
    return fun 

-- | Process Maybe versions
instance (Process a n b) => Process (Maybe a) n (Maybe b) where
  process Nothing = return Nothing
  process (Just expr) =
    process expr >>= return . Just 

-- | Process Either versions
instance (Process a n b, Process c n d) => 
  Process (Either a c) n (Either b d) where
  process (Left a) = process a >>= return . Left
  process (Right b) = process b >>= return . Right

-- | Process Pair versions
instance (Process a n b, Process c n d) => 
  Process (a,c) n (b,d) where
  process (a,c) = do
    b <- process a
    d <- process c
    return $ (b,d)


-- | Process List versions
instance (Process a n b) => Process [a] n [b] where
  process a = mapM process a

{-

-- | Process Triple versions
instance (Process a n b, Process c n d, Process e n f) => 
	Process (a,c,e) n (b,d,f) where
    process (a,c,e) = do 
      b <- process a
      d <- process c
      f <- process e
      return (b,d,f)

-- | Convert the 'C AssemblyStatement'
instance Convertible (CAssemblyStatement NodeInfo) (CAssemblyStatement ()) where
    translate cAsmStmt = case cAsmStmt of
	CAsmStmt mCTyQual cStrLit lCAsmOp _lCAsmOp lCStrLit n ->
			let mTyQual = translate mCTyQual
		strLit = translate cStrLit
		lAsmOp = translate lCAsmOp
		_lAsmOp = translate _lCAsmOp
		lStrLit = translate lCStrLit
			in CAsmStmt mTyQual strLit lAsmOp _lAsmOp lStrLit ()

-- | Convert the 'C AssemblyOperand'
instance Convertible (CAssemblyOperand NodeInfo) (CAssemblyOperand ()) where
    translate cAsmOp = case cAsmOp of
	CAsmOperand mIdent cStrLit cExpr n ->
			let strLit = translate cStrLit
		expr = translate cExpr
			in CAsmOperand mIdent strLit expr ()




-}
