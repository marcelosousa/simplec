{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RecordWildCards #-}

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
module Language.SimpleC.FConverter where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident
import qualified Language.SimpleC.FAST as SC
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
  , syms    :: Map Int Symbol
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

toSymbol :: Ident -> ProcessorOp a SC.SymId
toSymbol i@(Ident str hash nodeinfo) = do
  p@ProcState{..} <- get
  case M.lookup (hash,scope) godel of
    Nothing -> do
      k <- incCounter
      p@ProcState{..} <- get
      let syms' = M.insert k (VarSym i) syms
          godel' = M.insert (hash,scope) k godel
      put p {syms=syms',godel=godel'}
      return k
    Just k  -> return k 
  

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
addDecl ty el = do
  let d = SC.Decl ty el
  addDeclaration d

-- | Main Functions
processor :: CTranslationUnit a -> ProcessorState a
processor cprog = 
  let ((),st) = runState (process cprog) init_st
  in st

-- | Main processing class 
class Process a n v  where
  process :: a -> ProcessorOp n v

-- | Convert the 'C Translation Unit'
instance Process (CTranslationUnit a) a () where
  process (CTranslUnit cdecls n) =
    mapM_ (process :: CExternalDeclaration a -> ProcessorOp a ()) cdecls

instance Process (CExternalDeclaration a) a () where
  process cextdecl =
    case cextdecl of
      CDeclExt cdecl -> process cdecl
      CFDefExt cfun  -> process cfun
      CAsmExt cstr n -> error "TODO: Support CAsmExt"

instance Process (CDeclaration a) a () where
  process (CDecl cdeclspec cdeclrs n) = do
    ty <- toType cdeclspec
    if null cdeclrs
    then addType ty
    else do
      -- | Process the C declarators
      declrs <- mapM process cdeclrs
      mapM_ (addDecl ty) declrs 

instance Process (CDeclaration a) a (SC.Declaration SC.SymId a) where
  process (CDecl cdeclspec cdeclrs n) = do
    ty <- toType cdeclspec
    if null cdeclrs
    then return $ SC.TypeDecl ty 
    else case cdeclrs of
      [cdeclr] -> do
        declr <- process cdeclr
        let d = SC.Decl ty declr
        return d
      _ -> error "cant process CDeclaration with multiple declarators" 

-- | CDeclarationSpecifier specifies a type
toType :: [CDeclarationSpecifier a] -> ProcessorOp a (SC.Type SC.SymId a)
toType decl_spec = do
  (st,ty,tyqual) <- foldM _toType ([],[],[]) decl_spec
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
  process (mCDeclr, mCInit, mCSizeExpr) =
    case mCDeclr of
      Nothing -> error "empty declarator"
      Just cdeclr ->
        case mCSizeExpr of
          Nothing -> do
            declr <- process cdeclr
            mInit <- process mCInit
            return $ SC.DeclElem declr mInit
          Just _ -> error "Process CDeclElem not supported"

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
              sym <- toSymbol ident
              return $ SC.Declr (Just sym) derDeclr cStr attr a 
   
-- | Process an Initializer
instance Process (CInitializer a) a (SC.Initializer SC.SymId a) where
  process cInit =
    case cInit of
      CInitExpr expr _ -> return $ SC.InitExpr expr 
      CInitList list _ -> return $ SC.InitList list

-- | Process the 'C Derived Declarator'
instance Process (CDerivedDeclarator a) a (SC.DerivedDeclarator SC.SymId a) where
  process cDerDeclr = 
    case cDerDeclr of
      CPtrDeclr cTyQual _ -> do
        tyQual <- mapM process cTyQual
        return $ SC.PtrDeclr tyQual
      CArrDeclr cTyQual cArrSize _ -> do
        tyQual <- mapM process cTyQual
        arrSize <- process cArrSize
        return $ SC.ArrDeclr tyQual arrSize
      CFunDeclr eth cAttr _ -> do
        attrs <- mapM process cAttr
        case eth of
          Left idents -> do
            syms <- mapM toSymbol idents
            return $ SC.FunDeclr (Left syms) attrs
          Right (cdecls, b) -> do
            -- | Dont care about identifiers for pars 
            s@ProcState{..} <- get
            let scope' = scope
            put s {scope = None}
            decls <- mapM process cdecls
            put s {scope = scope'}
            return $ SC.FunDeclr (Right (decls,b)) attrs

-- | Process the 'C Array Size' 
instance Process (CArraySize a) a (SC.ArraySize SC.SymId a) where
  process cArrSize =
    case cArrSize of
      CNoArrSize b -> return $ SC.NoArrSize b
      CArrSize b expr -> return $ SC.ArrSize b expr

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
    CVoidType n	   -> return $ SC.VoidType
    CCharType n	   -> return $ SC.CharType	
    CShortType n   -> return $ SC.ShortType	
    CIntType n	   -> return $ SC.IntType
    CLongType n	   -> return $ SC.LongType	
    CFloatType n   -> return $ SC.FloatType
    CDoubleType n  -> return $ SC.DoubleType
    CSignedType n  -> return $ SC.SignedType
    CUnsigType n   -> return $ SC.UnsigType
    CBoolType n	   -> return $ SC.BoolType
    CComplexType n -> return $ SC.ComplexType
    CTypeDef ident n -> do
      sym <- toSymbol ident
      return $ SC.TypeDef sym n
    CSUType cStructureUnion n -> do
      structOrUnion <- process cStructureUnion
      return $ SC.SUType structOrUnion 
    CEnumType cEnumeration n -> 
      return $ SC.EnumType cEnumeration n
    CTypeOfExpr cExpression n -> 
      error "process CTypeOfExpr not supported"
    CTypeOfType cDeclaration n ->
      error "process CTypeOfType not supported"

instance Process (CStructureUnion a) a (SC.StructureUnion SC.SymId a) where
  process cStruct =
    case cStruct of
      CStruct tag mIdent mDecl cAttr n ->
        case mIdent of
          Nothing -> error "Struct without identifier"
          Just ident -> do
            sym <- toSymbol ident
            decl <- process mDecl
            attrs <- process cAttr
            return $ SC.Struct tag sym decl attrs n

instance Process (CAttribute a) a (SC.Attribute SC.SymId a) where
  process cAttribute =
    case cAttribute of
      CAttr ident lCExpr n -> do
        sym <- toSymbol ident
        return $ SC.Attr sym lCExpr

instance Process (CFunctionDef a) a t where
  process = undefined

-- | Process Maybe versions
instance (Process a n b) => Process (Maybe a) n (Maybe b) where
  process Nothing = return Nothing
  process (Just expr) = do 
    _expr <- process expr
    return $ Just _expr 

-- | Process List versions
instance (Process a n b) => Process [a] n [b] where
  process a = mapM process a
{-
-- | Convert the 'C External Declaration'
instance Convertible (CExternalDeclaration NodeInfo) (CExternalDeclaration ()) where
    translate cdecl = case cdecl of
	CDeclExt cDeclaration	  -> CDeclExt $ translate cDeclaration
	CFDefExt cFunctionDef	  -> CFDefExt $ translate cFunctionDef 
	CAsmExt  cStringLiteral n -> CAsmExt  (translate cStringLiteral) ()
 
-- | Convert the 'C Declaration'
--   CDecl [CDeclarationSpecifier a] [(Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a))] a	
instance Convertible (CDeclaration NodeInfo) (CDeclaration ()) where
    translate (CDecl cDeclSpecifiers cDeclts n) =
	let scDeclSpecifiers = translate cDeclSpecifiers
	    scDeclts         = translate cDeclts
        in CDecl scDeclSpecifiers scDeclts ()

-- | Convert the 'C Declaration Specifier'
instance Convertible (CDeclarationSpecifier NodeInfo) (CDeclarationSpecifier ()) where
    translate cdeclspec = case cdeclspec of
	CStorageSpec cStorageSpecifier -> CStorageSpec $ translate cStorageSpecifier	
	CTypeSpec cTypeSpecifier -> CTypeSpec $ translate cTypeSpecifier
	CTypeQual cTypeQualifier -> CTypeQual $ translate cTypeQualifier


-- | Convert the 'C Type Specifier'
instance Convertible (CTypeSpecifier NodeInfo) (CTypeSpecifier ()) where
    translate ctyspec = case ctyspec of
	CVoidType n	  -> CVoidType ()	
	CCharType n	  -> CCharType ()	
	CShortType n	  -> CShortType ()	
	CIntType n	  -> CIntType ()
	CLongType n	  -> CLongType ()	
	CFloatType n	  -> CFloatType ()	
	CDoubleType n	  -> CDoubleType ()	
	CSignedType n	  -> CSignedType ()	
	CUnsigType n	  -> CUnsigType ()	
	CBoolType n	  -> CBoolType ()	
	CComplexType n	  -> CComplexType ()	
	CTypeDef ident n  -> CTypeDef ident ()
	CSUType cStructureUnion  n ->
		CSUType (translate cStructureUnion) ()
	CEnumType cEnumeration   n ->
		CEnumType (translate cEnumeration) ()
	CTypeOfExpr cExpression  n ->
		CTypeOfExpr (translate cExpression) ()
	CTypeOfType cDeclaration n ->
		CTypeOfType (translate cDeclaration) ()


-- | Convert the 'C Structure Union'
instance Convertible (CStructureUnion NodeInfo) (CStructureUnion ()) where
    translate cStruct = case cStruct of
	CStruct cStructTag mIdent mCDeclList cAttrList n ->
	    let scAttrList = translate cAttrList
		scCDeclList = translate mCDeclList 
	    in CStruct cStructTag mIdent scCDeclList scAttrList ()  

-- | Convert the 'C Enumeration'
instance Convertible (CEnumeration NodeInfo) (CEnumeration ()) where
    translate cEnum = case cEnum of
	CEnum mIdent mAuxPair lAttr n ->
	    let sclAttr = translate lAttr
                auxPair = translate mAuxPair 
 	    in CEnum mIdent auxPair sclAttr () 

-- | Convert the 'C Expression'
instance Convertible (CExpression NodeInfo) (CExpression ()) where
    translate cExpr = case cExpr of
	CComma lCExpr n -> CComma (translate lCExpr) () 
	CAssign cAssignOp lhsExpr rhsExpr n ->
	    let lhs = translate lhsExpr
		rhs = translate rhsExpr
	    in CAssign cAssignOp lhs rhs ()
	CCond condExpr mThenExpr elseExpr n ->
	    let cond = translate condExpr
		mThen = translate mThenExpr
		_else = translate elseExpr
	    in CCond cond mThen _else ()
	CBinary cBinaryOp lhsExpr rhsExpr n -> 
	    let lhs = translate lhsExpr
		rhs = translate rhsExpr
	    in CBinary cBinaryOp lhs rhs ()
	CCast cDecl cExpr n ->
	    let sCDecl = translate cDecl
		sCExpr = translate cExpr
	    in CCast sCDecl sCExpr ()	 
	CUnary cUnaryOp cExpr n ->
	    let sCExpr = translate cExpr
	    in CUnary cUnaryOp sCExpr ()	 
	CSizeofExpr cExpr n ->  
	    let sCExpr = translate cExpr
	    in CSizeofExpr sCExpr ()	 
	CSizeofType cDecl n ->  
	    let sCDecl = translate cDecl
	    in CSizeofType sCDecl ()	 
	CAlignofExpr cExpr n ->  
	    let sCExpr = translate cExpr
	    in CAlignofExpr sCExpr ()	 
	CAlignofType cDecl n ->  
	    let sCDecl = translate cDecl
	    in CAlignofType sCDecl ()	 
	CComplexReal cExpr n ->  
	    let sCExpr = translate cExpr
	    in CComplexReal sCExpr ()	 
	CComplexImag cExpr n ->  
	    let sCExpr = translate cExpr
	    in CComplexImag sCExpr ()	 
	CIndex lhsExpr rhsExpr n ->	 
	    let lhs = translate lhsExpr
		rhs = translate rhsExpr
	    in CIndex lhs rhs ()
	CCall fnExpr argsExpr n	-> 
	    let fn = translate fnExpr
		args = translate argsExpr
	    in CCall fn args ()
	CMember cExpr ident bool n ->
	    let expr = translate cExpr 
	    in CMember expr ident bool ()
	CVar ident n -> CVar ident () 
	CConst cConstant -> CConst $ translate cConstant
	CCompoundLit cDecl cInitList n ->
	    let decl = translate cDecl 
		initList = translate cInitList 
	    in CCompoundLit decl initList ()
	CStatExpr cStat n ->
	    let stat = translate cStat 
	    in CStatExpr stat ()
	CLabAddrExpr ident n -> CLabAddrExpr ident ()	
	CBuiltinExpr cBuiltinThing ->
	    let builtin = translate cBuiltinThing 
	    in CBuiltinExpr builtin 

-- | Convert the 'C Attribute'
instance Convertible (CAttribute NodeInfo) (CAttribute ()) where
    translate cAttr = case cAttr of
	CAttr ident lCExpr n -> CAttr ident (translate lCExpr) () 

-- | Convert the 'C Declarator'
instance Convertible (CDeclarator NodeInfo) (CDeclarator ()) where
    translate cDeclr = case cDeclr of 
	CDeclr mIdent lCDerDeclr mCStringLit lCAttr n ->
	     let slCDerDeclr = translate lCDerDeclr
	         smCStringLit = translate mCStringLit
		 slCAttr = translate lCAttr
	     in CDeclr mIdent slCDerDeclr smCStringLit slCAttr ()

-- | Convert the 'C DerivedDeclarator'
instance Convertible (CDerivedDeclarator NodeInfo) (CDerivedDeclarator ()) where
    translate cDerDeclr = case cDerDeclr of
	CPtrDeclr lCTypeQualifier n -> CPtrDeclr (translate lCTypeQualifier) ()	
	CArrDeclr lCTypeQualifier cArraySize n ->
	     let slCTypeQualifier = translate lCTypeQualifier
		 scArraySize = translate cArraySize
	     in CArrDeclr slCTypeQualifier scArraySize ()
	CFunDeclr e lCAttr n -> CFunDeclr (translate e) (translate lCAttr) () 

-- | Convert the 'C ArraySize'
instance Convertible (CArraySize NodeInfo) (CArraySize ()) where
    translate cArraySize = case cArraySize of
	CNoArrSize b     -> CNoArrSize b	
	CArrSize b cExpr -> CArrSize b $ translate cExpr 

-- | Convert the 'C Initializer'
instance Convertible (CInitializer NodeInfo) (CInitializer ()) where
    translate cInit = case cInit of
	CInitExpr cExpr n -> CInitExpr (translate cExpr) () 
	CInitList cInitList n -> CInitList (translate cInitList) () 

-- | Convert the 'C Part Designator'
instance Convertible (CPartDesignator NodeInfo) (CPartDesignator ()) where
    translate cPartDes = case cPartDes of
	CArrDesig cExpr n -> CArrDesig (translate cExpr) () 
	CMemberDesig ident n -> CMemberDesig ident ()
	CRangeDesig cExpr cExpr' n ->
	     CRangeDesig (translate cExpr) (translate cExpr') () 

-- | Convert the 'C Constant'
instance Convertible (CConstant NodeInfo) (CConstant ()) where
    translate cConst = case cConst of
	CIntConst cInteger n -> CIntConst cInteger () 
	CCharConst cChar n   -> CCharConst cChar ()	 
	CFloatConst cFloat n -> CFloatConst cFloat () 	 
	CStrConst cString n  -> CStrConst cString ()

-- | Convert the 'C Statement'
instance Convertible (CStatement NodeInfo) (CStatement ()) where
    translate cStat = case cStat of
	-- An (attributed) label followed by a statement
	CLabel ident cStat lCAttr n ->
	    let stat = translate cStat
		lcAttr = translate lCAttr
	    in CLabel ident stat lcAttr ()	
	-- A statement of the form case expr : stmt
	CCase cExpr cStat n -> 
	    let expr = translate cExpr
		stat = translate cStat
	    in CCase expr stat ()
	-- A case range of the form case lower ... upper : stmt
	CCases cExpr _cExpr cStat n ->
	    let expr = translate cExpr
		_expr = translate _cExpr
		stat = translate cStat
	    in CCases expr _expr stat ()	
	-- The default case default : stmt
	CDefault cStat n ->
	    let stat = translate cStat
	    in CDefault stat ()
	-- A simple statement, that is in C:
	-- evaluating an expression with side-effects
 	-- and discarding the result
	CExpr mCExpr n ->
	    let cExpr = translate mCExpr
	    in CExpr cExpr ()
	-- Compound statement CCompound localLabels blockItems at
	CCompound idents cCompoundBlockItem n ->
	    let compoundBlockItem = translate cCompoundBlockItem
	    in CCompound idents compoundBlockItem ()
	-- Conditional statement CIf ifExpr thenStmt maybeElseStmt at
	CIf cIfExpr cThenStmt maybecElseStmt n ->
	    let ifExpr = translate cIfExpr
		thenStmt = translate cThenStmt
		mElseStmt = translate maybecElseStmt 
	    in CIf ifExpr thenStmt mElseStmt ()
	-- Switch statement CSwitch selectorExpr switchStmt, 
	-- where switchStmt usually includes case, break and default statements
	CSwitch cExpr cStat n ->
	    let expr = translate cExpr
		stat = translate cStat
	    in CSwitch expr stat ()
	-- While or do-while statement CWhile guard stmt isDoWhile at
	CWhile cExpr cStat isDoWhile n ->
	    let guard = translate cExpr
		stmt = translate cStat
	    in CWhile guard stmt isDoWhile () 
	-- For statement CFor init expr-2 expr-3 stmt,
	-- where init is either a declaration or initializing expression
	CFor cInit cExpr _cExpr cStat n -> 
	    let init = translate cInit 
		expr = translate cExpr 
		_expr = translate _cExpr 
		stat = translate cStat 
	    in CFor init expr _expr stat () 
	-- Goto statement CGoto label
	CGoto ident n -> CGoto ident ()	
	-- Computed goto CGotoPtr labelExpr
	CGotoPtr cExpr n -> CGotoPtr (translate cExpr) ()
	-- Continue statement
	CCont n -> CCont ()
	-- Break statement
	CBreak n -> CBreak ()	
	-- Return statement CReturn returnExpr
	CReturn mCExpr n -> CReturn (translate mCExpr) ()
	-- Assembly statement	
	CAsm cAsmStmt n -> CAsm (translate cAsmStmt) () 

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

-- | Convert the 'C CompoundBlockItem'
instance Convertible (CCompoundBlockItem NodeInfo) (CCompoundBlockItem ()) where
    translate cCompound = case cCompound of
	-- A statement
	CBlockStmt cStat -> CBlockStmt (translate cStat)
	-- A local declaration
	CBlockDecl cDecl -> CBlockDecl (translate cDecl)
	-- A nested function (GNU C)
	CNestedFunDef cFunDef -> CNestedFunDef (translate cFunDef)

-- | Convert the 'C BuiltinThing'
instance Convertible (CBuiltinThing NodeInfo) (CBuiltinThing ()) where
    translate cBuiltinThing = case cBuiltinThing of
	CBuiltinVaArg cExpr cDecl n ->
	    let expr = translate cExpr
		decl = translate cDecl
	    in CBuiltinVaArg expr decl ()	
	CBuiltinOffsetOf cDecl lcPartDes n ->
	    let decl = translate cDecl
		lpartDes = translate lcPartDes
	    in CBuiltinOffsetOf decl lpartDes () 
	CBuiltinTypesCompatible cDecl _cDecl n ->
	    let decl = translate cDecl
		_decl = translate _cDecl
	    in CBuiltinTypesCompatible decl _decl () 

-- | Convert the 'C Function Definition'
instance Convertible (CFunctionDef NodeInfo) (CFunctionDef ()) where
    translate cFun = case cFun of
	CFunDef lCDeclSpec cDeclr lCDecl cStat n ->
	    let lDeclSpec = translate lCDeclSpec
		declr = translate cDeclr
		lDecl = translate lCDecl
		stat = translate cStat
	    in CFunDef lDeclSpec declr lDecl stat ()

-- | Convert the 'C String Literal'
instance Convertible (CStringLiteral NodeInfo) (CStringLiteral ()) where
    translate cStrLit = case cStrLit of
	CStrLit cStr n -> CStrLit cStr () 

-- | Convert the 'C Ident'
instance Convertible Ident Ident where
    translate = id 

-- | Convert the 'C Bool'
instance Convertible Bool Bool where
    translate = id 

-- | Convert Either versions
instance (Convertible a b, Convertible c d) => 
	Convertible (Either a c) (Either b d) where
    translate (Left a) = Left $ translate a 
    translate (Right b) = Right $ translate b 

-- | Convert Pair versions
instance (Convertible a b, Convertible c d) => 
	Convertible (a,c) (b,d) where
    translate (a,c) = (translate a, translate c)

-- | Convert Triple versions
instance (Convertible a b, Convertible c d, Convertible e f) => 
	Convertible (a,c,e) (b,d,f) where
    translate (a,c,e) = (translate a, translate c, translate e)

-}
