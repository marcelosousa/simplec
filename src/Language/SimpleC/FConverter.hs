{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}

{-
  This is a full converter from the C language 
-}
module Language.SimpleC.FConverter where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident
import qualified Language.SimpleC.AST as SC
import qualified Data.Map as M
import Data.Map (Map)

class Convertible a b | a -> b where
  translate :: a -> b

-- | Convert the 'C Translation Unit'
instance Convertible CTranslUnit SC.Program where
  translate (CTranslUnit cdecls n) = 
    let (decls,defs,asms) = tr cdecls
        flat_decls = flatten_decl decls
    in SC.Prog flat_decls defs asms
   where 
    tr [] = ([],[],[])
    tr (cdecl:cs) =
      let (decls,defs,asms) = tr cs
      in case cdecl of
           CDeclExt decl -> (decl:decls,defs,asms)
           CFDefExt fn   -> (decls,fn:defs,asms)  
           CAsmExt asm _ -> (decls,defs,asm:asms) 

flatten_decl :: [CDeclaration NodeInfo] -> SC.Declarations
flatten_decl [] = []
flatten_decl (d:ds) =
  let decls = flatten_decl ds
      decl = case d of
        CDecl spec ids at -> map (\i -> SC.Decl spec i at) ids
  in decl ++ decls
 
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

-- | Convert the 'C Storage Specifier' 
instance Convertible (CStorageSpecifier NodeInfo) (CStorageSpecifier ()) where
    translate cstorspec = case cstorspec of
	CAuto n     -> CAuto ()	
	CRegister n -> CRegister ()	
	CStatic n   -> CStatic ()
	CExtern n   -> CExtern ()
	CTypedef n  -> CTypedef ()
	CThread n   -> CThread ()

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

-- | Convert the 'C Type Qualifier'
instance Convertible (CTypeQualifier NodeInfo) (CTypeQualifier ()) where
    translate cTypeQualifier = case cTypeQualifier of
	CConstQual n  -> CConstQual () 
	CVolatQual n  -> CVolatQual ()
	CRestrQual n  -> CRestrQual ()
	CInlineQual n -> CInlineQual ()
	CAttrQual cAttribute -> CAttrQual $ translate cAttribute

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

-- | Convert Maybe versions
instance (Convertible a b) => Convertible (Maybe a) (Maybe b) where
    translate Nothing = Nothing
    translate (Just expr) = Just $ translate expr 

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

-- | Convert List versions
instance (Convertible a b) => Convertible [a] [b] where
    translate a = map translate a
-}
{-
-- | Convert the 'C ?'
instance Convertible (C? NodeInfo) (C? ()) where
    translate = undefined
-}

