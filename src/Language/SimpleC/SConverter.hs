{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}

module Language.SimpleC.SConverter where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident
import qualified Language.SimpleC.AST as SC
import qualified Data.Map as M
import Data.Map (Map)

class Convertible a b | a -> b where
    translate :: a -> b

type SCTranslUnit = CTranslationUnit ()

-- | Convert the 'C Translation Unit'
instance Convertible CTranslUnit SCTranslUnit where
    translate (CTranslUnit decls n) =
	CTranslUnit (translate decls) () 

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
	CBinary cBinaryOp lhsExpr rhsExpr n 
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
	    in CAlignOfType sCDecl ()	 
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
	CCall fnExpr argsExpr n	 
	    let fn = translate fnExpr
		rhs = translate argsExpr
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
	    in CBuiltinExpr builtin ()

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

-- | Convert the 'C Function Definition'
instance Convertible (CFunctionDef NodeInfo) (CFunctionDef ()) where
    translate = undefined

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

{-
-- | Convert the 'C ?'
instance Convertible (C? NodeInfo) (C? ()) where
    translate = undefined
-}

