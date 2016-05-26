{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.SimpleC.FAST where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Array
import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo)
import Language.C.Data.Ident

-- Symbol Id
type SymId = Int
   
type OpCode  = CBinaryOp
type UOpCode = CUnaryOp
type AssignOp = CAssignOp
type At = NodeInfo 


{-
Given the AST of the C file:
- It is easy to remove the NodeInfo
  by using: fmap (\_ -> ())
- In an ident the name is fully qualified
-  I would like to apply some godel numbering 
-  for the symbol table.
-}
-- | C Program 
-- A C Program contains declarations
-- which can be type declarations,
-- initialization of global variables
-- and definition of functions.
data Program ident a = Prog {
    decls   :: Declarations ident a
  , defs    :: Definitions a
  , asm_ext :: AsmExt a
  } deriving Show

data ProgramData = ProgData {
    code :: Program Ident ()
  , symbols :: !Symbols
  , cfgs :: !CFG
  }

data Symbols = SymInfo
data CFG = CFG

-- | AsmExt : Not sure the use of this attr
type AsmExt a = [CStringLiteral a] 
-- Declarations can be initialized or not
type Declarations ident a = [Declaration ident a]
type Definitions a = [CFunctionDef a]

data Declaration ident a = 
    Decl (Type ident a) (DeclElem ident a)
  | TypeDecl (Type ident a)
  deriving Show

type CDeclElem a = (Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a))
data DeclElem ident a =
  DeclElem (Declarator ident a) (Maybe (Initializer ident a))
  deriving Show

-- | C Declarator
data Declarator ident a =
  Declr ident [DerivedDeclarator ident a] (Maybe (CStringLiteral a)) [Attribute ident a] a
  deriving Show

data DerivedDeclarator ident a
  = PtrDeclr [TypeQualifier ident a]
  | ArrDeclr [TypeQualifier ident a] (ArraySize ident a)
  | FunDeclr (Either [ident] ([Declaration ident a], Bool)) [Attribute ident a]
  deriving Show

data ArraySize ident a = NoArrSize Bool | ArrSize Bool (CExpression a)
  deriving Show
 
data Initializer ident a
  = InitExpr (CExpression a) | InitList (CInitializerList a)
  deriving Show

data Type ident a
  = Type StorageSpecifier [TypeQualifier ident a] [TypeSpecifier ident a] 
  deriving Show

-- ^ storage-class specifier or typedef
data StorageSpecifier 
  = Auto     -- ^ auto (default)
  | Register -- ^ register
  | Static   -- ^ static
  | Extern   -- ^ extern
  | Typedef  -- ^ typedef
  | Thread   -- ^ GNUC thread local storage
  deriving Show

-- ^ type name
data TypeSpecifier ident a
  = VoidType    
  | CharType    
  | ShortType   
  | IntType     
  | LongType    
  | FloatType   
  | DoubleType  
  | SignedType  
  | UnsigType   
  | BoolType    
  | ComplexType 
  | Int128Type  
  | SUType      (StructureUnion ident a)      -- ^ Struct or Union specifier
  | EnumType    (CEnumeration a)    a      -- ^ Enumeration specifier
  | TypeDef     ident        a      -- ^ Typedef name
  | TypeOfExpr  (CExpression a)  a  -- ^ @typeof(expr)@
  | TypeOfType  (CDeclaration a) a  -- ^ @typeof(type)@
  deriving Show
  
data StructureUnion ident a 
  = Struct CStructTag ident (Maybe [Declaration ident a]) [Attribute ident a] a
  deriving Show
  
-- ^ type qualifier
data TypeQualifier ident a 
  = ConstQual 
  | VolatQual 
  | RestrQual 
  | InlineQual
  | AttrQual  (Attribute ident a)
 deriving Show

data Attribute ident a 
  =  Attr ident [CExpression a] 
 deriving Show 
