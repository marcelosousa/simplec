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
data Program a = Prog {
    decls   :: Declarations a
  , defs    :: Definitions a
  , asm_ext :: AsmExt a
  } deriving Show

data ProgramData = ProgData {
    code :: Program ()
  , symbols :: !Symbols
  , cfgs :: !CFG
  }

data Symbols = SymInfo
data CFG = CFG

-- | AsmExt : Not sure the use of this attr
type AsmExt a = [CStringLiteral a] 
-- Declarations can be initialized or not
type Declarations a = [Declaration a]
type Definitions a = [CFunctionDef a]

type DeclElem a = (Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a))
data Declaration a = 
    Decl (DeclarationSpecifier a) (DeclElem a)
  | TypeDecl (DeclarationSpecifier a)
  deriving Show

data DeclarationSpecifier a
  = DeclSpec StorageSpecifier [CTypeQualifier a] [TypeSpecifier a] 
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
  = Struct CStructTag (Maybe ident) (Maybe [Declaration a]) [CAttribute a] a
  deriving Show
  
-- ^ type qualifier
data TypeQualifier a 
  = ConstQual 
  | VolatQual 
  | RestrQual 
  | InlineQual
  | CAttrQual  (CAttribute a)
