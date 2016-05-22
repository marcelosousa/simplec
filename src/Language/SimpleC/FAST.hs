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


-- | C Program 
-- A C Program contains declarations
-- which can be type declarations,
-- initialization of global variables
-- and definition of functions. 
data Program = Prog {
    decls   :: !Declarations 
  , defs    :: !Definitions
  , asm_ext :: !AsmExt 
  } 

data ProgramData = ProgData {
    code :: !Program
  , symbols :: !Symbols
  , cfgs :: !CFG
  }

data Symbols = SymInfo
data CFG = CFG

-- | AsmExt : Not sure the use of this attr
type AsmExt = [CStringLiteral At] 
-- Declarations can be initialized or not
type Declarations = [Declaration]
type Definitions = [CFunctionDef At]

type DeclElem = (Maybe (CDeclarator At), Maybe (CInitializer At), Maybe (CExpression At))
data Declaration = Decl (DeclarationSpecifier At) DeclElem At	
  deriving Show

data DeclarationSpecifier a
  = DeclSpec StorageSpecifier [CTypeQualifier a] [CTypeSpecifier a] 
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
data TypeSpecifier a
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
  | SUType      (CStructureUnion a) a      -- ^ Struct or Union specifier
  | EnumType    (CEnumeration a)    a      -- ^ Enumeration specifier
  | TypeDef     Ident        a      -- ^ Typedef name
  | TypeOfExpr  (CExpression a)  a  -- ^ @typeof(expr)@
  | TypeOfType  (CDeclaration a) a  -- ^ @typeof(type)@

-- ^ type qualifier
data TypeQualifier a 
  = ConstQual 
  | VolatQual 
  | RestrQual 
  | InlineQual
  | CAttrQual  (CAttribute a)
