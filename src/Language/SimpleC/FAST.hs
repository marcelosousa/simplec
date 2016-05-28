{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Language.SimpleC.FAST where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Array
import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo)
import Language.C.Data.Ident
import Language.C.Syntax.Constants

-- Symbol Id
newtype SymId = SymId Int

instance Show SymId where
  show (SymId i) = "%"++show i
   
type BinaryOp = CBinaryOp
type UnaryOp = CUnaryOp
type AssignOp = CAssignOp
type StructTag = CStructTag
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
data Program ident a
  = Prog 
    {
      decls   :: Declarations ident a
    , defs    :: Definitions ident a
    , asm_ext :: AsmExt a
    } deriving Show

data ProgramData
  = ProgData 
    {
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
type Definitions  ident a = [FunctionDef ident a]

data FunctionDef ident a
  = FunDef 
  { ret_ty :: Type ident a          -- return type?
  , symbol :: Declarator ident a    -- identifer?
  , params :: [Declaration ident a] -- parameters?
  , body   :: Statement ident a     -- body?
  , loc    :: a
  }
  deriving Show

data Declaration ident a 
  = Decl (Type ident a) (DeclElem ident a)
  | TypeDecl (Type ident a)

-- | Just a type synonym
type CDeclElem a = (Maybe (CDeclarator a), Maybe (CInitializer a), Maybe (CExpression a))
data DeclElem ident a
  = DeclElem 
  { declarator :: Declarator ident a
  , initializer :: Maybe (Initializer ident a)
  }

-- | C Declarator
data Declarator ident a
  = Declr
  { declr_ident :: Maybe ident
  , declr_type  :: [DerivedDeclarator ident a]
  , declr_cstr  :: Maybe (CStringLiteral a)
  , declr_attr  :: [Attribute ident a]
  , declr_loc   :: a
  }

data DerivedDeclarator ident a
  = PtrDeclr [TypeQualifier ident a]
  | ArrDeclr [TypeQualifier ident a] (ArraySize ident a)
  | FunDeclr (Either [ident] ([Declaration ident a], Bool)) [Attribute ident a]

data ArraySize ident a
  = NoArrSize Bool
  | ArrSize Bool (Expression ident a)
  deriving Show
 
data Initializer ident a
  = InitExpr (Expression ident a) 
  | InitList (InitializerList ident a)
  deriving Show

type CInitializerListEl a = ([CPartDesignator a],CInitializer a)
type InitializerListEl ident a = ([PartDesignator ident a],Initializer ident a)
type InitializerList ident a 
  = [([PartDesignator ident a],Initializer ident a)]

data PartDesignator ident a
  = ArrDesig (Expression ident a)
  | MemberDesig ident
  | RangeDesig (Expression ident a) (Expression ident a)
  deriving Show

-- | Equivalent to a DeclarationSpecifier
data Type ident a
  = Type 
  { storspec :: StorageSpecifier
  , tyquals  :: [TypeQualifier ident a]
  , tyspecs  :: [TypeSpecifier ident a]
  } 

-- ^ storage-class specifier or typedef
data StorageSpecifier 
  = Auto     -- ^ auto (default)
  | Register -- ^ register
  | Static   -- ^ static
  | Extern   -- ^ extern
  | Typedef  -- ^ typedef
  | Thread   -- ^ GNUC thread local storage

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
  | SUType      (StructureUnion ident a) -- ^ Struct or Union specifier
  | EnumType    (Enumeration ident a) a  -- ^ Enumeration specifier
  | TypeDef     ident        a           -- ^ Typedef name
  | TypeOfExpr  (Expression ident a)  a  -- ^ @typeof(expr)@
  | TypeOfType  (Declaration ident a) a  -- ^ @typeof(type)@
  
data StructureUnion ident a 
  = Struct StructTag ident (Maybe [Declaration ident a]) [Attribute ident a] a
  deriving Show

data Enumeration ident a
  = Enum ident
         (Maybe [(ident, Maybe (Expression ident a))])
         [Attribute ident a]
         a
  deriving Show
 
-- ^ type qualifier
data TypeQualifier ident a 
  = ConstQual 
  | VolatQual 
  | RestrQual 
  | InlineQual
  | AttrQual  (Attribute ident a)

-- ^ TODO: Comment
data Attribute ident a 
  =  Attr ident [Expression ident a] 
 deriving Show 

-- ^ Expression
data Expression ident a
  = AlignofExpr (Expression ident a)
  | AlignofType (Declaration ident a)
  | Assign AssignOp (Expression ident a) (Expression ident a)
  | Binary BinaryOp (Expression ident a) (Expression ident a)
  | Call (Expression ident a) [Expression ident a] a
  | Cast (Declaration ident a) (Expression ident a)
  | Cond (Expression ident a) (Maybe (Expression ident a)) (Expression ident a)
  | Const Constant
  | Index (Expression ident a) (Expression ident a)
  | LabAddrExpr ident 
  | Member (Expression ident a) ident Bool
  | SizeofExpr (Expression ident a)
  | SizeofType (Declaration ident a)
  | StatExpr (Statement ident a) 
  | Unary UnaryOp (Expression ident a)
  | Var ident
-- Unsupported expressions:
  | BuiltinExpr (CBuiltinThing a)
  | ComplexReal (CExpression a)
  | ComplexImag (CExpression a)
  | CompoundLit (CDeclaration a) (CInitializerList a)
  | Comma [CExpression a]
  deriving Show

data Constant
  = IntConst CInteger 
  | CharConst CChar 
  | FloatConst CFloat 
  | StrConst CString 
  deriving Show

data Statement ident a
  = Break a
  | Case (Expression ident a) (Statement ident a) a
  | Cases (Expression ident a) (Expression ident a) (Statement ident a) a
  | Compound [ident] [CompoundBlockItem ident a] a
  | Cont a
  | Default (Statement ident a) a
  | Expr (Expression ident a) a -- * Removed the Maybe
  | For (Either (Maybe (Expression ident a)) (Declaration ident a))
        (Maybe (Expression ident a))
        (Maybe (Expression ident a))
        (Statement ident a)
        a
  | Goto ident a
  | GotoPtr (Expression ident a) a
  | If (Expression ident a) (Statement ident a) (Maybe (Statement ident a)) a
  | Label ident (Statement ident a) [Attribute ident a] a
  | Return (Maybe (Expression ident a)) a
  |""++show i Switch (Expression ident a) (Statement ident a) a
  | While (Expression ident a) (Statement ident a) Bool a
  -- | Not supported
  | Asm (CAssemblyStatement a) a
  deriving Show

data CompoundBlockItem ident a
  = BlockStmt (Statement ident a)
  | BlockDecl (Declaration ident a)
  | NestedFunDef (FunctionDef ident a)
  deriving Show

-- Show instances
pp_with_sep :: Show a => String -> [a] -> String
pp_with_sep sep [] = ""
pp_with_sep sep (x:[]) = show x
pp_with_sep sep (x:xs) =
  show x++sep++pp_with_sep sep xs 

-- @TODO: Print all the fields
instance (Show ident, Show a) => Show (Declarator ident a) where
  show d@Declr{..} =
    let ty = foldr shows "" declr_type 
    in case declr_ident of
      Nothing -> ty
      Just i  -> show i++" "++ty

-- @TODO: FunDeclr print attributes. 
instance (Show ident, Show a) => Show (DerivedDeclarator ident a) where
  show dd = case dd of
    PtrDeclr tyquals -> "*" ++ foldr shows "" tyquals
    ArrDeclr tyquals arr -> foldr shows "" tyquals++"["++show arr++"]"
    FunDeclr eIdsDecl _ ->
      case eIdsDecl of
        Left lidents -> "("++pp_with_sep "," lidents++")"
        Right (ldecl,_) -> "("++pp_with_sep "," ldecl++")"

instance (Show ident, Show a) => Show (Declaration ident a) where
  show decl = case decl of
    Decl ty declr -> show ty ++ " " ++ show declr  
    TypeDecl ty -> show ty 

instance (Show ident, Show a) => Show (DeclElem ident a) where
  show d@DeclElem{..} =
    let pp_declr = show declarator
    in case initializer of
         Nothing -> pp_declr
         Just i  -> pp_declr++" "++show initializer

instance (Show ident, Show a) => Show (Type ident a) where
  show ty@Type{..} =
    let pp_storspec = show storspec 
        pp_tyquals = foldr shows "" tyquals
        pp_tyspec = foldr shows "" tyspecs
    in pp_storspec ++ pp_tyquals ++ " " ++ pp_tyspec 

instance Show StorageSpecifier where
  show spec = case spec of
    Auto     -> "auto"
    Register -> "register"
    Static   -> "static"
    Extern   -> "extern"
    Typedef  -> "typedef"
    Thread   -> "thread"

instance (Show ident, Show a) => Show (TypeQualifier ident a) where
  show tyqual = case tyqual of
    ConstQual -> "const" 
    VolatQual -> "volatile"
    RestrQual -> "restrict"
    InlineQual -> "inline"
    AttrQual attr -> "__"++show attr++"__"

instance (Show ident, Show a) => Show (TypeSpecifier ident a) where
  show ty = case ty of
    VoidType    -> "void" 
    CharType    -> "char"
    ShortType   -> "short"
    IntType     -> "int"
    LongType    -> "long"
    FloatType   -> "float"
    DoubleType  -> "double"
    SignedType  -> "signed"
    UnsigType   -> "unsigned"
    BoolType    -> "bool"
    ComplexType -> "complex"
    Int128Type  -> "int128"
    TypeDef i a -> "typedef " ++ show i 
    SUType su   -> show su
    EnumType en _ -> show en
    TypeOfExpr e _ -> "typeof("++show e++")"
    TypeOfType d _ -> "typeofType("++show d++")"
