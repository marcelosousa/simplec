{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Language.SimpleC.AST where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Array
import Language.C.Syntax.AST
import Language.C.Pretty
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
      prog :: Program Ident ()
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
  { declarator :: Maybe (Declarator ident a)
  , initializer :: Maybe (Initializer ident a)
  , size_expr :: Maybe (Expression ident a)
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
  = Struct StructTag (Maybe ident) (Maybe [Declaration ident a]) [Attribute ident a] a
  deriving Show

data Enumeration ident a
  = Enum (Maybe ident)
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
  | BuiltinExpr (BuiltinThing ident a)
  | Call (Expression ident a) [Expression ident a] a
  | Cast (Declaration ident a) (Expression ident a)
  | Comma [Expression ident a]
  | CompoundLit (Declaration ident a) (InitializerList ident a)
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
  | ComplexReal (CExpression a)
  | ComplexImag (CExpression a)

data Constant
  = IntConst CInteger 
  | CharConst CChar 
  | FloatConst CFloat 
  | StrConst CString 

data Statement ident a
  = Break a
  | Case (Expression ident a) (Statement ident a) a
  | Cases (Expression ident a) (Expression ident a) (Statement ident a) a
  | Compound [ident] [CompoundBlockItem ident a] a
  | Cont a
  | Default (Statement ident a) a
  | Expr (Maybe (Expression ident a)) a 
  | For (Either (Maybe (Expression ident a)) [Declaration ident a])
        (Maybe (Expression ident a))
        (Maybe (Expression ident a))
        (Statement ident a)
        a
  | Goto ident a
  | GotoPtr (Expression ident a) a
  | If (Expression ident a) (Statement ident a) (Maybe (Statement ident a)) a
  | Label ident (Statement ident a) [Attribute ident a] a
  | Return (Maybe (Expression ident a)) a
  | Switch (Expression ident a) (Statement ident a) a
  | While (Expression ident a) (Statement ident a) Bool a
  -- | Not supported
  | Asm (CAssemblyStatement a) a

data CompoundBlockItem ident a
  = BlockStmt (Statement ident a)
  | BlockDecl [Declaration ident a]
  | NestedFunDef (FunctionDef ident a)


data BuiltinThing ident a
  = BuiltinVaArg (Expression ident a) (Declaration ident a) a
  | BuiltinOffsetOf (Declaration ident a) [PartDesignator ident a] a
  | BuiltinTypesCompatible (Declaration ident a) (Declaration ident a) a
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
         Nothing -> pp_declr++" "++show size_expr
         Just i  -> pp_declr++" "++show initializer++" "++ show size_expr

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

instance (Show ident, Show a) => Show (Statement ident a) where
  show stmt = case stmt of
    Break a -> "break"
    Case cond stat a ->
      let condS = show cond
          statS = show stat
      in "case "++condS++" :\n\t"++statS
    Cases lower upper stat a ->
      let lowerS = show lower
          upperS = show upper 
          statS = show stat
      in "case "++lowerS++" ... "++upperS++": \n\t"++statS
    Compound labels stmts a ->
      let labelsS = show labels
          stmtsS = 
            if length stmts == 1
            then show (stmts!!0) ++ ";" 
            else foldr (\stmt s -> show stmt++";\n"++s) "" stmts 
      in if null labels 
         then stmtsS
         else "compound "++labelsS++"\n"++stmtsS
    Cont a -> "continue"
    Default stmt a -> "default : "++show stmt
    Expr expr a -> show expr 
    For init cond final body a ->
      let initS = case init of
                    Left mExpr -> maybe "" show mExpr
                    Right decls -> show decls
          condS = maybe "" show cond
          finalS = maybe "" show final
          bodyS = show body 
      in "for ("++initS++"; "++condS++"; "++finalS++")\n{\n"++bodyS++"}" 
    Goto ident a -> "goto "++show ident
    GotoPtr expr a -> "goto "++show expr
    If cond _then _else a -> 
      let condS = show cond
          thenS = show _then
          elseS = maybe "" (\e -> "else {\n"++show e++"\n}") _else
      in "if ("++condS++"){\n"++thenS++"\n} "++elseS
    Label ident stat attrs a -> 
      let identS = show ident
          statS = show stat
          attrsS = show attrs
      in "label "++identS++":\n{\n"++statS++"\n"++attrsS++"\n}"
    Return mExpr a -> "return "++maybe "" show mExpr
    Switch expr stat a -> 
      let exprS = show expr
          statS = show stat
      in "switch ("++exprS++") {\n"++statS++"\n}\n"
    While cond stat doWhile a -> 
      let condS = show cond
          statS = show stat
      in if doWhile 
         then "do {\n"++statS++"\n} while("++condS++")"
         else "while("++condS++") {\n"++statS++"\n}" 

instance (Show ident, Show a) => Show (CompoundBlockItem ident a) where
  show block = case block of
    BlockStmt stmt -> show stmt
    BlockDecl ldecl -> 
      case ldecl of
        [] -> ""
        [decl] -> show decl++";"
        _ -> foldr (\d s -> show d++";\n"++s) (show (last ldecl)) (init ldecl)
    NestedFunDef fun -> show fun -- TODO

instance Show Constant where
  show const = case const of 
    IntConst cInt -> show cInt 
    CharConst cChar -> show cChar
    FloatConst cFloat -> show cFloat
    StrConst cStr -> show cStr

instance (Show ident, Show a) => Show (Expression ident a) where
  show expr = case expr of
    AlignofExpr expr -> "alignofexpr("++show expr++")"
    AlignofType decl -> "alignofty("++show decl++")"
    Assign assignOp lhs rhs -> show lhs++" "++show (pretty assignOp)++" "++show rhs 
    Binary binaryOp lhs rhs -> show lhs++" "++show (pretty binaryOp)++" "++show rhs
    BuiltinExpr c -> "builtin "++show c
    Call fnName args a -> 
      let argsS = case args of
            [] -> ""
            [arg] -> show arg
            _ -> foldr (\a s -> show a++", "++s) (show (last args)) (init args) 
      in show fnName++"("++argsS++")"
    Cast decl expr -> "("++show decl++") "++show expr 
    Comma exprs ->
      if length exprs == 1
      then "("++show (exprs!!0)++")"
      else foldr (\s r -> show s++","++r) (show $ last exprs) (init exprs)
    CompoundLit cdecl cinit -> "compound_lit "++show cdecl++" "++show cinit
    Cond e1 e2 e3 -> 
      let e1S = show e1
          e2S = maybe "" show e2
          e3S = show e3
      in e1S++" ? "++e2S++" : "++e3S
    Const constant -> show constant 
    Index e1 e2 -> show e1++"["++show e2++"]"
    LabAddrExpr ident -> "label "++show ident 
    Member expr ident b -> show expr++"."++show ident
    SizeofExpr expr -> "sizeofexpr("++show expr++")"
    SizeofType decl -> "sizeofty("++show decl++")"
    StatExpr stat -> show stat 
    Unary unaryOp expr -> show (pretty unaryOp)++show expr
    Var ident -> show ident
