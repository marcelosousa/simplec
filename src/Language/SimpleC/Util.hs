{-# LANGUAGE RecordWildCards #-}
module Language.SimpleC.Util where

import Data.Map (Map)
import Language.SimpleC.AST 
import Language.SimpleC.Converter
import Language.SimpleC.Flow
import Language.C.Syntax.Constants

-- The result of the front-end
--  AST of the file
--  Control Flow Graphs per Function
--  Symbol Table
data FrontEnd node st 
 = FrontEnd 
 { 
   ast  :: Program SymId node
 , cfgs :: Graphs SymId node st
 , symt :: Map SymId Symbol
 } deriving Show

data MemCell ident node val
 = MCell { 
   ty  :: Ty ident node
 , val :: val
 } deriving (Eq,Show)

data Value
 = VInt Int
 | VShort Int
 | VLong Int
 | VDouble Double
 | VFloat Float
 | VBool Bool
 | VChar Char
 | VString String
 deriving (Show,Eq,Ord)

-- Full Type
data Ty ident node 
 = Ty [DerivedDeclarator ident node] (Type ident node)
  deriving (Show,Eq)

init_value :: Ty ident node -> Value
init_value (Ty d base@Type{..}) =
  case d of
    [] -> init_val tyspecs
    _ -> error "init_value: not supported" 

init_val :: [TypeSpecifier ident node] -> Value
init_val [] = error "init_val: no type"
init_val (t:ts) = case t of
  VoidType   -> error "init_val: no value for void type"
  CharType   -> VChar '0'
  ShortType  -> VShort 0
  IntType    -> VInt 0
  LongType   -> VLong 0
  FloatType  -> VFloat 0
  DoubleType -> VDouble 0
  SignedType -> init_val ts
  UnsigType  -> init_val ts
  BoolType   -> VBool False
  ComplexType -> error "init_val: complex type"
  Int128Type -> error "init_val: int128 type"
  SUType struct -> error "init_val: struct type" 
  EnumType enum node -> error "init_val: enum type"
  TypeDef ident node -> error "init_val: type def"
  TypeOfExpr exp node -> error "init_val: type of expr"
  TypeOfType dec node -> error "init_val: type of type"

toValue :: Constant -> Value
toValue const = case const of
  -- Assume that we always have CIntegers in Decimal represetation
  IntConst (CInteger i _ _) -> VInt $ fromEnum i 
  CharConst cchar -> case cchar of
    CChar c _ -> VChar c
    _ -> error "toValue: CChars not supported"
  FloatConst cfloat -> error "toValue: Float not supported" 
  StrConst (CString str _) -> VString str 
  BoolConst b -> VBool b
