{-# LANGUAGE RecordWildCards #-}
module Language.SimpleC.Util where

import Data.Map (Map)
import Language.SimpleC.AST 
import Language.SimpleC.Converter
import Language.SimpleC.Flow
import Language.C.Syntax.Constants
import qualified Data.Map as M

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

sep = "-----------------------------------\n"
show_symt :: Map SymId Symbol -> String 
show_symt tab =
  let header = "Symbol Table\n" ++ sep ++ "SymId  | Symbol\n" ++ sep
      tab_s =  M.foldrWithKey (\k s r -> 
        show k ++ " | " ++ show s ++ "\n" ++ r) "" tab
  in header ++ tab_s

show_symt_dot :: Map SymId Symbol -> String 
show_symt_dot tab =
  let header = "symt [label=\"{"
      ((k,s):els) = M.toList tab
      el_s = "{" ++ show k ++ " | " ++ show s ++ "}" 
      tab_s =  foldr (\(k,s) r -> 
        "{" ++ show k ++ " | " ++ show s ++ "} | " ++ r) el_s els 
  in header ++ tab_s ++ "}\"];\n"

data MemCell ident node val
 = MCell { 
   ty  :: Ty ident node
 , val :: val
 } deriving (Eq,Ord)

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

type SymbolTable = Map SymId Symbol

-- Full Type
type STy = Ty SymId ()
data Ty ident node 
 = Ty [DerivedDeclarator ident node] (Type ident node)
  deriving (Show,Eq,Ord)

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

const_int :: Int -> Constant
const_int i = IntConst (CInteger (toInteger i) DecRepr (Flags 0))

const_one :: Constant
const_one = const_int 1 

const_char :: Char -> Constant
const_char c = CharConst (CChar c True)

const_float :: Float -> Constant
const_float f = FloatConst (CFloat $ show f) 

const_string :: String -> Constant
const_string s = StrConst (CString s True)

const_bool :: Bool -> Constant
const_bool b = BoolConst b

-- Operations over Value
arith_int_op :: (Int -> Int -> Int) -> Value -> Value -> Value
arith_int_op op v1 v2 = case (v1,v2) of 
  (VInt i,VInt j) -> VInt $ i `op` j
  (VShort i,VShort j) -> VShort $ i `op` j
  (VLong i,VLong j) -> VLong $ i `op` j
  _ -> error "arith_op: type mismatch"

arith_double_op op v1 v2 = case (v1,v2) of 
  (VDouble i,VDouble j) -> VDouble $ i `op` j
  _ -> error "arith_double_op: type mismatch"

arith_float_op op v1 v2 = case (v1,v2) of 
  (VFloat i,VFloat j) -> VFloat $ i `op` j
  _ -> error "arith_float_op: type mismatch"

add_value :: Value -> Value -> Value
add_value v1 v2 = case v1 of
  VInt _    -> arith_int_op (+) v1 v2 
  VShort _  -> arith_int_op (+) v1 v2 
  VLong _   -> arith_int_op (+) v1 v2 
  VDouble _ -> arith_double_op (+) v1 v2 
  VFloat _  -> arith_float_op (+) v1 v2
  _ -> error "add_value: type mismatch" 

sub_value :: Value -> Value -> Value
sub_value v1 v2 = case v1 of
  VInt _    -> arith_int_op (-) v1 v2 
  VShort _  -> arith_int_op (-) v1 v2 
  VLong _   -> arith_int_op (-) v1 v2 
  VDouble _ -> arith_double_op (-) v1 v2 
  VFloat _  -> arith_float_op (-) v1 v2
  _ -> error "sub_value: type mismatch" 

minus_value :: Value -> Value
minus_value v1 = case v1 of
  VInt v    -> VInt (-v)    
  VShort v  -> VShort (-v)  
  VLong v   -> VLong (-v)   
  VDouble v -> VDouble (-v) 
  VFloat v  -> VFloat (-v)  
  _ -> error "minus_value: type mismatch" 
  
mult_value :: Value -> Value -> Value
mult_value v1 v2 = case v1 of
  VInt _    -> arith_int_op (*) v1 v2 
  VShort _  -> arith_int_op (*) v1 v2 
  VLong _   -> arith_int_op (*) v1 v2 
  VDouble _ -> arith_double_op (*) v1 v2 
  VFloat _  -> arith_float_op (*) v1 v2
  _ -> error "mult_value: type mismatch" 

div_value :: Value -> Value -> Value
div_value v1 v2 = case v1 of
  VInt _    -> arith_int_op div v1 v2 
  VShort _  -> arith_int_op div v1 v2 
  VLong _   -> arith_int_op div v1 v2 
  _ -> error "div_value: type mismatch" 

rmd_value :: Value -> Value -> Value
rmd_value v1 v2 = case v1 of
  VInt _    -> arith_int_op mod v1 v2 
  VShort _  -> arith_int_op mod v1 v2 
  VLong _   -> arith_int_op mod v1 v2 
  _ -> error "rmd_value: type mismatch" 

-- Boolean Operations over Value
bool_int_op :: (Int -> Int -> Bool) -> Value -> Value -> Value
bool_int_op op v1 v2 = case (v1,v2) of 
  (VInt i,VInt j)     -> VBool $ i `op` j
  (VShort i,VShort j) -> VBool $ i `op` j
  (VLong i,VLong j)   -> VBool $ i `op` j
  _ -> error "bool_op: type mismatch"

bool_double_op op v1 v2 = case (v1,v2) of 
  (VDouble i,VDouble j) -> VBool $ i `op` j
  _ -> error "bool_double_op: type mismatch"

bool_float_op op v1 v2 = case (v1,v2) of 
  (VFloat i,VFloat j) -> VBool $ i `op` j
  _ -> error "bool_float_op: type mismatch"

-- Boolean operations
neg_value :: Value -> Value
neg_value v1 = case v1 of
  VInt 0 -> VBool True 
  VInt _ -> VBool False 
  VBool b -> VBool $ not b
  _ -> error "neg_value: type mismatch" 

le_value :: Value -> Value -> Value
le_value v1 v2 = case v1 of
  VInt   _  -> bool_int_op (<) v1 v2
  VShort _  -> bool_int_op (<) v1 v2
  VLong _   -> bool_int_op (<) v1 v2 
  VDouble _ -> bool_double_op (<) v1 v2 
  VFloat _  -> bool_float_op (<) v1 v2
  _ -> error "le_value: type mismatch" 

gr_value :: Value -> Value -> Value
gr_value v1 v2 = case v1 of
  VInt   _  -> bool_int_op (>) v1 v2
  VShort _  -> bool_int_op (>) v1 v2
  VLong _   -> bool_int_op (>) v1 v2 
  VDouble _ -> bool_double_op (>) v1 v2 
  VFloat _  -> bool_float_op (>) v1 v2
  _ -> error "gr_value: type mismatch" 

eq_value :: Value -> Value -> Value
eq_value v1 v2 = VBool $ v1 == v2

land_value :: Value -> Value -> Value
land_value v1 v2 = case (v1,v2) of
  (VBool b1,VBool b2) -> VBool $ b1 && b2
  _ -> error "land_value: type mismatch" 

lor_value :: Value -> Value -> Value
lor_value v1 v2 = case (v1,v2) of
  (VBool b1,VBool b2) -> VBool $ b1 || b2
  _ -> error "lor_value: type mismatch"  
 
get_expr_id :: Expression ident a -> ident
get_expr_id e = case e of
  Var i -> i
  Unary _ expr -> get_expr_id expr
  _ -> error "get_expr_id: not supported" 

assign_param :: Declaration ident a -> Expression ident a -> Declaration ident a
assign_param decl expr = case decl of 
  Decl ty e@DeclElem{..} ->  
    let initial = case initializer of
          Nothing -> Just $ InitExpr expr 
          Just i  -> error "assign_param: Initializer is not Nothing" 
        ne = e { initializer = initial }
    in Decl ty ne
  TypeDecl ty -> error "assign_param: TypeDecl" 

valueToConst :: Value -> Constant
valueToConst v = case v of 
 VInt    k -> const_int k 
 VFloat  k -> const_float k
 VBool   k -> const_bool k
 VChar   k -> const_char k
 VString k -> const_string k
 _ -> error "valueToConst: missing conversion"
  
valueToExpr :: Value -> Expression ident a
valueToExpr val = Const $ valueToConst val 
