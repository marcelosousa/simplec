{-# LANGUAGE RecordWildCards #-}

module Language.SimpleC.Interpreter where

import Control.Monad.State.Lazy

import Data.Map (Map)
import qualified Data.Map as M
import Data.IntMap

import Language.SimpleC.AST 
import Language.SimpleC.Flow
import Language.SimpleC.Util

type MCell = MemCell SymId ()
type Heap = IntMap MCell 
type Stack = IntMap MCell 
type Types = [Type SymId ()]
type Globals = Map SymId MCell 

data Env = 
  Env {
    heap :: Heap
  , proc :: Functions
  , tys  :: Types 
  , gls  :: Globals
  }

-- Map Pid Functions
type Functions = IntMap Function 

-- Not sure what a process needs yet.
-- | Calling context [(Stack, Function)]
data Function = 
  Func {
    stack :: Stack
  , fcode :: Graph SymId () 
  , pos  :: Int
  }  

type InterOp a = State Env a

-- API 
addType :: Type SymId () -> InterOp ()
addType ty = do
  s@Env{..} <- get
  let tys' = ty:tys
  put s {tys = tys'}

addGlobal :: SymId -> MCell -> InterOp ()
addGlobal id cell = do
  s@Env{..} <- get
  let gls' = M.insert id cell gls 
  put s {gls = gls'}

-- The purpose of the interpreter 
-- is to load the program and put
-- it in a state where one can
-- interpret functions with arguments
interpreter :: FrontEnd () -> ()
interpreter = undefined

-- Given the FrontEnd
-- computes an environment/configuration
-- with the initial declarations
i_env :: FrontEnd () -> InterOp ()
i_env f@FrontEnd{..} = mapM_ interGDecl $ decls ast   

interGDecl :: Declaration SymId () -> InterOp ()
interGDecl decl =
  case decl of
    Decl ty decl@DeclElem{..} ->
      case declarator of
        Nothing -> error "Cant interpreter declaration without declarator"
        Just d@Declr{..} -> do
          let id = case declr_ident of
                    Nothing -> error "Cant interpreter declarator without identifier"
                    Just id -> id
              full_type = Ty declr_type ty
              ival = case initializer of
                       Nothing -> init_value full_type
                       Just init -> undefined
              cell = MCell full_type ival
          addGlobal id cell 
    TypeDecl ty -> addType ty

-- Interpret an expression
-- Transformers for the concrete semantics 
interExpr :: Expression SymId () -> InterOp Value
interExpr expr = case expr of
  AlignofExpr expr -> error "interExpr: align of expr" 
  AlignofType decl -> error "interExpr: align of type"
  Assign assignOp lhs rhs -> error "interExpr: align of type"
  Binary binaryOp lhs rhs -> error "interExpr: "  
  BuiltinExpr builtin -> error "interExpr: "  
  Call fn args _ -> error "interExpr: " 
  Cast decl expr -> error "interExpr: " 
  Comma exprs -> error "interExpr: "  
  CompoundLit decl init -> error "interExpr: " 
  Cond cond th el -> error "interExpr: " 
  Const const -> error "interExpr: " 
  Index arr_expr ind_expr -> error "interExpr: " 
  LabAddrExpr ident -> error "interExpr: " 
  Member struct ident bool->  error "interExpr: " 
  SizeofExpr expr -> error "interExpr: " 
  SizeofType decl -> error "interExpr: "
  Skip -> error "interExpr: " 
  StatExpr stat -> error "interExpr: " 
  Unary unaryOp expr -> error "interExpr: " 
  Var ident -> error "interExpr: " 
  _ -> error "interExpr: " 
