{-#LANGUAGE UndecidableInstances #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE FlexibleContexts #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE DoAndIfThenElse #-}

{-
-}
module Language.SimpleC.Flow where

import Language.SimpleC.AST 
import qualified Data.Map as M
import Data.Map (Map)
import Control.Monad.State.Lazy

type NodeId = Int

data NodeInfo = NodeInfo 
  deriving Show

type Graphs ident a
 = Map ident (Graph ident a)

data Graph ident a
 = Graph
   {
     entry_node :: NodeId              -- entry point
   , graph :: Map NodeId [NodeId]      -- successors
   , node_table :: Map NodeId NodeInfo -- information
   }
  deriving Show


-- data GraphProg a
--  = GProg
--    {
--      decls :: Declarations SymId a
--    , defs  :: Graphs SymId a
--    , ams_ext :: AsmExt a
--    } 
--   deriving Show
 
data FlowState ident node
  = FlowState {
    graphs  :: Graphs ident node
  , entries :: Map ident NodeId
  , current :: [NodeId] 
  , pc_counter :: NodeId
  } deriving Show

init_st :: FlowState ident node
init_st = FlowState M.empty M.empty [] 0

type FlowOp ident node val = State (FlowState ident node) val

-- | API
incCounter :: FlowOp ident node Int
incCounter = do
  p@FlowState{..} <- get
  let c' = pc_counter + 1
  put p {pc_counter = c'}
  return c'

addEntry :: Ord ident => ident -> NodeId -> FlowOp ident node ()
addEntry sym n = do
  p@FlowState{..} <- get
  let e = M.insert sym n entries
  put p {entries = e}

-- | Main Functions
computeGraphs :: Ord ident => Program ident node -> Graphs ident node
computeGraphs prog = 
  let ((),st) = runState (toFlow prog) init_st
  in graphs st

-- | Main ToFlow class 
class Flow a i n v  where
  toFlow :: a -> FlowOp i n v

-- | Flow Program 
instance Ord ident => Flow (Program ident node) ident node () where
  toFlow p@Prog{..} = do 
    mapM_ computeEntry defs
    mapM_ computeGraph defs 

getFnIdent :: FunctionDef ident node -> ident
getFnIdent fn@FunDef{..} = _getFnIdent symbol
  where 
    _getFnIdent de@Declr{..} =
      case declr_ident of
        Nothing -> error "compute entry: no function declarator"
        Just sym -> sym

computeEntry :: Ord ident => FunctionDef ident node -> FlowOp ident node ()
computeEntry fn = do
  let sym = getFnIdent fn
  n <- incCounter
  addEntry sym n

computeGraph :: Ord ident => FunctionDef ident node -> FlowOp ident node ()
computeGraph fn@FunDef{..} = return () -- undefined 
