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
import Data.List 
import Data.Map (Map)
import Control.Monad.State.Lazy

type NodeId = Int

data Code ident n
 = E (Expression ident n) 
 | D (Declaration ident n)
  deriving Show
 
data NodeTag = LoopHead | IfJoin | Entry | Exit
  deriving (Show, Eq)

data NodeInfo ident n
 = NodeInfo 
   {
     node_tags :: [NodeTag]
   , node_code :: Code ident n
   }
  deriving Show

instance Eq (NodeInfo ident n) where
  (==) n1 n2 = node_tags n1 == node_tags n2

type Graphs ident n
 = Map ident (Graph ident n)

data Graph ident n
 = Graph
   {
     entry_node :: NodeId              -- entry point
   , graph :: Map NodeId [NodeId]      -- successors
   , node_table :: Map NodeId (NodeInfo ident n) -- information
   }
  deriving Show

init_graph :: NodeId -> Graph ident a
init_graph e = Graph e M.empty M.empty

data FlowState ident node
  = FlowState {
    graphs  :: Graphs ident node
  , this    :: Graph ident node
  , entries :: Map ident NodeId
  , current :: [NodeId] 
  , next  :: [NodeId] 
  , pc_counter :: NodeId
  } deriving Show

init_st :: FlowState ident node
init_st = FlowState M.empty undefined M.empty [] [] 0

type FlowOp ident node val = State (FlowState ident node) val

-- | API
addNode :: NodeId -> NodeInfo ident node -> FlowOp ident node ()
addNode nId nInfo = do
  p@FlowState{..} <- get
  let table = node_table this
  case M.lookup nId table of
    Nothing -> do
      let new_table = M.insert nId nInfo table 
          this' = this {node_table = new_table}
      put p {this = this'}
    Just info ->
      if nInfo == info
      then return ()
      else error "different info's: not sure what to do"
 
addEdge :: NodeId -> NodeId -> FlowOp ident node ()
addEdge nA nB = do 
  p@FlowState{..} <- get
  let gr = graph this
  case M.lookup nA gr of
    Nothing -> do
      let new_gr = M.insert nA [nB] gr 
          this' = this {graph = new_gr}
      put p {this = this'}
    Just nodes -> do
      let n = nub $ nB:nodes
          new_gr = M.insert nA n gr
          this' = this {graph = new_gr}
      put p {this = this'}

addThis :: Ord ident => ident -> FlowOp ident node ()
addThis sym = do 
  p@FlowState{..} <- get
  let graphs' = M.insert sym this graphs
  put p {graphs = graphs'}

replaceGraph :: Graph ident node -> FlowOp ident node ()
replaceGraph graph = do
  p@FlowState{..} <- get
  put p {this = graph}
 
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

pushCurrent :: NodeId -> FlowOp ident node ()
pushCurrent n = do
  p@FlowState{..} <- get
  let e = n:current 
  put p {current = e}

popCurrent :: FlowOp ident node NodeId
popCurrent = do
  p@FlowState{..} <- get
  case current of
    [] -> error "cant pop empty stack"
    (x:xs) -> do
      put p {current = xs}
      return x 

getCurrent :: FlowOp ident node NodeId
getCurrent = do
  p@FlowState{..} <- get
  case current of
    [] -> error "cant get current: empty"
    (x:xs) -> return x
 
pushNext :: NodeId -> FlowOp ident node ()
pushNext n = do
  p@FlowState{..} <- get
  let e = n:next 
  put p {next = e}

popNext :: FlowOp ident node NodeId
popNext = do
  p@FlowState{..} <- get
  case next of
    [] -> error "cant pop empty stack"
    (x:xs) -> do
      put p {next = xs}
      return x 

getNext :: FlowOp ident node NodeId
getNext = do
  p@FlowState{..} <- get
  case next of
    [] -> error "cant get next: empty"
    (x:xs) -> return x
 
getEntryId :: Ord ident => ident -> FlowOp ident node NodeId
getEntryId sym = do
  p@FlowState{..} <- get
  case M.lookup sym entries of
    Nothing -> error "getEntryId"
    Just n  -> return n

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
computeGraph fn@FunDef{..} = do
  let sym = getFnIdent fn
  entry <- getEntryId sym
  let this = init_graph entry
  replaceGraph this
  pushCurrent entry
  computeGraphBody body
  popCurrent
  addThis sym 
  return ()

computeGraphBody :: Ord ident => Statement ident node -> FlowOp ident node ()
computeGraphBody stmt =
  case stmt of
    Break n -> do
      next <- getNext
      undefined
    _ -> undefined
{- 
       
    Case (Expression ident a) (Statement ident a) a
    Cases (Expression ident a) (Expression ident a) (Statement ident a) a
    Compound [ident] [CompoundBlockItem ident a] a
    Cont a
    Default (Statement ident a) a
    Expr (Maybe (Expression ident a)) a
    For (Either (Maybe (Expression ident a)) [Declaration ident a])
        (Maybe (Expression ident a))
        (Maybe (Expression ident a))
        (Statement ident a)
        a
    Goto ident a
    GotoPtr (Expression ident a) a
    If (Expression ident a) (Statement ident a) (Maybe (Statement ident a)) a
    Label ident (Statement ident a) [Attribute ident a] a
    Return (Maybe (Expression ident a)) a
    Switch (Expression ident a) (Statement ident a) a
    While (Expression ident a) (Statement ident a) Bool a
-}
