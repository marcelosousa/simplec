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
import Language.C.Syntax.AST 
import qualified Data.Map as M
import Data.List 
import Data.Map (Map)
import Control.Monad.State.Lazy
import Debug.Trace

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
  , prev :: [NodeId] 
  , current :: NodeId 
  , next  :: [NodeId] 
  , switch_exit :: Maybe NodeId 
  , pc_counter :: NodeId
  } deriving Show

init_st :: FlowState ident node
init_st = FlowState M.empty undefined M.empty [] 0 [] Nothing 0

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

getCurrent :: FlowOp ident node NodeId
getCurrent = do
  p@FlowState{..} <- get
  return current

getSwitch :: FlowOp ident node (Maybe NodeId)
getSwitch = do
  p@FlowState{..} <- get
  return switch_exit 

replaceSwitch :: Maybe NodeId -> FlowOp ident node ()
replaceSwitch n = do
  p@FlowState{..} <- get
  put p {switch_exit = n}

replaceCurrent :: NodeId -> FlowOp ident node ()
replaceCurrent n = do
  p@FlowState{..} <- get
  put p {current = n}

pushPrev :: NodeId -> FlowOp ident node ()
pushPrev n = do
  p@FlowState{..} <- get
  let e = n:prev
  put p {prev = e}

popPrev :: FlowOp ident node NodeId
popPrev = do
  p@FlowState{..} <- get
  case prev of
    [] -> error "cant pop empty stack"
    (x:xs) -> do
      put p {prev = xs}
      return x 

getPrev :: FlowOp ident node NodeId
getPrev = do
  p@FlowState{..} <- get
  case prev of
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

getPCLabel :: Ord ident => ident -> FlowOp ident node NodeId
getPCLabel sym = do
  p@FlowState{..} <- get
  case M.lookup sym entries of
    Nothing -> do
      lab <- incCounter
      addEntry sym lab
      return lab 
 
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
  replaceCurrent entry
  computeGraphBody body
  addThis sym 
  return ()

computeGraphBody :: Ord ident => Statement ident node -> FlowOp ident node ()
computeGraphBody stmt =
  case stmt of
    Break n -> do
      curr <- getCurrent
      switch_exit <- getSwitch
      next <- do
        case switch_exit of
          Nothing -> getNext
          Just l  -> return l
      let nInfo = NodeInfo [] (E Skip) 
      addNode curr nInfo
      addEdge curr next 
    Case expr body n -> do
      curr <- getCurrent
      new <- incCounter
      replaceCurrent new
      let nInfo = NodeInfo [] (E expr)
      addNode curr nInfo
      addEdge curr new
      computeGraphBody body 
    Cases lower upper body n -> do
      cur <- getCurrent
      lowerPc <- incCounter
      upperPc <- incCounter
      let lowerInfo = NodeInfo [] (E lower)
          upperInfo = NodeInfo [] (E upper)
      addNode cur lowerInfo 
      addNode lowerPc upperInfo
      addEdge cur lowerPc
      addEdge lowerPc upperPc
      replaceCurrent upperPc 
      computeGraphBody body 
    Compound idents items n ->
      case idents of
        [] -> computeGraphCompound items
        _ -> error "compound has idents"
    Cont n -> do
      curr <- getCurrent
      prev <- getPrev
      let nInfo = NodeInfo [] (E Skip) 
      addNode curr nInfo
      addEdge curr prev 
    Default stat n ->  
      computeGraphBody stat
    Expr mExpr n -> 
      case mExpr of
        Nothing -> return ()
        Just e  -> do 
          curr <- getCurrent
          next <- getNext
          let nInfo = NodeInfo [] (E e) 
          trace ("Expr: " ++ show curr) $ addNode curr nInfo
          addEdge curr next 
          replaceCurrent next 
    For begin cond end body n -> computeFor begin cond end body 
    Goto ident n -> do
      curr <- getCurrent
      next <- getPCLabel ident 
      let nInfo = NodeInfo [] (E Skip) 
      addNode curr nInfo
      addEdge curr next
      next' <- incCounter
      replaceCurrent next'      
    GotoPtr expr n -> error "GotoPtr not supported"
    If cond tStmt mEStmt n -> do
      curr <- getCurrent
      next <- getNext -- Join point
      thenPc <- incCounter
      elsePc <- incCounter
      let nInfo = NodeInfo [] (E Skip)
          nThen = NodeInfo [] (E cond)
          nElse = NodeInfo [] (E (Unary CNegOp cond))
          nJoin = NodeInfo [IfJoin] (E Skip) 
      addNode curr nInfo
      addNode thenPc nThen 
      addNode elsePc nElse 
      addEdge curr thenPc
      addEdge curr elsePc
      nextT <- incCounter
      addEdge thenPc nextT 
      replaceCurrent nextT 
      computeGraphBody tStmt
      currentThen <- getCurrent
      let nAThen = NodeInfo [] (E Skip)
      -- addNode next nJoin
      addNode currentThen nAThen
      addEdge currentThen next
      case mEStmt of
        Nothing -> do
          next <- getNext
          addEdge elsePc next 
        Just eStmt -> do
          nextE <- incCounter
          addEdge elsePc nextE 
          replaceCurrent nextE 
          computeGraphBody eStmt
          currentElse <- getCurrent
          let nAElse = NodeInfo [] (E Skip)
          addNode currentElse nAElse
          addEdge currentElse next
          replaceCurrent next
    Label sym stat attrs n ->
      case attrs of
        [] -> do
          curr <- getPCLabel sym 
          next <- incCounter
          let nInfo = NodeInfo [] (E Skip)
          addNode curr nInfo
          addEdge curr next
          replaceCurrent next
          computeGraphBody stat
        _ -> error "cant handle label with attributes"
    -- TODO: Need to warn the next one 
    Return mExpr n -> do
      curr <- getCurrent
      next <- getNext 
      let nInfo = case mExpr of
            Nothing -> NodeInfo [Exit] (E Skip)
            Just e  -> NodeInfo [Exit] (E e)
      addNode curr nInfo
      addEdge curr next
    -- Be careful with the case statements
    Switch cond body n -> do
      curr <- getCurrent
      next <- incCounter
      prev_switch <- getSwitch
      prev_next <- getNext
      replaceSwitch (Just prev_next)
      let nInfo = NodeInfo [] (E cond)
      addNode curr nInfo
      addEdge curr next
      replaceCurrent next
      computeGraphBody body
      replaceSwitch prev_switch
    While cond body isDoWhile n -> computeWhile cond body isDoWhile

computeGraphCompound :: Ord ident => [CompoundBlockItem ident a] -> FlowOp ident a () 
computeGraphCompound list =
  case list of
    [] -> return () 
    (item:rest) -> do
      curr <- getCurrent
      next <- incCounter
      trace ("Compound: " ++ show (curr,next)) $ pushNext next
      case item of
        BlockStmt stmt -> do
          computeGraphBody stmt
          popNext
          computeGraphCompound rest
        BlockDecl decls -> do
          computeGraphDecls decls
          popNext
          computeGraphCompound rest
        NestedFunDef _ -> error "cant handle nested functions"

computeFor begin cond end body = do
  -- Take care of the initialization part
  case begin of
    Left init -> do
      let initInfo = case init of
            Nothing -> NodeInfo [] (E Skip)
            Just i  -> NodeInfo [] (E i)
      curr <- getCurrent
      addNode curr initInfo
      next <- incCounter
      addEdge curr next 
      replaceCurrent next 
    Right decls -> computeGraphDecls decls
  -- After initialization we have an usual while loop
  -- The current should be the condition and the scope
  condPc <- getCurrent
  let nInfo = NodeInfo [LoopHead] (E Skip)
      _cond = case cond of
         Nothing -> Const (BoolConst True)
         Just e  -> e
      nTrue = NodeInfo [] (E _cond)
      nFalse = NodeInfo [] (E (Unary CNegOp _cond))
  addNode condPc nInfo
  truePc <- incCounter
  falsePc <- getNext
  -- Add the edges from the loop head
  addEdge condPc truePc
  addEdge condPc falsePc
  -- Going to do the loop body now
  replaceCurrent truePc
  endPc <- incCounter
  pushPrev endPc
  computeGraphBody body
  -- New current should be at the end
  curr <- getCurrent
  let nTran = NodeInfo [] (E Skip)
      nEnd = case end of
        Nothing -> NodeInfo [] (E Skip)
        Just e -> NodeInfo [] (E e)
  addNode curr nTran
  addNode endPc nEnd
  addEdge curr endPc
  addEdge endPc condPc 
  popPrev
  return ()

computeWhile :: Ord ident => Expression ident a -> Statement ident a -> Bool -> FlowOp ident a ()
computeWhile cond body doWhile = 
  if doWhile
  then error "no support for do while loops"
  else do
    curr <- getCurrent
    pushPrev curr
    let nInfo = NodeInfo [LoopHead] (E Skip)
        nTrue = NodeInfo [] (E cond)
        nFalse = NodeInfo [] (E (Unary CNegOp cond))
    addNode curr nInfo
    truePc <- incCounter
    falsePc <- incCounter
    addNode curr nInfo
    addNode truePc nTrue 
    addEdge curr truePc
    addEdge curr falsePc
    replaceCurrent truePc 
    computeGraphBody body 
    popPrev
    return ()

computeGraphDecls :: Ord ident => [Declaration ident a] -> FlowOp ident a ()
computeGraphDecls decls = 
  case decls of
    [] -> return ()
    (d:rest) -> do
      curr <- getCurrent
      let nInfo = NodeInfo [] (D d)
      addNode curr nInfo
      next <- incCounter
      addEdge curr next
      replaceCurrent next
      computeGraphDecls rest 
