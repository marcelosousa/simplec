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
import qualified Debug.Trace as T

trace a b = b
-- trace = T.trace

type NodeId = Int

data Code ident n
 = E (Expression ident n) 
 | D (Declaration ident n)
  deriving Show
 
data EdgeTag = LoopHead | IfJoin | Entry | Exit | CondTag
  deriving (Show, Eq)

data EdgeInfo ident n
 = EdgeInfo 
   {
     edge_tags :: [EdgeTag]
   , edge_code :: Code ident n
   }
  deriving Show

instance Eq (EdgeInfo ident n) where
  (==) n1 n2 = edge_tags n1 == edge_tags n2

type EdgeId = Int
type Graphs ident n st
 = Map ident (Graph ident n st)

data Graph ident n st
 = Graph
   {
     entry_node :: NodeId                        -- entry point
   , graph :: Map NodeId [(EdgeId,NodeId)]       -- successors
   , edge_table :: Map EdgeId (EdgeInfo ident n) -- information
   , node_table :: Map NodeId [st]               -- ^ states per thread 
   }
  deriving Show

succs :: Graph ident n st -> NodeId -> [(EdgeId,NodeId)]
succs cfg@Graph{..} node = case M.lookup node graph of
  Nothing -> [] -- error "succs: no successors for node"
  Just s  -> s

get_edge_info :: Graph ident n st -> EdgeId -> EdgeInfo ident n
get_edge_info cfg@Graph{..} edge = case M.lookup edge edge_table of
  Nothing -> error "get_edge_info: no info for edge id"
  Just s  -> s

get_node_info :: Graph ident n st -> NodeId -> [st]
get_node_info cfg@Graph{..} node = case M.lookup node node_table of
  Nothing -> [] 
  Just s  -> s
 
is_cond :: [EdgeTag] -> Bool
is_cond = any (== CondTag)  

is_exit :: [EdgeTag] -> Bool
is_exit = any (== Exit)  


is_join :: [EdgeTag] -> Bool
is_join = any (== IfJoin)  

init_graph :: NodeId -> Graph ident a st
init_graph e = Graph e M.empty M.empty M.empty

data FlowState ident node st
  = FlowState {
    graphs  :: Graphs ident node st
  , this    :: Graph ident node st
  , entries :: Map ident NodeId
  , prev :: [NodeId] 
  , current :: NodeId 
  , next  :: [NodeId] 
  , exit :: Maybe NodeId 
  , pc_counter :: NodeId
  , edge_counter :: EdgeId 
  } deriving Show

init_st :: FlowState ident node st
init_st = FlowState M.empty undefined M.empty [] 0 [] Nothing 0 0

type FlowOp ident node val st = State (FlowState ident node st) val

-- | API
addEdgeInfo :: EdgeId -> EdgeInfo ident node -> FlowOp ident node () st
addEdgeInfo eId eInfo = do
  p@FlowState{..} <- get
  let table = edge_table this
  case M.lookup eId table of
    Nothing -> do
      let new_table = M.insert eId eInfo table 
          this' = this {edge_table = new_table}
      put p {this = this'}
    Just info ->
      if eInfo == info
      then return ()
      else error "different info's: not sure what to do"
 
addEdge :: NodeId -> EdgeId -> NodeId -> FlowOp ident node () st
addEdge nA eI nB = do 
  p@FlowState{..} <- get
  let gr = graph this
  case M.lookup nA gr of
    Nothing -> do
      let new_gr = M.insert nA [(eI,nB)] gr 
          this' = this {graph = new_gr}
      put p {this = this'}
    Just nodes -> do
      let n = nub $ (eI,nB):nodes
          new_gr = M.insert nA n gr
          this' = this {graph = new_gr}
      put p {this = this'}

addThis :: Ord ident => ident -> FlowOp ident node () st
addThis sym = do 
  p@FlowState{..} <- get
  let graphs' = M.insert sym this graphs
  put p {graphs = graphs'}

replaceGraph :: Graph ident node st -> FlowOp ident node () st
replaceGraph graph = do
  p@FlowState{..} <- get
  put p {this = graph}
 
incCounter :: FlowOp ident node Int st
incCounter = do
  p@FlowState{..} <- get
  let c' = pc_counter + 1
  put p {pc_counter = c'}
  return c'

incEdCounter :: FlowOp ident node Int st
incEdCounter = do
  p@FlowState{..} <- get
  let c' = edge_counter + 1
  put p {edge_counter = c'}
  return c'

addEntry :: Ord ident => ident -> NodeId -> FlowOp ident node () st
addEntry sym n = do
  p@FlowState{..} <- get
  let e = M.insert sym n entries
  put p {entries = e}

getCurrent :: FlowOp ident node NodeId st
getCurrent = do
  p@FlowState{..} <- get
  return current

getExit :: FlowOp ident node (Maybe NodeId) st
getExit = do
  p@FlowState{..} <- get
  return exit 

replaceExit :: Maybe NodeId -> FlowOp ident node () st
replaceExit n = do
  p@FlowState{..} <- get
  put p {exit = n}

replaceCurrent :: NodeId -> FlowOp ident node () st
replaceCurrent n = do
  p@FlowState{..} <- get
  put p {current = n}

pushPrev :: NodeId -> FlowOp ident node () st
pushPrev n = do
  p@FlowState{..} <- get
  let e = n:prev
  put p {prev = e}

popPrev :: FlowOp ident node NodeId st
popPrev = do
  p@FlowState{..} <- get
  case prev of
    [] -> error "cant pop empty stack"
    (x:xs) -> do
      put p {prev = xs}
      return x 

getPrev :: FlowOp ident node NodeId st
getPrev = do
  p@FlowState{..} <- get
  case prev of
    [] -> error "cant get current: empty"
    (x:xs) -> return x
 
pushNext :: NodeId -> FlowOp ident node () st
pushNext n = do
  p@FlowState{..} <- get
  let e = n:next 
  put p {next = e}

popNext :: FlowOp ident node NodeId st
popNext = do
  p@FlowState{..} <- get
  case next of
    [] -> error "cant pop empty stack"
    (x:xs) -> do
      put p {next = xs}
      return x 

getPCLabel :: Ord ident => ident -> FlowOp ident node NodeId st
getPCLabel sym = do
  p@FlowState{..} <- get
  case M.lookup sym entries of
    Nothing -> do
      lab <- incCounter
      addEntry sym lab
      return lab 
 
getNext :: FlowOp ident node NodeId st
getNext = do
  p@FlowState{..} <- get
  case next of
    [] -> error "cant get next: empty"
    (x:xs) -> return x
 
getEntryId :: Ord ident => ident -> FlowOp ident node NodeId st
getEntryId sym = do
  p@FlowState{..} <- get
  case M.lookup sym entries of
    Nothing -> error "getEntryId"
    Just n  -> return n

-- | Main Functions
computeGraphs :: Ord ident => Program ident node -> Graphs ident node st
computeGraphs prog = 
  let ((),st) = runState (toFlow prog) init_st
  in graphs st

-- | Main ToFlow class 
class Flow a i n v  where
  toFlow :: a -> FlowOp i n v st

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

computeEntry :: Ord ident => FunctionDef ident node -> FlowOp ident node () st
computeEntry fn = do
  let sym = getFnIdent fn
  n <- incCounter
  addEntry sym n

computeGraph :: Ord ident => FunctionDef ident node -> FlowOp ident node () st
computeGraph fn@FunDef{..} = do
  let sym = getFnIdent fn
  entry <- getEntryId sym
  let this = init_graph entry
  replaceGraph this
  replaceCurrent entry
  computeGraphBody body
  addThis sym 
  return ()

computeGraphBody :: Ord ident => Statement ident node -> FlowOp ident node Bool st
computeGraphBody stmt =
  case stmt of
    Break n -> do
      eId <- incEdCounter
      curr <- getCurrent
      exit <- getExit
      next <- do
        case exit of
          Nothing -> getNext
          Just l  -> return l
      let eInfo = EdgeInfo [] (E Skip) 
      addEdgeInfo eId eInfo
      addEdge curr eId next
      return True 
    Case expr body n -> do
      eId <- incEdCounter
      curr <- getCurrent
      new <- incCounter
      replaceCurrent new
      let eInfo = EdgeInfo [] (E expr)
      addEdgeInfo eId eInfo
      addEdge curr eId new
      computeGraphBody body 
    Cases lower upper body n -> do
      cur <- getCurrent
      lowerId <- incEdCounter
      upperId <- incEdCounter
      lowerPc <- incCounter
      upperPc <- incCounter
      let lowerInfo = EdgeInfo [] (E lower)
          upperInfo = EdgeInfo [] (E upper)
      addEdgeInfo lowerId lowerInfo 
      addEdgeInfo upperId upperInfo
      addEdge cur lowerId lowerPc
      addEdge lowerPc upperId upperPc
      replaceCurrent upperPc 
      computeGraphBody body 
    Compound idents items n ->
      case idents of
        [] -> computeGraphCompound items
        _ -> error "compound has idents"
    Cont n -> do
      edgeId <- incEdCounter
      curr <- getCurrent
      prev <- getPrev
      let eInfo = EdgeInfo [] (E Skip) 
      addEdgeInfo edgeId eInfo
      addEdge curr edgeId prev
      return True 
    Default stat n ->  
      computeGraphBody stat
    Expr mExpr n -> 
      case mExpr of
        Nothing -> return False
        Just e  -> do 
          edgeId <- incEdCounter
          curr <- getCurrent
          next <- getNext
          let eInfo = EdgeInfo [] (E e) 
          addEdgeInfo edgeId eInfo
          addEdge curr edgeId next 
          replaceCurrent next
          return False 
    For begin cond end body n -> computeFor begin cond end body 
    Goto ident n -> do
      edgeId <- incEdCounter
      curr <- getCurrent
      next <- getPCLabel ident 
      let eInfo = EdgeInfo [] (E Skip) 
      addEdgeInfo edgeId  eInfo
      addEdge curr edgeId next
      return False
    GotoPtr expr n -> error "GotoPtr not supported"
    If cond tStmt mEStmt n -> do
      curr <- getCurrent
      next <- getNext -- Join point
      thenEdge <- incEdCounter
      elseEdge <- incEdCounter
      joinTEdge <- incEdCounter
      joinEEdge <- incEdCounter
      thenPc <- incCounter
      elsePc <- incCounter
      let eThen = EdgeInfo [CondTag] (E cond)
          eElse = EdgeInfo [CondTag] (E (Unary CNegOp cond))
          eJoin = EdgeInfo [IfJoin] (E Skip)
      -- Add the edges from the branches
      addEdgeInfo thenEdge eThen
      addEdgeInfo elseEdge eElse
      addEdge curr thenEdge thenPc
      addEdge curr elseEdge elsePc
      -- Add the edges info for the joins
      addEdgeInfo joinTEdge eJoin 
      addEdgeInfo joinEEdge eJoin
      -- Execute then branch 
      replaceCurrent thenPc 
      bT <- computeGraphBody tStmt
      -- Add the join edge from the then
      _ <- if not bT 
      then do
        curr <- getCurrent
        addEdge curr joinTEdge next
        return False
      else return False
      case mEStmt of
        Nothing -> do
          addEdge elsePc joinEEdge next
          replaceCurrent next 
          return False 
        Just eStmt -> do
          replaceCurrent elsePc
          bE <- computeGraphBody eStmt
          trace ("inside if with bE = " ++ show bE ) $ if not bE 
          then do 
            curr <- getCurrent
            addEdge curr joinEEdge next
            replaceCurrent next
            return False
          else do
            replaceCurrent next 
            return False
    Label sym stat attrs n ->
      case attrs of
        [] -> do
          eId <- incEdCounter
          curr <- getPCLabel sym 
          next <- incCounter
          let eInfo = EdgeInfo [] (E Skip)
          addEdgeInfo eId eInfo
          addEdge curr eId next
          replaceCurrent next
          computeGraphBody stat
        _ -> error "cant handle label with attributes"
    -- TODO: Need to warn the next one 
    Return mExpr n -> do
      eId <- incEdCounter
      curr <- getCurrent
      next <- getNext 
      let eInfo = case mExpr of
            Nothing -> EdgeInfo [Exit] (E Skip)
            Just e  -> EdgeInfo [Exit] (E e)
      addEdgeInfo eId eInfo
      addEdge curr eId next
      return True
    -- Be careful with the case statements
    Switch cond body n -> do
      eId <- incEdCounter
      curr <- getCurrent
      next <- incCounter
      prev_exit <- getExit
      prev_next <- getNext
      replaceExit (Just prev_next)
      let eInfo = EdgeInfo [] (E cond)
      addEdgeInfo eId eInfo
      addEdge curr eId next
      replaceCurrent next
      computeGraphBody body
      replaceExit prev_exit
      return False
    While cond body isDoWhile n -> computeWhile cond body isDoWhile

computeGraphCompound :: Ord ident => [CompoundBlockItem ident a] -> FlowOp ident a Bool st
computeGraphCompound list =
  case list of
    [] -> return False
    [item] -> do 
      next <- incCounter
      pushNext next
      case item of
        BlockStmt stmt -> do
          b <- computeGraphBody stmt
          popNext
          return b
        BlockDecl decls -> do
          b <- computeGraphDecls decls
          popNext
          return b
        NestedFunDef _ -> error "cant handle nested functions"
    (item:rest) -> do
      curr <- getCurrent
      next <- incCounter
      trace ("Compound: " ++ show (curr,next)) $ pushNext next
      case item of
        BlockStmt stmt -> do
          b <- computeGraphBody stmt
          popNext
          if b
          then return False
          else computeGraphCompound rest
        BlockDecl decls -> do
          b <- computeGraphDecls decls
          popNext
          computeGraphCompound rest
        NestedFunDef _ -> error "cant handle nested functions"

computeFor begin cond end body = do
  -- Take care of the initialization part
  _ <- case begin of
    Left init -> do
      iEid <- incEdCounter
      let initInfo = case init of
            Nothing -> EdgeInfo [] (E Skip)
            Just i  -> EdgeInfo [] (E i)
      curr <- getCurrent
      addEdgeInfo iEid initInfo
      next <- incCounter
      addEdge curr iEid next 
      replaceCurrent next
      return False 
    Right decls -> computeGraphDecls decls
  -- After initialization we have an usual while loop
  -- The current should be the condition and the scope
  condPc <- getCurrent
  condEId <- incEdCounter
  trueEId <- incEdCounter
  falseEId <- incEdCounter
  let eInfo = EdgeInfo [LoopHead] (E Skip)
      _cond = case cond of
         Nothing -> Const (BoolConst True)
         Just e  -> e
      eTrue = EdgeInfo [] (E _cond)
      eFalse = EdgeInfo [] (E (Unary CNegOp _cond))
  addEdgeInfo condEId eInfo
  addEdgeInfo trueEId eTrue
  addEdgeInfo falseEId eFalse
  truePc <- incCounter
  falsePc <- getNext
  -- Add the edges from the loop head
  addEdge condPc trueEId truePc
  addEdge condPc falseEId falsePc
  -- Going to do the loop body now
  replaceCurrent truePc
  endPc <- incCounter
  pushPrev endPc
  computeGraphBody body
  -- New current should be at the end
  curr <- getCurrent
  tranE <- incEdCounter
  endE  <- incEdCounter
  let eTran = EdgeInfo [] (E Skip)
      eEnd = case end of
        Nothing -> EdgeInfo [] (E Skip)
        Just e -> EdgeInfo [] (E e)
  addEdgeInfo endE eEnd
  addEdgeInfo tranE eTran
  addEdge curr tranE endPc
  addEdge endPc endE condPc 
  popPrev
  return False

computeWhile :: Ord ident => Expression ident a -> Statement ident a -> Bool -> FlowOp ident a Bool st
computeWhile cond body doWhile = 
  if doWhile
  then error "no support for do while loops"
  else do
    curr <- getCurrent
    pushPrev curr
    trueE <- incEdCounter
    falseE <- incEdCounter
    let eTrue = EdgeInfo [LoopHead] (E cond)
        eFalse = EdgeInfo [LoopHead] (E (Unary CNegOp cond))
    truePc <- incCounter
    falsePc <- incCounter
    addEdgeInfo trueE eTrue
    addEdgeInfo falseE eFalse
    addEdge curr trueE truePc
    addEdge curr falseE falsePc
    replaceCurrent truePc 
    prev_exit <- getExit
    replaceExit (Just falsePc)
    computeGraphBody body 
    popPrev
    let eEnd = EdgeInfo [] (E Skip)
    endE <- incEdCounter
    addEdgeInfo endE eEnd
    _curr <- getCurrent
    addEdge _curr endE curr
    replaceCurrent falsePc
    replaceExit prev_exit
    return False

computeGraphDecls :: Ord ident => [Declaration ident a] -> FlowOp ident a Bool st
computeGraphDecls decls = 
  case decls of
    [] -> return False
    (d:rest) -> do
      curr <- getCurrent
      let eInfo = EdgeInfo [] (D d)
      eId <- incEdCounter
      addEdgeInfo eId eInfo
      next <- incCounter
      addEdge curr eId next
      replaceCurrent next
      computeGraphDecls rest 
