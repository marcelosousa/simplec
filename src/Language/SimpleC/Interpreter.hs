{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.SimpleC.Interpreter where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.IntMap.Lazy as IM
import Data.IntMap.Lazy (IntMap) 
import Data.Array
import Language.C.Syntax.AST
import Language.C.Data.Node (NodeInfo)
import qualified Language.SimpleC.AST as SC
import Control.Monad.State.Lazy

data MemCell = MemCell
type Heap = IntMap MemCell
 
data Env = 
  Env {
    heap :: Heap
  , proc :: Processes
  }

-- Map Pid Process
type Processes = IntMap Process 

-- Not sure what a process needs yet.
-- | Calling context [(Stack, Function)]
data Process = 
  Proc {
    stack :: Stack
  , code :: SC.CFG
  , pos  :: Int
  }  

type Stack = IntMap MemCell 

type EnvState = State Env
 
interpret :: SC.Program -> EnvState ()
interpret (SC.Prog decls defs _) = undefined 
