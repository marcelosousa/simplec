{-# LANGUAGE RecordWildCards #-}

module Language.SimpleC.Interpreter where

import Control.Monad.State.Lazy

import Data.Map (Map)
import Data.IntMap

import Language.SimpleC.AST 
import Language.SimpleC.Converter
import Language.SimpleC.Flow
import Language.SimpleC.Util

{-
data FrontEnd node 
 = FrontEnd 
 { 
   ast  :: Program SymId node
 , cfgs :: Graphs SymId  node
 , symt :: Map SymId Symbol
 } deriving Show
-}

data MemCell = MemCell
type Heap = IntMap MemCell

data Env = 
  Env {
    heap :: Heap
  , proc :: Functions
  }

-- Map Pid Functions
type Functions = IntMap Function 

data Function = F
{-
-- Not sure what a process needs yet.
-- | Calling context [(Stack, Function)]
data Function = 
  Func {
    stack :: Stack
  , code :: Graph 
  , pos  :: Int
  }  
-}

type Stack = IntMap MemCell 

type EnvState = State Env

-- The purpose of the interpreter 
-- is to load the program and put
-- it in a state where one can
-- interpret functions with arguments
interpreter :: FrontEnd () -> ()
interpreter = undefined

-- Given the FrontEnd
-- computes an environment/configuration
-- with the initial declarations
i_env :: FrontEnd () -> Env
i_env f@FrontEnd{..} = undefined
