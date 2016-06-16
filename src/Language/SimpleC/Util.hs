module Language.SimpleC.Util where

import Data.Map (Map)
import Language.SimpleC.AST 
import Language.SimpleC.Converter
import Language.SimpleC.Flow

-- The result of the front-end
--  AST of the file
--  Control Flow Graphs per Function
--  Symbol Table
data FrontEnd node 
 = FrontEnd 
 { 
   ast  :: Program SymId node
 , cfgs :: Graphs SymId  node
 , symt :: Map SymId Symbol
 } deriving Show
