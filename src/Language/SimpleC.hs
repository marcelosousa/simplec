module Language.SimpleC where
    
import qualified Data.Map as M
import Data.Map (Map)

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident

import qualified Language.SimpleC.AST as SC
import Language.SimpleC.Converter
import Language.SimpleC.Printer
import Language.SimpleC.Flow

