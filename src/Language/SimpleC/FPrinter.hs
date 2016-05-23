{-#LANGUAGE RecordWildCards #-}

module Language.SimpleC.FPrinter where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident
import qualified Language.SimpleC.FAST as SC
import qualified Data.Map as M
import Data.Map (Map)

printProg :: (Show a) => SC.Program a -> String
printProg p@SC.Prog{..} = 
  foldl printDecl "" decls

printDecl :: (Show a) => String -> SC.Declaration a -> String
printDecl s d = show d ++ "\n" ++ s 

