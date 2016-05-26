{-#LANGUAGE RecordWildCards #-}

module Language.SimpleC.FPrinter where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident
import qualified Language.SimpleC.FAST as SC
import qualified Data.Map as M
import Data.Map (Map)

printProg :: (Show ident, Show a) => SC.Program ident a -> String
printProg p@SC.Prog{..} = 
  foldr printDecl "" decls

printDecl :: (Show ident, Show a) => SC.Declaration ident a -> String -> String
printDecl d s = show d ++ "\n" ++ s 

