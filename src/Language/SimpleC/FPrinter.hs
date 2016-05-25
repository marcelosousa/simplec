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
  foldl printDecl "" decls

printDecl :: (Show ident, Show a) => String -> SC.Declaration ident a -> String
printDecl s d = show d ++ "\n" ++ s 

