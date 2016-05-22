{-#LANGUAGE RecordWildCards #-}

module Language.SimpleC.FPrinter where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident
import qualified Language.SimpleC.FAST as SC
import qualified Data.Map as M
import Data.Map (Map)

printProg :: SC.Program -> String
printProg p@SC.Prog{..} = 
  foldl printDecl "" decls

printDecl :: String -> SC.Declaration -> String
printDecl s d = show d ++ "\n" ++ s 

