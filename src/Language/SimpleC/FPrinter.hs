{-#LANGUAGE RecordWildCards #-}

module Language.SimpleC.FPrinter where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident
import Language.SimpleC.FAST
import qualified Data.Map as M
import Data.Map (Map)

ppProg :: (Show ident, Show a) => Program ident a -> String
ppProg p@Prog{..} = 
  foldr ppDecl "" decls

ppDecl :: (Show ident, Show a) => Declaration ident a -> String -> String
ppDecl d s = show d ++ "\n" ++ s

ppFun :: (Show ident, Show a) =>  FunctionDef ident a -> String -> String
ppFun = undefined
