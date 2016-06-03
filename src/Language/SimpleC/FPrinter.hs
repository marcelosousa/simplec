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
  let pp_decls = foldr ppDecl "" decls
      pp_defs  = foldr ppDefs "" defs
  in pp_decls ++ pp_defs

ppDecl :: (Show ident, Show a) => Declaration ident a -> String -> String
ppDecl d s = show d ++ "\n" ++ s

ppDefs :: (Show ident, Show a) => FunctionDef ident a -> String -> String
ppDefs fun cont = ppFun fun ++ "\n" ++ cont
 
ppFun :: (Show ident, Show a) =>  FunctionDef ident a -> String
ppFun fun@FunDef{..} =
  let pp_retty = show ret_ty
      pp_sym   = show symbol
      pp_params = show params
      pp_body = show body
  in pp_retty ++ " " ++ pp_sym ++ pp_params ++ "{\n" ++ pp_body ++ "\n}\n"
