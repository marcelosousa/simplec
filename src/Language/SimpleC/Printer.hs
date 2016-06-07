{-#LANGUAGE RecordWildCards #-}

module Language.SimpleC.Printer where

import qualified Data.Map as M

import Language.SimpleC.AST
import Language.SimpleC.Flow

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

pp_dot_graphs :: (Show ident, Show a) => Graphs ident a -> String
pp_dot_graphs graphs =
  let n_e_s = "digraph program {" 
      n_x_s = "}"
      prog_s = M.fold pp_dot_graph "" graphs
  in n_e_s ++ "\n" ++ prog_s ++ n_x_s
  where
pp_dot_graph gr@Graph{..} rest =
  let g = M.foldWithKey (pp_dot_edges edge_table) "" graph
  in g ++ "\n" ++ rest
pp_dot_edges table pre succs rest =
  let succs' = foldr  (pp_dot_edge table pre) "" succs 
  in succs' ++ "\n" ++ rest
pp_dot_edge table pre (eId,succ) rest =
  let e_label = case M.lookup eId table of
        Nothing -> ""
        Just info -> show info
      e = show pre ++ " -> " ++ show succ ++ " [label=< " ++ e_label ++ " >]"
  in e ++ "\n" ++ rest
