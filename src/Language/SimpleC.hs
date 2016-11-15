module Language.SimpleC (extract, FrontEnd(..)) where

import Data.Map 
 
import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident

import Language.SimpleC.AST 
import Language.SimpleC.Converter
import Language.SimpleC.Flow
import Language.SimpleC.Interpreter
import Language.SimpleC.Printer
import Language.SimpleC.Util

import System.FilePath.Posix

dParseFile = parseFile "-I/home/msousa/benchmarks/musketeer/debian/packages/coreutils-8.21/lib/"
sParseFile = parseFile ""
parseFile :: String -> FilePath -> IO CTranslUnit
parseFile cOpt f  = do
  parse_result <- 
    if cOpt == ""
    then parseCFile (newGCC "gcc") Nothing [] f
    else parseCFile (newGCC "gcc") Nothing [cOpt] f
  case parse_result of
    Left parse_err -> do 
      parse_result <- parseCFilePre f
      case parse_result of
        Left _ -> error (show parse_err)
        Right ast -> return ast
    Right ast      -> return ast

extract :: String -> FilePath -> IO (FrontEnd () st)
extract cOpt f = do 
  ctu <- parseFile cOpt f
  let s_ctu = fmap (\_ -> ()) ctu -- remove the nodes
      proc_st = processor s_ctu   -- simplify the AST
      ast = code proc_st          -- get the new AST
      sym_table = syms proc_st    -- get the symbol table
      cfgs = computeGraphs ast    -- compute the cfgs
 --  print $ s_ctu 
  return $ FrontEnd ast cfgs sym_table 

test_flow f = do 
  ctu <- sParseFile f
  let s_ctu = fmap (\_ -> ()) ctu -- remove the nodes
      proc_st = processor s_ctu   -- simplify the AST
      ast = code proc_st          -- get the new AST
      cfgs = computeGraphs ast    -- compute the cfgs
      fname = fst $ splitExtension f
      sym_table = syms proc_st    -- get the symbol table
  print $ s_ctu 
  putStrLn $ ppProg ast 
  writeFile (fname ++ ".dot") $ pp_dot_graphs cfgs sym_table 
