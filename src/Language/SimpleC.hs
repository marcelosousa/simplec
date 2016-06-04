module Language.SimpleC where
    
import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident

import Language.SimpleC.AST 
import Language.SimpleC.Converter
import Language.SimpleC.Printer
-- import Language.SimpleC.Flow

parseFile :: FilePath -> IO CTranslUnit
parseFile f  =
  do parse_result <- parseCFile (newGCC "gcc") Nothing ["-I/home/msousa/benchmarks/musketeer/debian/packages/coreutils-8.21/lib/","-I/usr/include/linux/"] f
     case parse_result of
       Left parse_err -> do 
           parse_result <- parseCFilePre f
           case parse_result of
               Left _ -> error (show parse_err)
               Right ast -> return ast
       Right ast      -> return ast

--extract :: FilePath -> IO SC.Program
--extract f = do ctu <- parseFile f
----               print $ ctu
--               return $ translate ctu

--test :: FilePath -> IO a
test f = do ctu <- parseFile f
            let sctu = fmap (\_ -> ()) ctu            
                st = processor sctu 
           -- print sctu
           -- putStrLn ""
            putStrLn $ ppProg $ code st
           -- print $ FCon.syms st
