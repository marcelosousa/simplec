module Language.SimpleC where
    
import qualified Data.Map as M
import Data.Map (Map)

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident

import qualified Language.SimpleC.AST as SC
import Language.SimpleC.Converter
import qualified Language.SimpleC.SConverter as SCon
import Language.SimpleC.Printer
--import Language.SimpleC.Flow

parseFile :: FilePath -> IO CTranslUnit
parseFile f  =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] f
     case parse_result of
       Left parse_err -> do 
           parse_result <- parseCFilePre f
           case parse_result of
               Left _ -> error (show parse_err)
               Right ast -> return ast
       Right ast      -> return ast

extract :: FilePath -> IO SC.Program
extract f = do ctu <- parseFile f
--               print $ ctu
               return $ translate ctu

test :: FilePath -> IO ()
test f = do ctu <- parseFile f
            print $ SCon.translate ctu 

success :: SC.Program -> Bool
success (SC.Program _) = True

