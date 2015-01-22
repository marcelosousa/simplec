module Language.SimpleC where
    
import qualified Data.Map as M
import Data.Map (Map)

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident

import qualified Language.SimpleC.AST as SC
import Language.SimpleC.Converter
import Language.SimpleC.Printer
import Language.SimpleC.Flow

parseMyFile :: FilePath -> IO CTranslUnit
parseMyFile input_file =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

printMyAST :: CTranslUnit -> IO ()
printMyAST ctu = print ctu -- (print . pretty) ctu

ppProg :: CTranslUnit -> IO ()
ppProg ctu = print $ translate ctu

myMain fn f = parseMyFile fn >>= f

{-
mainMerge :: FilePath -> FilePath -> FilePath -> IO ()
mainMerge base v1 v2 = do
  abase <- parseMyFile base
  av1   <- parseMyFile v1
  av2   <- parseMyFile v2
  let cbase = translate abase
      cv1 = translate av1
      cv2 = translate av2
  case mergeProg cbase cv1 cv2 of
    Nothing -> error "failed to merge"
    Just cmerge -> print cmerge
-}

