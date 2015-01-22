module Main where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident

import qualified Language.SimpleC.AST as SC
import Language.SimpleC.Converter
import Language.SimpleC.Printer
import Language.SimpleC.Flow

import System.FilePath
import System.Directory
import Test.HUnit

main :: IO ()
main = putStrLn "simple-c: a library for simple c"

parseFile :: FilePath -> IO CTranslUnit
parseFile f  =
  do parse_result <- parseCFile (newGCC "gcc") Nothing [] f
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

pp :: FilePath -> IO ()
pp f = do ctu <- parseFile f
          print $ translate ctu

printMyAST :: CTranslUnit -> IO ()
printMyAST ctu = print ctu -- (print . pretty) ctu

myMain fn f = parseFile fn >>= f

success :: SC.Program -> Bool
success (SC.Program _) = True

-- svcomptest :: FilePath -> IO ()
svcomptest = do
  fs <- getDirectoryContents "/Users/mabs/Research/tools/simplec/tests/svcomp/pthread/"
  let fs' = filter (\f -> takeExtension f == ".c") fs
      cfs = map (\f -> "/Users/mabs/Research/tools/simplec/tests/svcomp/pthread/" ++ f) fs'
  tests <- mapM (\f -> do parseFile f >>= \ctu -> return $ translate ctu) cfs
  print tests
--  tests <- mapM (\f -> do parseFile f >>= \ctu -> let p = translate ctu in return $ TestCase $ assertBool f (success p)) cfs
--  runTestTT $ TestList tests

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
