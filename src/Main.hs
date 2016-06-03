module Main where

import Language.C 
import Language.C.System.GCC  -- preprocessor used
import Language.C.Data.Ident

import Language.SimpleC
import Language.SimpleC.Converter

import System.FilePath
import System.Directory

main :: IO ()
main = putStrLn "simple-c: a library for simple c"

printMyAST :: CTranslUnit -> IO ()
printMyAST ctu = print ctu -- (print . pretty) ctu

myMain fn f = parseFile fn >>= f

-- svcomptest :: FilePath -> IO ()
-- svcomptest = do
--   fs <- getDirectoryContents "/Users/mabs/Research/tools/simplec/tests/svcomp/pthread/"
--   let fs' = filter (\f -> takeExtension f == ".c") fs
--       cfs = map (\f -> "/Users/mabs/Research/tools/simplec/tests/svcomp/pthread/" ++ f) fs'
--   tests <- mapM (\f -> do parseFile f >>= \ctu -> return $ translate ctu) cfs
--   print tests
--  tests <- mapM (\f -> do parseFile f >>= \ctu -> let p = translate ctu in return $ TestCase $ assertBool f (success p)) cfs
--  runTestTT $ TestList tests

