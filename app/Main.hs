module Main where

import qualified ParseHoogle as PH
import qualified Test
import System.Environment
import Data.List
import System.Exit

data Action = Error String
            | Usage
            | TestHoogleAll
            | TestHoogle String
            | EmitFunctionInfo String
            | TestSigParser String
            | EmitJson String
  deriving (Show)

verbs = [ "test-hoogle-parser", "emit-function-info", "test-sig-parser", "emit-json" ]

complete :: [String] -> String -> String
complete words arg =
  case matches of
    [w] -> w
    _   -> arg
  where
    matches = [ w | w <- words, arg `isPrefixOf` w ]

parseOpts :: [String] -> Action
parseOpts (arg1:args) = go cmd args
  where
    cmd = complete verbs arg1
    go "test-hoogle-parser" ("--all":_) = TestHoogleAll
    go "test-hoogle-parser" (path:_)    = TestHoogle path
    go "test-hoogle-parser" _           = Error "path required for 'test-hoogle-parser'"
    go "emit-function-info" (path:_)    = EmitFunctionInfo path
    go "emit-function-info" _           = Error "path required for 'emit-function-info'"
    go "test-sig-parser"    (path:_)    = TestSigParser path
    go "test-sig-parser"    _           = Error "path required for 'test-sig-parser'"
    go "emit-json"          (path:_)    = EmitJson path
    go "emit-json"          _           = Error "path requried for 'emit-json'"
    go cmd                  _           = Error $ "unknown command: " ++ cmd
parseOpts [] = Usage

usage name = do
  putStr $ unlines
     [ "Usage:"
     , "  app test-hoogle-parser --all  -- test the hoogle parser on all files"
     , "  app test-hoogle-parser file   -- test the hoogle parser on a specific file"
     , "  app emit-function-info file   -- emit the FunctionInfo records for a hoogle file"
     , "  app test-sig-parser file      -- test the signature parser on a hoogle file"
     , "  app emit-json file            -- emit json commands for a hoogle file"
     ]

main :: IO ()
main = do
  args <- getArgs
  let action = parseOpts args
  case action of
    Usage                 -> usage "app"
    Error e               -> do putStrLn $ "error: " ++ e; exitFailure
    TestHoogleAll         -> Test.testAllFiles
    TestHoogle path       -> Test.testFile PH.hoogleLine path
    EmitFunctionInfo path -> Test.testFunctionInfo path
    TestSigParser path    -> Test.testParseSignature path
    EmitJson path         -> Test.testJson path
  exitSuccess
    

