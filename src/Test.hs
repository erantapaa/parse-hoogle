{-# LANGUAGE NoMonomorphismRestriction #-}

module Test
where

import Control.Monad
import Pipes
import Text.Show.Pretty
import Process

import           Text.Parsec
import           ParseHoogle          (HoogleLine(..), hoogleLine)
import           Hayoo.ParseSignature
import qualified ProcessLine
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import           Control.Monad.State.Strict

-- emit the FunctionInfo records in a Hoogle file
testFunctionInfo path = evalHState $ textLines path >-> toHoogleLine >-> toFunctionInfo >-> ppShowPipe

-- run parseSignature on the function declarations in a file
-- e.g. testParseSignatures "in-funcions"
testParseSignature path = runEffect $ (textLines path >-> toHoogleLine) `for` (lift . checkSig)

-- Run parseSignature against a function signature
checkSig (FunctionDecl name sig) = do
  let sig' = filter (/= '!') sig -- remove bang annotations
  case parseSignature sig' of
    Left e -> do putStrLn $ "error parsing signature: " ++ show e
                 putStrLn $ "  - name: " ++ name
                 putStrLn $ "  - sig : " ++ sig'
                 putStrLn ""
    Right s -> return ()
checkSig _ = return ()

-- check a line against a parser
checkLine path parser (i,x) = do
  let source = path ++ " line " ++ show i
      str = T.unpack x
  case parse parser source str of
    Left e  -> do putStrLn $ "error: " ++ show e
                  T.putStr x
                  putStrLn ""
    Right _ -> return ()

-- test a parser against the lines in a file - do not skip preamble
testFile' parser path = do
  runEffect $ textLines path >-> forever (await >>= liftIO . checkLine path parser)

-- test a parser against the lines in a file - skip lines before @package
testFile parser path = 
  runEffect $ textLines path >-> skipHeader >-> forever (await >>= liftIO . checkLine path parser)

-- test the anyLine parse against all of the hoogle files
testAllFiles = do
  files <- fmap lines $ readFile "all-hoogle-files"
  mapM_ (testFile hoogleLine) files

