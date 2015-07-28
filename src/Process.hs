{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Process
where

import qualified Data.Aeson       as A

import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text
import           Data.Text           (Text)
import qualified Data.Text.Encoding as Text

import           Pipes
import qualified Pipes.Prelude    as P
import           ParseHoogle      (hoogleLine)
import           Control.Monad
import           Text.Parsec

import qualified ProcessLine      as PL
import           Control.Monad.State.Strict

import qualified Text.Show.Pretty as PP

import qualified Hayoo.FunctionInfo (Score)

import qualified FctIndexerCore   as FJ
import           Data.Time        (UTCTime, getCurrentTime)

import           JsonUtil         (jsonPutStr)

import qualified Data.ByteString.Char8 as BS

-- A producer which yields the lines of a file (as Text)
-- The entire file is read strictly, so there shouldn't be any resource cleanup issues.
-- XXX - make sure UTF-8 is used here.
textLines path = do
  lns <- liftIO $ fmap Text.lines (Text.readFile path)
  forM_ (zip [(1::Int)..] lns) $ yield

byteLines path = do
  lns <- liftIO $ fmap BS.lines (BS.readFile path)
  forM_ (zip [(1::Int)..] lns) $ yield

-- skip all lines before @package because they might not be well-formed UTF-8.
skipToPackage = do
  (i,x) <- await
  if (BS.isPrefixOf "@package" x)
    then do yield (i, Text.decodeUtf8 x)
            forever $ do { (i,x) <- await; yield (i, Text.decodeUtf8 x) }
    else skipToPackage

skippedHeader path = byteLines path >-> skipToPackage

toHoogleLine = forever $ do
  (lineno, txt)  <- await
  case parse hoogleLine "(file)" (Text.unpack txt) of
    Left e      -> liftIO $ do putStrLn $ "error on line " ++ show lineno ++ ": " ++ show e
    Right hline -> yield hline

-- Convert a HoogleLine to a (String, FunctionInfo) pair
toFunctionInfo = forever $ do
  hline <- await
  PL.processLine yield hline

-- Pretty-print each eleement in a stream
ppShowPipe = forever $ do { x <- await; liftIO $ putStrLn (PP.ppShow x) }

-- Emit a (name, fctInfo) to JSON commands
toCommands pkgName score now  = forever $ do
  item@(fctName, fctInfo) <- await
  let cmds = FJ.buildInserts score now [ item ]
  liftIO $ forM_ cmds $ jsonPutStr True

-- Run a MonadState HState pipeline
evalHState pipeline = evalStateT (runEffect pipeline) PL.emptyHState

test1pipe path = skippedHeader path >-> toHoogleLine >-> toFunctionInfo >-> P.drain
test2pipe path = skippedHeader path >-> toHoogleLine >-> toFunctionInfo >-> ppShowPipe
test1 = evalHState . test1pipe
test2 = evalHState . test2pipe
test3 path = do
  now <- getCurrentTime
  evalHState $ skippedHeader path >-> toHoogleLine >-> toFunctionInfo
                           >-> toCommands "foo" 1.0 now

