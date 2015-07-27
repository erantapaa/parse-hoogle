{-# LANGUAGE NoMonomorphismRestriction #-}

module Test
where

import Control.Monad

import Text.Parsec
import Lib (HoogleLine(..), hoogleLine)
import Hayoo.ParseSignature

import Data.Char
import Data.List (isPrefixOf, break)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy as LBS hiding (readFile, pack, snoc)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (decodeUtf8)

import Pipes
import Text.Show.Pretty

import qualified Process
import qualified Lib
import Control.Monad.State.Strict

-- -----

processFile path lines startLN =
  forM_ (zip [startLN ..] lines) $ \(i,ln) -> do
    let source = path ++ " line " ++ show i
        ln' = Text.unpack $ (decodeUtf8 ln)
    case parse Lib.hoogleLine source ln' of
      Left e  -> return ()
      Right x -> Process.processLine x

testFunctionInfo path = do
  allLines <- fmap LBS.lines $ LBS.readFile path
  let (ignored, body) = break (LBS.isPrefixOf (LBS.pack "@package")) allLines
      i0 = 1+length ignored
  runStateT (processFile path body i0) Process.emptyHState

-- -----

hoogleLines path = do
  lns <- liftIO $ fmap LBS.lines $ LBS.readFile path
  forM_ lns $ \ln -> do
    let ln' = Text.unpack $ (decodeUtf8 ln)
    case parse hoogleLine "<line>" ln' of
      Left e  -> lift $ putStrLn $ "error: " ++ show e
      Right h -> yield h

-- run parseSignature on the function declarations in a file
-- e.g. testParseSignatures "in-funcions"
testParseSignature path = runEffect $ for (hoogleLines path) doit
  where
    doit h = do
      case h of
        FunctionDecl name sig' -> do
          let sig = filter (/= '!') sig' -- remove bang annotations
          case parseSignature sig of
            Left e -> lift $ do putStrLn $ "error parsing signature: " ++ show e
                                putStrLn $ "  - name: " ++ name
                                putStrLn $ "  - sig : " ++ sig
                                putStrLn ""
            Right s -> return ()
        _                     -> return ()

-- test a parser against the lines in a file - do not skip preamble
testFile' parser path = do
  lns <- fmap LBS.lines $ LBS.readFile path
  forM_ (zip [(1::Int)..] lns) $ \(i,ln) -> do
    let source = path ++ " line " ++ show i
        ln' = Text.unpack $ (decodeUtf8 ln)
    case parse parser source ln' of
      Left e  -> -- do putStrLn $ "error: " ++ show e; putStr ln'
                 do putStr ln'
      Right x -> return ()

-- test a parser against the lines in a file - skip lines before @package
testFile parser path = do
  lns <- fmap LBS.lines $ LBS.readFile path
  -- Lines before @package may not be properly UTF-8 encoded
  -- so ignore them.
  let (ignored, lns') = break (LBS.isPrefixOf (LBS.pack "@package")) lns
      i0 = 1+length ignored
  forM_ (zip [(i0::Int)..] lns') $ \(i,ln) -> do
    let source = path ++ " line " ++ show i
        ln' = Text.unpack $ (decodeUtf8 ln)
    case parse parser source ln' of
      Left e  -> do putStrLn $ "error: " ++ show e
                    putStr ln'
                    putStrLn ""
      Right x -> return ()

-- test the anyLine parse against all of the hoogle files
testAllFiles = do
  files <- fmap lines $ readFile "all-hoogle-files"
  mapM_ (testFile hoogleLine) files

