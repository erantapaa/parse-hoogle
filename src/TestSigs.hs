{-# LANGUAGE NoMonomorphismRestriction #-}

module TestSigs
where

import Hayoo.ParseSignature
import Text.Parsec
import Text.Show.Pretty

import Lib
import Pipes
import Control.Monad
import qualified Data.ByteString.Lazy as LBS hiding (readFile)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (decodeUtf8)

test1 x = case parseSignature x of
            Left e -> putStrLn $ "error: " ++ show e
            Right s -> putStrLn $ ppShow s

-- filename -> producer of Hoogle lines

hoogleLines path = do
  lns <- liftIO $ fmap LBS.lines $ LBS.readFile path
  forM_ lns $ \ln -> do
    let ln' = Text.unpack $ (decodeUtf8 ln)
    case parse hoogleLine "<line>" ln' of  
      Left e  -> lift $ putStrLn $ "error: " ++ show e
      Right h -> yield h

-- run parseSignature on the function declarations in a file 
testParseSignature = runEffect $ for (hoogleLines "in-functions") doit
  where
    doit h = do
      case h of
        FunctionDecl name sig' -> do
          let sig = filter (/= '!') sig'
          case parseSignature sig of
            Left e -> lift $ do putStrLn $ "error parsing signature: " ++ show e
                                putStrLn $ "  - name: " ++ name
                                putStrLn $ "  - sig : " ++ sig
                                putStrLn ""
            Right s -> return ()
        _                     -> return ()

{-
-- test a parser against the lines in a file - do not skip preamble
testFile' parser path = do
  lns <- fmap LBS.lines $ LBS.readFile path
  forM_ (zip [(1::Int)..] lns) $ \(i,ln) -> do
    let source = path ++ " line " ++ show i
        ln' = Text.unpack $ (decodeUtf8 ln) `Text.snoc` '\n'
    case parse parser source ln' of
      Left e  -> -- do putStrLn $ "error: " ++ show e; putStr ln'
                 do putStr ln'
      Right x -> return ()
-}
