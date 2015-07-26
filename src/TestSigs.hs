module TestSigs
where

import Hayoo.ParseSignature
import Text.Parsec
import Text.Show.Pretty

test1 x = case parseSignature x of
            Left e -> putStrLn $ "error: " ++ show e
            Right s -> putStrLn $ ppShow s

-- filename -> producer of Hoogle lines

{-
hoogleLines path = do
  lns <- fmap LBS.lines $ LBS.readFile path
  forM_ lns $ do
    case parse parser source ln' of  
-}
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
