{-# LANGUAGE NoMonomorphismRestriction #-}

module Process
where

import Text.Parsec (parse)
import qualified Lib
import           Lib (HoogleLine(..))
import Control.Monad.State.Strict

import Data.Char
import Data.List (isPrefixOf, break)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy as LBS hiding (readFile, pack, snoc)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (decodeUtf8)

import Hayoo.FunctionInfo
import Text.Show.Pretty (ppShow)

import Data.Char (isSpace)

data HState = HState { h_moduleName :: String    -- current module
                     , h_package    :: String    -- current package
                     , h_comments   :: [String]  -- comment lines preceding a definition
                     }

emptyHState = HState "" "" []

processFile path lines startLN =
  forM_ (zip [startLN ..] lines) $ \(i,ln) -> do
    let source = path ++ " line " ++ show i
        ln' = Text.unpack $ (decodeUtf8 ln) `Text.snoc` '\n'
    case parse Lib.anyLine source ln' of
      Left e  -> return ()
      Right x -> processLine x

fixupComments :: [String] -> String
fixupComments xs = unlines $ map go xs
  where go x = dropWhile (\ch -> isSpace ch || ch == '|') x

makeFunctionInfo kind name signature = do
  hs <- get
  let comments = fixupComments . reverse . h_comments $ hs
      fi = mkFunctionInfo (h_moduleName hs) signature (h_package hs) "" comments kind
  clearComments
  return fi

emitFunctionInfo kind name signature = do
  fi <- makeFunctionInfo kind name signature
  liftIO $ putStrLn $ ppShow (name, fi)

processLine BlankLine    = return ()
processLine (Comment s)  = addComment s
processLine (Package s)  = setPackage s
processLine (Version s)  = return ()
processLine (Module s)   = setModuleName s
processLine (Instance s) = return ()

processLine (Type name lhs sig)     = emitFunctionInfo "type" name sig
processLine (Newtype name _)        = emitFunctionInfo "newtype" name ""
processLine (FunctionDecl name sig) = emitFunctionInfo "function" name sig
processLine (DataDecl name)         = emitFunctionInfo "data" name ""

processLine _           = return ()

addComment s = modify (\hs -> hs { h_comments = (s:(h_comments hs)) } )
setPackage s = modify (\hs -> hs { h_package = s })
setModuleName s  = modify (\hs -> hs { h_moduleName = s })
clearComments = modify (\hs -> hs { h_comments = [] })

doit path = do
  allLines <- fmap LBS.lines $ LBS.readFile path
  let (ignored, body) = break (LBS.isPrefixOf (LBS.pack "@package")) allLines
      i0 = 1+length ignored
  runStateT (processFile path body i0) emptyHState

-- doit = runStateT (processFile "asd") emptyHState

