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

import Data.Char (isSpace,ord,isAlphaNum)

data HState = HState { h_moduleName :: String    -- current module
                     , h_package    :: String    -- current package
                     , h_comments   :: [String]  -- comment lines preceding a definition
                     , h_uriPrefix  :: String    -- current uri prefix
                     }

emptyHState = HState "" "" [] ""

fixupComments :: [String] -> String
fixupComments xs = unlines $ map go xs
  where go x = dropWhile (\ch -> isSpace ch || ch == '|') x

makeFunctionInfo kind name signature uriSuffix = do
  hs <- get
  let comments = fixupComments . reverse . h_comments $ hs
      uri = h_uriPrefix hs ++ uriSuffix
      fi = mkFunctionInfo (h_moduleName hs) signature (h_package hs) uri comments kind
  clearComments
  return fi

emitFunctionInfo kind name signature uri = do
  fi <- makeFunctionInfo kind name signature uri
  liftIO $ putStrLn $ ppShow (name, fi)

toUri :: String -> String
toUri name = concatMap go name
  where go ch | isAlphaNum ch = [ch]
              | otherwise     = "-" ++ show (ord ch) ++ "-"

typeUri name = "#t:" ++ toUri name
funcUri name = "#v:" ++ toUri name

processLine BlankLine    = return ()
processLine (Comment s)  = addComment s
processLine (Package s)  = setPackage s
processLine (Version s)  = return ()
processLine (Module s)   = do
  setModuleName s
  hs <- get
  let prefix = "http://hackage.haskell.org/package/" ++ (h_package hs) ++ "/docs/" ++ moduleDashed ++ ".html"
      moduleDashed = map (replaceDot '-') (h_moduleName hs)
        where replaceDot ch '.' = ch
              replaceDot _  x   = x
  put $ hs { h_uriPrefix = prefix }
  emitFunctionInfo "module" s "" "#"

processLine (Instance s) = return ()

processLine (Type name lhs sig)     = emitFunctionInfo "type"     name sig (typeUri name)
processLine (Newtype name _)        = emitFunctionInfo "newtype"  name ""  (typeUri name)
processLine (FunctionDecl name sig) = emitFunctionInfo "function" name sig (funcUri name)
processLine (DataDecl name)         = emitFunctionInfo "data"     name ""  (typeUri name)
processLine (MultiDecl names sig)   = forM_ names $ \name -> emitFunctionInfo "function" name sig (funcUri name)

processLine _           = return ()

addComment s = modify (\hs -> hs { h_comments = (s:(h_comments hs)) } )
setPackage s = modify (\hs -> hs { h_package = s })
setModuleName s  = modify (\hs -> hs { h_moduleName = s })
clearComments = modify (\hs -> hs { h_comments = [] })

processFile path lines startLN =
  forM_ (zip [startLN ..] lines) $ \(i,ln) -> do
    let source = path ++ " line " ++ show i
        ln' = Text.unpack $ (decodeUtf8 ln) `Text.snoc` '\n'
    case parse Lib.anyLine source ln' of
      Left e  -> return ()
      Right x -> processLine x

doit path = do
  allLines <- fmap LBS.lines $ LBS.readFile path
  let (ignored, body) = break (LBS.isPrefixOf (LBS.pack "@package")) allLines
      i0 = 1+length ignored
  runStateT (processFile path body i0) emptyHState

-- doit = runStateT (processFile "asd") emptyHState

