{-# LANGUAGE OverloadedStrings     #-}

module JsonUtil
where

import           Data.Aeson                 (ToJSON, encode)
import           Data.Aeson.Encode.Pretty   (Config(..), encodePretty', keyOrder)
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.Monoid                (Monoid(..))

jsonOutput :: (ToJSON c) => Bool -> (LB.ByteString -> IO a) -> c -> IO a
jsonOutput pretty io x
    = io $ (if pretty then encodePretty' encConfig else encode) x
      where
        encConfig :: Config
        encConfig
            = Config { confIndent = 2
                     , confCompare
                         = keyOrder ["description", "index", "uri"]
                           `mappend`
                           compare
                     }

jsonPutStr :: (ToJSON c) => Bool -> c -> IO ()
jsonPutStr pretty c = jsonOutput pretty LC.putStrLn c

hJsonPutStr pretty fh c = jsonOutput pretty (LC.hPutStrLn fh) c

