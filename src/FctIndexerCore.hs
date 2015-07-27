{-# LANGUAGE OverloadedStrings #-}

module FctIndexerCore
where

-- import Debug.Trace

import qualified Data.Aeson                   as A
import qualified Data.Vector                  as V
import           Hayoo.FunctionInfo           (FunctionInfo(..), fromFct'Type)
import qualified Data.Text                    as T
import           Data.Text                    (Text)
import           Data.Time                    (UTCTime)
import           Data.Time.Format             (formatTime)
import           System.Locale                (defaultTimeLocale)

fmtDateXmlSchema :: UTCTime -> String
fmtDateXmlSchema = fmtDate' "%FT%X"

fmtDateHTTP :: UTCTime -> String
fmtDateHTTP = fmtDate' "%a %b %e %H:%M:%S %Z %Y"

fmtDate' :: String -> UTCTime -> String
fmtDate' fmt
    = formatTime defaultTimeLocale fmt

pair :: Text -> String -> (Text, A.Value)
pair k v = (k, A.String (T.pack v))

-- build the index for a FunctionInfo
buildIndex :: String -> FunctionInfo -> [(Text, A.Value)]
buildIndex fctName fctInfo = kvpairs
  where
    kvpairs =
      [ pair "package"   (package fctInfo)
      , pair "module"    mname
      , pair "name"      fctName
      , pair "type"      infoType
      , pair "hierarchy" hierarchy
      , ("description",  descrWords)
      ]
      ++ sigPairs

    infoType = fromFct'Type (fctType fctInfo)

    mname = moduleName fctInfo

    hierarchy = map replaceDotSpace mname
      where
        replaceDotSpace '.' = ' '
        replaceDotSpace x   = x

    sigPairs =
      let sig = signature fctInfo in
      if null sig then []
                  else [ pair "signature" sig
                       , pair "subsig"    ""
                       ]
    descrWords = "" -- XXX need to figure out how to remove puncuation and HTML tags from a string

 -- description:    words contained in the fctInfo description
 -- hierarchy:      module name with dots removed
 -- indexed:        time indexed
 -- package:        fctInfo package
 -- module:         fctInfo moduleName
 -- name:           fctName
 -- type:           fctInfo type
 -- signature:      fctInfo signature
 -- subsig:         derived from signature

buildDocument :: UTCTime -> String -> FunctionInfo -> A.Value
buildDocument now fctName fctInfo =
  A.object 
  [ pair "indexed"     now'
  , pair "package"     (package fctInfo)
  , pair "module"      (moduleName fctInfo)
  , pair "name"        fctName
  , pair "type"        infoType
  , pair "description" (fctDescr fctInfo)
  , pair "source"      ""
  , ("index",          index)
{-
  , pair "index"  index
  , pair "uri"    uri
  , pair "weight" weight
-}
  ]
  where
    now'  = fmtDateXmlSchema now
    nowA  = A.String $ T.pack now'
    infoType = fromFct'Type (fctType fctInfo)
    index = A.object $ [ ("indexed", nowA) ] ++ buildIndex fctName fctInfo

buildInsert :: UTCTime -> String -> FunctionInfo -> A.Value
buildInsert now fctName fctInfo =
  A.object
  [ ("cmd",      "insert")
  , ("document", buildDocument now fctName fctInfo)
  ]

buildInserts :: UTCTime -> [ (String, FunctionInfo) ] -> [A.Value]
buildInserts now pairs = insertCmds
  where
    insertCmds = [ buildInsert now fctName fctInfo | (fctName, fctInfo) <- pairs ]

-- generate the delete command for a package
deleteCommand :: String -> A.Value
deleteCommand pkgName =
  A.object
  [ ("cmd",   "delete-by-query")
  , ("query", A.object [ ("contexts", A.Array $ V.fromList [ A.String "package" ] )
                       , ("type",     "context")
                       , ("query",    A.object [ ("op",   "case")
                                               , ("type", "fullword")
                                               , ("word",  (A.String $ T.pack $ pkgName))
                                               ]
                         )
                       ]
    )
  ]

