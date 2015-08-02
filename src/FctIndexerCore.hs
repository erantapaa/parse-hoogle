{-# LANGUAGE OverloadedStrings #-}

module FctIndexerCore
where

import qualified Data.Aeson                   as A
import qualified Data.Vector                  as V
import qualified Data.Text                    as T
import           Data.Text                    (Text)

import           Hayoo.FunctionInfo           (FunctionInfo(..), fromFct'Type, Score)
import           Hayoo.ParseSignature         (prettySignature, complexSignatures,
                                               subSignatures, parseSignature)
import           ProcessLine                  (fixupSignature)
import           ParseHoogle                  (removeTags)

import           Data.Time                    (UTCTime)
import           Data.Time.Format             (formatTime)
import           System.Locale                (defaultTimeLocale)
import           Data.Scientific              as S
import           Data.List                    (intercalate)

fmtDateXmlSchema :: UTCTime -> String
fmtDateXmlSchema = fmtDate' "%FT%X"

fmtDateHTTP :: UTCTime -> String
fmtDateHTTP = fmtDate' "%a %b %e %H:%M:%S %Z %Y"

fmtDate' :: String -> UTCTime -> String
fmtDate' fmt
    = formatTime defaultTimeLocale fmt

pair :: Text -> String -> (Text, A.Value)
pair k v = (k, A.String (T.pack v))

optPair :: Text -> String -> [ (Text, A.Value) ]
optPair k [] = []
optPair k v  = [ pair k v ]

type APair = (Text, A.Value)

-- | Build the index for a FunctionInfo
buildIndexPairs :: String                       -- ^ Name of function / type / module / etc.
                -> FunctionInfo                 -- ^ FunctionInfo record
                -> [(Text, A.Value)]            -- ^ Pairs comprising the index for this document
buildIndexPairs fctName fctInfo = kvpairs
  where
    kvpairs =
      [ pair "package"     (package fctInfo)
      , pair "module"      mname
      , pair "name"        fctName
      , pair "type"        infoType
      , pair "hierarchy"   hierarchy
      ]
      ++ optPair "description" descrWords
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
                       , pair "subsig"    (toSubSignatures sig)
                       ]
    descrWords = removeTags (fctDescr fctInfo)

-- | Build the document component of an Insert command.
buildDocument :: [APair]         -- ^ Score key-value pair for this package (or null)
              -> UTCTime         -- ^ The indexed time
              -> String          -- ^ The function / method / type name
              -> FunctionInfo    -- ^ The FunctionInfo record
              -> A.Value         -- ^ Document object (as JSON)
buildDocument scoreKV now fctName fctInfo =
  A.object  $
  [ ("description",    A.object $
                       [ ("indexed",        nowD)
                       , pair "package"     (package fctInfo)
                       , pair "module"      (moduleName fctInfo)
                       , pair "name"        fctName
                       , pair "type"        infoType
                       , pair "source"      (sourceURI fctInfo)
                       ]
                       ++ optPair "description" (fctDescr fctInfo)
    )
  , ("index",          index)
  , pair "uri"         (docURI fctInfo)
  ]
  ++ scoreKV
  where
    nowD     = A.String $ T.pack $ fmtDateHTTP now       -- date formatted for the document
    nowI     = A.String $ T.pack $ fmtDateXmlSchema now  -- date formatted for the index
    infoType = fromFct'Type (fctType fctInfo)
    index    = A.object $ [ ("indexed", nowI) ] ++ buildIndexPairs fctName fctInfo

buildInsert :: [APair]          -- ^ Score Pair (or null) for this package
            -> UTCTime          -- ^ The index time
            -> String           -- ^ The function / method / type name
            -> FunctionInfo     -- ^ The FunctionInfo record
            -> A.Value          -- ^ Insert command (as JSON)
buildInsert scoreKV now fctName fctInfo =
  A.object
  [ ("cmd",      "insert")
  , ("document", buildDocument scoreKV now fctName fctInfo)
  ]
    
-- | Build the Insert commands for a list of FunctionInfo records.
buildInserts :: Maybe Score -> UTCTime -> [ (String, FunctionInfo) ] -> [A.Value]
buildInserts score now items = [ buildInsert scoreKV now fctName fctInfo | (fctName, fctInfo) <- items ]
  where
    scoreKV = maybe []  (\s -> [ ("weight", A.Number $ S.fromFloatDigits s) ] ) score

-- | Build the Delete command for a package.
buildDelete :: String -> A.Value
buildDelete pkgName =
  A.object
  [ ("cmd",   "delete-by-query")
  , ("query", A.object [ ("contexts", A.Array $ V.fromList [ A.String "package" ] )
                       , ("type",     "context")
                       , ("query",    A.object [ ("op",   "case")
                                               , ("type", "fullword")
                                               , ("word",  (A.String $ T.pack pkgName))
                                               ]
                         )
                       ]
    )
  ]

buildNOOP :: A.Value
buildNOOP = A.object [ ("cmd", "noop") ]

-- signature stuff

toSubSignatures :: String -> String
toSubSignatures str =
  case parseSignature (fixupSignature str) of
    Left _    -> ""
    Right sig -> intercalate "\n" $ map prettySignature $ complexSignatures 1 $ subSignatures sig

