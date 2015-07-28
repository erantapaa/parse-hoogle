{-# LANGUAGE OverloadedStrings #-}

module FctIndexerCore
where

import qualified Data.Aeson                   as A
import qualified Data.Vector                  as V
import qualified Data.Text                    as T
import           Data.Text                    (Text)

import           Hayoo.FunctionInfo           (FunctionInfo(..), fromFct'Type, Score)
import           ParseHoogle                  (removeTags)

import           Data.Time                    (UTCTime)
import           Data.Time.Format             (formatTime)
import           System.Locale                (defaultTimeLocale)
import           Data.Scientific              as S

fmtDateXmlSchema :: UTCTime -> String
fmtDateXmlSchema = fmtDate' "%FT%X"

fmtDateHTTP :: UTCTime -> String
fmtDateHTTP = fmtDate' "%a %b %e %H:%M:%S %Z %Y"

fmtDate' :: String -> UTCTime -> String
fmtDate' fmt
    = formatTime defaultTimeLocale fmt

pair :: Text -> String -> (Text, A.Value)
pair k v = (k, A.String (T.pack v))

-- | Build the index for a FunctionInfo
buildIndex :: String                       -- ^ Name of function / type / module / etc.
           -> FunctionInfo                 -- ^ FunctionInfo record
           -> [(Text, A.Value)]            -- ^ Pairs comprising the index for this document
buildIndex fctName fctInfo = kvpairs
  where
    kvpairs =
      [ pair "package"     (package fctInfo)
      , pair "module"      mname
      , pair "name"        fctName
      , pair "type"        infoType
      , pair "hierarchy"   hierarchy
      , pair "description" descrWords
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
    descrWords = removeTags (fctDescr fctInfo)

-- | Build the document component of an Insert command.
buildDocument :: A.Value         -- ^ Score for this package (as a A.Number value)
              -> A.Value         -- ^ The index time as an Aeson value (A.String)
              -> String          -- ^ The function / method / type name
              -> FunctionInfo    -- ^ The FunctionInfo record
              -> A.Value         -- ^ Document object (as JSON)
buildDocument scoreA nowA fctName fctInfo =
  A.object 
  [ ("indexed",        nowA)
  , ("weight",         scoreA)
  , pair "package"     (package fctInfo)
  , pair "module"      (moduleName fctInfo)
  , pair "name"        fctName
  , pair "type"        infoType
  , pair "description" (fctDescr fctInfo)
  , pair "uri"         (docURI fctInfo)
  , ("index",          index)
  ]
  where
    infoType = fromFct'Type (fctType fctInfo)
    index    = A.object $ [ ("indexed", nowA) ] ++ buildIndex fctName fctInfo

buildInsert :: A.Value          -- ^ Score for this package (as a A.Number value)
            -> A.Value          -- ^ The index time (as a A.String value)
            -> String           -- ^ The function / method / type name
            -> FunctionInfo     -- ^ The FunctionInfo record
            -> A.Value          -- ^ Insert command (as JSON)
buildInsert scoreA nowA fctName fctInfo =
  A.object
  [ ("cmd",      "insert")
  , ("document", buildDocument scoreA nowA fctName fctInfo)
  ]
    
-- | Build the Insert commands for a list of FunctionInfo records.
buildInserts :: Score -> UTCTime -> [ (String, FunctionInfo) ] -> [A.Value]
buildInserts score now items = [ buildInsert scoreA nowA fctName fctInfo | (fctName, fctInfo) <- items ]
  where
    nowA = A.String $ T.pack $ fmtDateXmlSchema now
    scoreA = A.Number $ S.fromFloatDigits score

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

