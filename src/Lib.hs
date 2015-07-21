{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib
where

import System.Environment
import Control.Monad

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language (haskell, haskellDef)
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Text.Parsec.Token as Token

-- import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator

import Data.Char
import Data.List (isPrefixOf, break)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Lazy as LBS hiding (readFile, pack, snoc)
import qualified Data.Text.Lazy as Text
import Data.Text.Lazy.Encoding (decodeUtf8)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data HoogleLine = Blank | Comment String | Instance | Class | Package | Version | Type | Data | Module | Decl | Newtype | MultiDecl | BracketDecl | DataType | Constr
  deriving (Show)

restOfLine = manyTill anyChar (char '\n');

oneLineComment =
  do string "--";
     comment <- restOfLine
     return $ Comment comment

blankLine =
  do skipMany (satisfy (\c -> isSpace c && c /= '\n'))
     char '\n'
     return Blank

lineSpace c = isSpace c && c /= '\n'

startsWith str val =
  do string str
     satisfy lineSpace
     restOfLine
     return val

instanceDef = startsWith "instance" Instance
classDef    = startsWith "class" Class
packageDef  = startsWith "@package" Package
versionDef  = startsWith "@version" Version
typeDef     = startsWith "type" Type
dataDef     = startsWith "data" Data
moduleDef   = startsWith "module" Module
newTypeDef  = startsWith "newtype" Newtype

lexeme      = Token.lexeme haskell
identStart  = letter <|> oneOf "_"
identLetter = alphaNum <|> oneOf "_'" <|> satisfy (\c -> ord c > 127)

ident = lexeme $ try $ 
          (do { c <- identStart
             ; cs <- many identLetter
             ; hash <- option False $ do { satisfy (== '#'); return True }
             ; return (c:cs)
             } <?> "identifier")

isSimpleSpace ch = isSpace ch && ch /= '\n'
symbol name = do { string name; skipMany (satisfy isSimpleSpace) }

opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~" <|> satisfy (\c -> ord c > 127)
operator = do { op <- many1 opLetter; whitespace; return op }
           <|> do { symbol "("; commas <- many (symbol ","); symbol ")"; return $ "(" ++ map (const ',') commas ++ ")" }

reservedOp = symbol
whitespace = skipMany (satisfy isSimpleSpace)

decl = do i <- ident
          reservedOp "::"
          restOfLine
          return $ Decl

multiDecl1 =
  do symbol "(" 
     sepBy (try ident <|> operator) (symbol ",")
     symbol ")"
     reservedOp "::"
     restOfLine
     return $ MultiDecl

multiDecl2 =
  do sepBy (try ident <|> operator) (symbol ",")
     reservedOp "::"
     restOfLine
     return $ MultiDecl

bracketDecl =
  do symbol "["
     sepBy (try ident <|> operator) (symbol ",")
     symbol "]"
     reservedOp "::"
     restOfLine
     return $ BracketDecl

dataTypeDecl =
  do string "dataType"
     symbol "["
     ident
     symbol "]"
     reservedOp "::"
     restOfLine
     return $ DataType

constrDecl =
  do string "constr"
     symbol "["
     ident
     symbol "]"
     reservedOp "::"
     restOfLine
     return $ Constr

anyLine = try oneLineComment
          <|> try instanceDef
          <|> try classDef
          <|> try packageDef
          <|> try versionDef
          <|> try typeDef
          <|> try dataDef
          <|> try moduleDef
          <|> try newTypeDef
          <|> try decl
          <|> try multiDecl1
          <|> try multiDecl2
          <|> try bracketDecl
          <|> try dataTypeDecl
          <|> constrDecl
          <|> blankLine

-- e.g.: testFile anyLine "hoogle-data/abacate/0.0.0.0/doc/html/abacate.txt"

testFile parser path = do
  lns <- fmap LBS.lines $ LBS.readFile path
  -- Lines before @package may not be properly UTF-8 encoded
  -- so ignore them.
  let (ignored, lns') = break (LBS.isPrefixOf (LBS.pack "@package")) lns
      i0 = 1+length ignored
  forM_ (zip [(i0::Int)..] lns') $ \(i,ln) -> do
    let source = path ++ " line " ++ show i
        ln' = Text.unpack $ (decodeUtf8 ln) `Text.snoc` '\n'
    case parse parser source ln' of
      Left e  -> do putStrLn $ "error: " ++ show e
                    putStr ln'
                    putStrLn ""
      Right x -> return ()

-- test the anyLine parse against all of the hoogle files
testAllFiles = do
  files <- fmap lines $ readFile "all-hoogle-files"
  mapM_ (testFile anyLine) files

