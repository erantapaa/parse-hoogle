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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data HoogleLine = Blank | Comment String | Instance | Class | Package | Version | Type | Data | Module | Decl | Newtype | MultiDecl | BracketDecl

oneLineComment =
  do string "--";
     comment <- manyTill anyChar (char '\n');
     return $ Comment comment

blankLine =
  do skipMany (satisfy (\c -> isSpace c && c /= '\n'))
     char '\n'
     return Blank

lineSpace c = isSpace c && c /= '\n'

startsWith str val =
  do string str
     satisfy lineSpace
     _ <- manyTill anyChar (char '\n');
     return val

instanceDef = startsWith "instance" Instance
classDef    = startsWith "class" Class
packageDef  = startsWith "@package" Package
versionDef  = startsWith "@version" Version
typeDef     = startsWith "type" Type
dataDef     = startsWith "data" Data
moduleDef   = startsWith "module" Module
newTypeDef  = startsWith "newtype" Newtype

ident = Token.identifier haskell
symbol = Token.symbol haskell
reservedOp = Token.reservedOp haskell
operator = Token.operator haskell
whitespace = Token.whiteSpace haskell

decl = do i <- ident
          reservedOp "::"
          _ <- manyTill anyChar (char '\n');
          return $ Decl

multiDecl =
  do symbol "(" 
     sepBy1 (try ident <|> operator) (symbol ",")
     symbol ")"
     reservedOp "::"
     _ <- manyTill anyChar (char '\n');
     return $ MultiDecl

bracketDecl =
  do symbol "["
     i <- try ident <|> operator
     symbol "]"
     reservedOp "::"
     _ <- manyTill anyChar (char '\n');
     return $ BracketDecl

anyLine = try oneLineComment <|> try instanceDef <|> try classDef <|> try packageDef <|> try versionDef <|> try typeDef <|> try dataDef <|> try moduleDef <|> try newTypeDef <|> try decl <|> try multiDecl <|> try bracketDecl <|> blankLine

-- e.g.: testFile anyLine "hoogle-data/abacate/0.0.0.0/doc/html/abacate.txt"

testFile parser path = do
  lns <- fmap lines $ readFile path
  forM_ (zip [(1::Int)..] lns) $ \(i,ln) -> do
    let source = path ++ " line " ++ show i
        ln' = ln ++ ['\n']
    case parse parser source ln' of
      Left e  -> putStrLn $ "error: " ++ show e
      Right x -> return ()

