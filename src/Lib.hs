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

data HoogleLine = BlankLine
                | Comment String             -- comment line (begins with "--"
                | Package String             -- @package declaration
                | Version String             -- @version declaration
                | Instance String            -- instance (...) => ...
                | Class String               -- class (...) => ...
                | Type String String String  -- type <name> <params> = ...
                | Data                       -- data <name> <params>
                | Module String              -- module ...
                | Decl                       -- function signature
                | Newtype String String      -- newtype <name> <params>
                | MultiDecl                  -- (a,b,c) :: ...
                | BracketDecl                -- [a] :: ...
                | DataType                   -- dataType[...] :: DataType
                | Constr                     -- constr[...] :: Constr
  deriving (Show)

isLineSpace c = isSpace c && c /= '\n'
lineSpace   = satisfy isLineSpace

lexeme p    = do{ x <- p; skipMany lineSpace; return x }

restOfLine  = manyTill anyChar (char '\n');

symbol name = lexeme (string name)

identStart  = letter <|> char '_'
identLetter = alphaNum <|> oneOf "_'" <|> satisfy (\c -> ord c > 127)

ident = lexeme $ try $ 
          (do { c <- identStart
             ; cs <- many identLetter
             ; hash <- option False $ do { satisfy (== '#'); return True }
             ; return (c:cs)
             } <?> "identifier")

opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~" <|> satisfy (\c -> ord c > 127)
operator = lexeme $ (many1 opLetter)
                    <|> do { symbol "("; commas <- many (symbol ","); symbol ")"; return $ "(" ++ map (const ',') commas ++ ")" }

parenOp = do char '('
             name <- many1 opLetter
             char ')'
             return name

identOrOp = ident <|> parenOp <|> operator

startsWith str =
  do symbol str
     restOfLine

instanceDef = fmap Instance $ startsWith "instance" 
classDef    = fmap Class $ startsWith "class"
packageDef  = fmap Package $ startsWith "@package" 
versionDef  = fmap Version $ startsWith "@version"
moduleDef   = fmap Module $ startsWith "module"
dataDef     = startsWith "data" >> return Data

blankLine =
  do skipMany lineSpace
     char '\n'
     return BlankLine

oneLineComment =
  do symbol "--";
     comment <- restOfLine
     return $ Comment comment

constraint = do
  manyTill anyChar (symbol " =>")
  return ()

newTypeDef = do
  symbol "newtype"
  option () (try constraint)
  name <- identOrOp
  params <- restOfLine
  return $ Newtype name params

typeDef     = do
  symbol "type"
  name <- identOrOp
  lhs <- many (satisfy (/= '='))
  symbol "="
  sig <- restOfLine
  return $ Type name lhs sig


decl = do i <- ident
          symbol "::"
          restOfLine
          return $ Decl

multiDecl1 =
  do symbol "(" 
     sepBy (try ident <|> operator) (symbol ",")
     symbol ")"
     symbol "::"
     restOfLine
     return $ MultiDecl

multiDecl2 =
  do sepBy (try ident <|> operator) (symbol ",")
     symbol "::"
     restOfLine
     return $ MultiDecl

bracketDecl =
  do symbol "["
     sepBy1 (try ident <|> operator) (symbol ",")
     symbol "]"
     symbol "::"
     restOfLine
     return $ BracketDecl

dataTypeDecl =
  do string "dataType"
     symbol "["
     ident
     symbol "]"
     symbol "::"
     restOfLine
     return $ DataType

constrDecl =
  do string "constr"
     symbol "["
     ident
     symbol "]"
     symbol "::"
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

