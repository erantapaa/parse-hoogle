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
                | Module String              -- module ...
                | Type String String String  -- type <name> <params> = ...
                | Newtype String String      -- newtype <name> <params>
                | FunctionDecl String String -- <name> :: <sig>   -- function signature
                | DataDecl String            -- data <name>
                | MultiDecl [String] String  -- (a,b,c) :: ...
                | BracketDecl                -- [a] :: ...
                | Instance String            -- instance (...) => ...
                | Class String               -- class (...) => ...
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

ident = lexeme $ ((try ident') <?> "identifier")

-- an identifer without consuming following whitespace
ident' = do { c <- identStart
            ; cs <- many identLetter
            ; hash <- option False $ do { satisfy (== '#'); return True }
            ; return (c:cs)
            }

opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~" <|> satisfy (\c -> ord c > 127)

operator = lexeme $ (many1 opLetter) <|> tupleOp

parenOp = do char '('
             name <- many1 opLetter
             char ')'
             return name

tupleOp = do char '('
             commas <- many (symbol ",")
             char ')'
             return $ "(" ++ concat commas ++ ")"

identOrOp = ident <|> parenOp <|> operator

startsWith str =
  do symbol str
     restOfLine

instanceDef = fmap Instance $ startsWith "instance"
classDef    = fmap Class $ startsWith "class"
packageDef  = fmap Package $ startsWith "@package"
versionDef  = fmap Version $ startsWith "@version"
moduleDef   = fmap Module $ startsWith "module"

blankLine =
  do skipMany lineSpace
     char '\n'
     return BlankLine

oneLineComment =
  do symbol "--";
     comment <- restOfLine
     return $ Comment comment

constraint = do
  manyTill anyChar (try (symbol " =>"))

newTypeDef = do
  symbol "newtype"
  option "" (try constraint)
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

functionDecl = do
  name <- ident
  symbol "::"
  sig <- restOfLine
  return $ FunctionDecl name sig

-- data declarations examples:
--
--     data Scenario
--     data Lit s
--     data AbList a b
--     data SatResult :: *
--     data Network (l :: * -> *) (g :: * -> *) :: (* -> *) -> (* -> *) -> *
--     data (:=:) a b
--     data ATuple20 s[am5Q] a[am5R]
--     data DebuggerM (m :: * -> *) (past :: [*]) (current :: *) (future :: [*])
dataDef     = do
  symbol "data"
  try d1 <|> try d2 <|> d3
  where
    d1 = do name <- dataName
            params <- many dataParam
            kindsig <- (do char '\n'; return "") <|> (do symbol "::"; restOfLine)
            return $ DataDecl name
    d2 = do skipMany1 ident
            symbol "=>"
            d1
    d3 = do lexeme $ do { char '('; many balancedParens; char ')' }
            symbol "=>"
            d1

dataName = lexeme $ try ident <|> try parenOp <|> tupleOp
dataParam = lexeme $ (try simpleParam <|> parenParam <|> dollarParam)
  where
    simpleParam = do i <- ident'; optional (do char '['; ident'; char ']'); return i
    parenParam = do char '('; many balancedParens; char ')'; return ""
    dollarParam = do char '$'; i <- ident'; return $ "$" ++ i

balancedParens =
  do { satisfy (\ch -> ch /= '(' && ch /= ')'); return () }
    <|> do { char '('; many balancedParens; char ')'; return () }

multiDecl =
  do symbol "("
     names <- sepBy (nakedOp <|> ident') (symbol ",")
     symbol ")"
     symbol "::"
     sig <- restOfLine
     return $ MultiDecl names sig
  where
    nakedOp = many1 opLetter

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
          <|> try functionDecl
          <|> try multiDecl
          <|> try bracketDecl
          <|> try dataTypeDecl
          <|> constrDecl
          <|> blankLine

-- e.g.: testFile anyLine "hoogle-data/abacate/0.0.0.0/doc/html/abacate.txt"

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

-- test a parser against the lines in a file - skip lines before @package
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

