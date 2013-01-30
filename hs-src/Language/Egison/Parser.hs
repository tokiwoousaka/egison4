module Language.Egison.Parser where
import Control.Monad.Identity
import Control.Monad.Error
import Control.Applicative ((<$>), (<*>), (*>), (<*), pure)
import Data.ByteString.Lazy(ByteString)
import Text.Parsec
import Text.Parsec.ByteString.Lazy
import Text.Parsec.Char
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as P

import Language.Egison.Types

parseEgisonTopExpr :: Parser EgisonTopExpr
parseEgisonTopExpr = parseDefineExpr
                     <|> parseTestExpr
                     <|> parseExecuteExpr
                     <|> parseLoadFileExpr
                     <|> parseLoadExpr

parseDefineExpr :: Parser EgisonTopExpr
parseDefineExpr = keywordDefine *> (Define <$> ident <*> parseEgisonExpr)

parseTestExpr :: Parser EgisonTopExpr
parseTestExpr = keywordTest *> (Test <$> parseEgisonExpr)

parseExecuteExpr :: Parser EgisonTopExpr
parseExecuteExpr = keywordExecute *> (Execute <$> sepEndBy stringLiteral whiteSpace)

parseLoadFileExpr :: Parser EgisonTopExpr
parseLoadFileExpr = keywordLoadFile *> (LoadFile <$> stringLiteral)

parseLoadExpr :: Parser EgisonTopExpr
parseLoadExpr = keywordLoad *> (Load <$> stringLiteral)

parseEgisonExpr :: Parser EgisonExpr
parseEgisonExpr = notImplemented

notImplemented :: Parser a
notImplemented = choice []

reservedKeywords :: [String]
reservedKeywords = 
  [ "define"
  , "define-type"
  , "define-class"
  , "test"
  , "execute"
  , "load-file"
  , "load"
  , "instance" ]
  
reservedOperators :: [String]
reservedOperators = []
reserved = P.reserved lexer

keywordDefine     = reserved "define"
keywordDefineTyp  = reserved "define-type"
keywordDefineCls  = reserved "define-class"
keywordTest       = reserved "test"
keywordExecute    = reserved "execute"
keywordLoadFile   = reserved "load-file"
keywordLoad       = reserved "load"

stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

ident :: Parser Ident
ident = P.identifier lexer

egisonDef :: P.GenLanguageDef ByteString () Identity
egisonDef = 
  P.LanguageDef { P.commentStart       = "#|"
                , P.commentEnd         = "|#"
                , P.commentLine        = ";"
                , P.identStart         = letter <|> char '_'
                , P.identLetter        = letter <|> char '_' <|> digit
                , P.opStart            = symbol
                , P.opLetter           = symbol
                , P.reservedNames      = reservedKeywords
                , P.reservedOpNames    = reservedOperators
                , P.nestedComments     = True
                , P.caseSensitive      = True }
  where
    symbol = oneOf "&*+-/:="

lexer :: P.GenTokenParser ByteString () Identity
lexer = P.makeTokenParser egisonDef
