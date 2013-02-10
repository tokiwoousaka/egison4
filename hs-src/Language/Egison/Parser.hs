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
parseEgisonTopExpr = parens $  parseDefineExpr
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
parseEgisonExpr = parseInductiveDataExpr
                  <|> parseTupleExpr
                  <|> parseCollectionExpr
                  <|> parseFuncExpr
                  <|> parseIfExpr
                  <|> parseLetRecExpr
                  <|> parseLetExpr
                  <|> parsePatternExpr
                  <|> parseConstantExpr
                  

parseMatchAllExpr :: Parser EgisonExpr
parseMatchAllExpr = notImplemented

parseMatchExpr :: Parser EgisonExpr
parseMatchExpr = notImplemented

parseInductiveDataExpr :: Parser EgisonExpr
parseInductiveDataExpr = angles $ InductiveDataExpr <$> ident <*> exprs
  where
    exprs = sepEndBy parseEgisonExpr whiteSpace

parseTupleExpr :: Parser EgisonExpr
parseTupleExpr = notImplemented

parseCollectionExpr :: Parser EgisonExpr
parseCollectionExpr = notImplemented

parseFuncExpr :: Parser EgisonExpr
parseFuncExpr = notImplemented

parseIfExpr :: Parser EgisonExpr
parseIfExpr = IfExpr <$> (keywordIf   *> parseEgisonExpr)
                     <*> (keywordThen *> parseEgisonExpr)
                     <*> (keywordElse *> parseEgisonExpr)

parseLetRecExpr :: Parser EgisonExpr
parseLetRecExpr =  keywordLetRec *> (LetRecExpr <$> parseRecursiveBindings <*> parseEgisonExpr)

parseLetExpr :: Parser EgisonExpr
parseLetExpr = notImplemented

parsePatternExpr :: Parser EgisonExpr
parsePatternExpr = notImplemented

parseConstantExpr :: Parser EgisonExpr
parseConstantExpr =  parseCharExpr
                     <|> parseStringExpr
                     <|> parseBoolExpr
                     <|> parseIntegerExpr
                     <|> parseFloatExpr

parseCharExpr :: Parser EgisonExpr
parseCharExpr = CharExpr <$> charLiteral

parseStringExpr :: Parser EgisonExpr
parseStringExpr = notImplemented

parseBoolExpr :: Parser EgisonExpr
parseBoolExpr = notImplemented

parseIntegerExpr :: Parser EgisonExpr
parseIntegerExpr = notImplemented

parseFloatExpr :: Parser EgisonExpr
parseFloatExpr = notImplemented

parseRecursiveBindings :: Parser RecursiveBindings
parseRecursiveBindings = braces $ sepEndBy binding whiteSpace
  where
    binding = brackets $ (\x y -> (x, y)) <$> parseName <*> parseEgisonExpr

parseName :: Parser String
parseName = char '$' >> ident
  
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
  , "instance" 
  , "if"
  , "then"
  , "else" 
  , "letrec"
  , "let"
  , "match-all"
  , "match"]
  
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
keywordLetRec     = reserved "letrec"
keywordLet        = reserved "let"
keywordIf         = reserved "if"
keywordThen       = reserved "then"
keywordElse       = reserved "else"
keywordMatchAll   = reserved "match-all"
keywordMatch      = reserved "match"


stringLiteral :: Parser String
stringLiteral = P.stringLiteral lexer

charLiteral :: Parser Char
charLiteral = P.charLiteral lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

brackets :: Parser a -> Parser a
brackets = P.brackets lexer

braces :: Parser a -> Parser a
braces = P.braces lexer

angles :: Parser a -> Parser a
angles = P.angles lexer

colon :: Parser String
colon = P.colon lexer

comma :: Parser String
comma = P.comma lexer

dot :: Parser String
dot = P.dot lexer

ident :: Parser String
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
