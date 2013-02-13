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
parseDefineExpr = keywordDefine >> Define <$> ident <*> parseEgisonExpr

parseTestExpr :: Parser EgisonTopExpr
parseTestExpr = keywordTest >> Test <$> parseEgisonExpr

parseExecuteExpr :: Parser EgisonTopExpr
parseExecuteExpr = keywordExecute >> Execute <$> sepEndBy stringLiteral whiteSpace

parseLoadFileExpr :: Parser EgisonTopExpr
parseLoadFileExpr = keywordLoadFile >> LoadFile <$> stringLiteral

parseLoadExpr :: Parser EgisonTopExpr
parseLoadExpr = keywordLoad >> Load <$> stringLiteral

parseEgisonExpr :: Parser EgisonExpr
parseEgisonExpr = parseMatchAllExpr
                  <|> parseMatchExpr
                  <|> parseInductiveDataExpr
                  <|> parseTupleExpr
                  <|> parseCollectionExpr
                  <|> parseFunctionExpr
                  <|> parseIfExpr
                  <|> parseLambdaExpr
                  <|> parseLetRecExpr
                  <|> parseLetExpr
                  <|> parseApplyExpr
                  <|> parsePatternExpr
                  <|> parseConstantExpr

parseMatchAllExpr :: Parser EgisonExpr
parseMatchAllExpr = keywordMatchAll >> MatchAllExpr <$> parseEgisonExpr <*> parseEgisonExpr <*> parseMatchClause

parseMatchExpr :: Parser EgisonExpr
parseMatchExpr = keywordMatch >> MatchExpr <$> parseEgisonExpr <*> parseEgisonExpr <*> parseMatchClauses

parseMatchClauses :: Parser [MatchClause]
parseMatchClauses = sepEndBy parseMatchClause whiteSpace

parseMatchClause :: Parser MatchClause
parseMatchClause = brackets $ (,) <$> parseEgisonExpr <*> parseEgisonExpr

parseInductiveDataExpr :: Parser EgisonExpr
parseInductiveDataExpr = angles $ InductiveDataExpr <$> ident <*> exprs
  where
    exprs = sepEndBy parseEgisonExpr whiteSpace

parseTupleExpr :: Parser EgisonExpr
parseTupleExpr = brackets $ TupleExpr <$> sepEndBy parseEgisonExpr whiteSpace

parseCollectionExpr :: Parser EgisonExpr
parseCollectionExpr = braces $ CollectionExpr <$> sepEndBy parseInnerExpr whiteSpace
  where
    parseInnerExpr :: Parser InnerExpr
    parseInnerExpr = (char '@' >> SubCollectionExpr <$> parseEgisonExpr)
                     <|> ElementExpr <$> parseEgisonExpr
parseFunctionExpr :: Parser EgisonExpr
parseFunctionExpr = keywordFunction >> FunctionExpr <$> parseEgisonExpr <*> parseMatchClauses

parseIfExpr :: Parser EgisonExpr
parseIfExpr = IfExpr <$> (keywordIf   *> parseEgisonExpr)
                     <*> (keywordThen *> parseEgisonExpr)
                     <*> (keywordElse *> parseEgisonExpr)

parseLambdaExpr :: Parser EgisonExpr
parseLambdaExpr = keywordLambda >> LambdaExpr <$> parseParams <*> parseEgisonExpr

parseParams :: Parser [String]
parseParams = brackets $ sepEndBy parseName whiteSpace

parseLetRecExpr :: Parser EgisonExpr
parseLetRecExpr =  keywordLetRec >> LetRecExpr <$> parseRecursiveBindings <*> parseEgisonExpr

parseLetExpr :: Parser EgisonExpr
parseLetExpr = keywordLet >> LetExpr <$> parseBindings <*> parseEgisonExpr

parseApplyExpr :: Parser EgisonExpr
parseApplyExpr = parens $ ApplyExpr <$> parseEgisonExpr <*> parseEgisonExpr

parseDoExpr :: Parser EgisonExpr
parseDoExpr = keywordDo >> DoExpr <$> parseBindings <*> parseEgisonExpr

parseBindings :: Parser Bindings
parseBindings = braces (sepEndBy (brackets $ (,) <$> parseEgisonExpr <*> parseEgisonExpr) whiteSpace)

parseRecursiveBindings :: Parser RecursiveBindings
parseRecursiveBindings = braces (sepEndBy (brackets $ (,) <$> parseName <*> parseEgisonExpr) whiteSpace)

parseVar :: Parser EgisonExpr
parseVar = VarExpr <$> ident <*> parseIndexNums

parseIndexNums :: Parser [EgisonExpr]
parseIndexNums = (char '_' >> (:) <$> ident <*> parseIndexNums)
                 <|> pure []

parsePatternExpr :: Parser EgisonExpr
parsePatternExpr =  notImplemented

parseCutPat :: Parser EgisonExpr
parseCutPat = char '!' >> CutPatExpr <$> parseEgisonExpr

parseNotPat :: Parser EgisonExpr
parseNotPat = char '^' >> NotPatExpr <$> parseEgisonExpr

parseWildCard :: Parser EgisonExpr
parseWildCard = char '_' >> WildCardExpr <$> parseEgisonExpr

parseValuePat :: Parser EgisonExpr
parseValuePat = char ',' >> ValuePatExpr <$> parseEgisonExpr



parseConstantExpr :: Parser EgisonExpr
parseConstantExpr =  parseCharExpr
                     <|> parseStringExpr
                     <|> parseBoolExpr
                     <|> parseIntegerExpr
                     <|> parseFloatExpr
                     <|> (keywordSomething *> pure SomethingExpr)
                     <|> (keywordUndefined *> pure UndefinedExpr)

parseCharExpr :: Parser EgisonExpr
parseCharExpr = CharExpr <$> charLiteral

parseStringExpr :: Parser EgisonExpr
parseStringExpr = notImplemented

parseBoolExpr :: Parser EgisonExpr
parseBoolExpr = char '#' >> BoolExpr <$> (char 't' *> pure True <|> char 'f' *> pure False)

parseIntegerExpr :: Parser EgisonExpr
parseIntegerExpr = IntegerExpr <$> integerLiteral

parseFloatExpr :: Parser EgisonExpr
parseFloatExpr = FloatExpr <$> floatLiteral

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
  , "lambda"
  , "letrec"
  , "let"
  , "match-all"
  , "match"
  , "do"
  , "function"
  , "something"
  , "undefined"]
  
reservedOperators :: [String]
reservedOperators = []
reserved = P.reserved lexer

keywordDefine     = reserved "define"
keywordDefineTyp  = reserved "define-type"
keywordDefineCls  = reserved "define-class"
keywordInstance   = reserved "instance"
keywordTest       = reserved "test"
keywordExecute    = reserved "execute"
keywordLoadFile   = reserved "load-file"
keywordLoad       = reserved "load"
keywordLambda     = reserved "lambda"
keywordLetRec     = reserved "letrec"
keywordLet        = reserved "let"
keywordIf         = reserved "if"
keywordThen       = reserved "then"
keywordElse       = reserved "else"
keywordMatchAll   = reserved "match-all"
keywordMatch      = reserved "match"
keywordDo         = reserved "do"
keywordFunction   = reserved "function"
keywordSomething  = reserved "something"
keywordUndefined  = reserved "undefiend"

integerLiteral :: Parser Integer
integerLiteral = P.integer lexer

floatLiteral :: Parser Double
floatLiteral = P.float lexer

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
