{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Language.Egison.Types where

import qualified Data.Map
import Data.IORef
import System.IO
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

--
-- Expressions
--
data EgisonTopExpr =
    Define String EgisonExpr
  | DefineType String DefineTypeExpr
  | DefineClass String DefineClassExpr
  | Instance String [String] [(String, String)]
  | Test EgisonExpr
  | Execute [String]
    -- temporary : we will replace load to import and export
  | LoadFile String
  | Load String
 deriving (Show)

data EgisonExpr =
    CharExpr Char
  | StringExpr String
  | BoolExpr Bool
  | IntegerExpr Integer
  | FloatExpr Double
  | PatVarExpr String [EgisonExpr]
  | VarExpr String [EgisonExpr]
  | VarNameExpr String
  | PatVarOmitExpr String [EgisonExpr]
  | OmitExpr String [EgisonExpr]

  | WildCardExpr
  | ValuePatExpr EgisonExpr
  | PredPatExpr EgisonExpr
  | CutPatExpr EgisonExpr
  | NotPatExpr EgisonExpr
  | AndPatExpr [EgisonExpr]
  | OrPatExpr [EgisonExpr]

  | InductiveDataExpr String [EgisonExpr]
  | TupleExpr [EgisonExpr]
  | CollectionExpr [InnerExpr]
  | FuncExpr EgisonExpr EgisonExpr

  | IfExpr EgisonExpr EgisonExpr EgisonExpr
  | LetExpr Bindings EgisonExpr
  | LetRecExpr RecursiveBindings EgisonExpr
  | DoExpr Bindings EgisonExpr

  | MatchExpr EgisonExpr EgisonExpr [MatchClause]
  | MatchAllExpr EgisonExpr EgisonExpr MatchClause

  | MatcherExpr MatcherInfoExpr
  
  | ClassExpr ClassInfoExpr
  | InstanceExpr ClassInfoExpr

  | ApplyExpr EgisonExpr EgisonExpr

  | SomethingExpr
  | UndefinedExpr
 deriving (Show)

data EgisonTypeExpr =
    CharTypeExpr
  | StringTypeExpr
  | BoolTypeExpr
  | IntegerTypeExpr
  | FloatTypeExpr
  | VarNameTypeExpr
    -- Type variable binded to some type
  | VarTypeExpr String
    -- _ -> _
  | FunTypeExpr EgisonTypeExpr EgisonTypeExpr
    -- (Match _), (Pattern _)
  | MatcherTypeExpr EgisonTypeExpr
  | PatternTypeExpr EgisonTypeExpr
    -- [_ _ ...], (Collection _)
  | TupleTypeExpr [EgisonTypeExpr]
  | CollectionTypeExpr EgisonTypeExpr
    -- (FnType ArgType ...)
  | AppTypeExpr EgisonTypeExpr EgisonTypeExpr
 deriving (Show)
          
data EgisonClassExpr =
    TypeClassExpr
  | VarClassExpr String
  | FunClassExpr EgisonClassExpr EgisonClassExpr
        
type MatchClause = (EgisonExpr, EgisonExpr)

data PrimitivePatPattern =
    PPWildCard
  | PPValuePat String
  | PPInductivePat String [PrimitivePatPattern]
 deriving (Show)

data PrimitivePattern =
    PWildCard
  | PPatVar String
  | PInductivePat String [PrimitivePattern]
  | PEmptyPat
  | PConsPat PrimitivePattern PrimitivePattern
  | PSnocPat PrimitivePattern PrimitivePattern

  | PPatBool Bool
  | PPatChar Char
  | PPatInteger Integer
  | PPatFloat Double
 deriving (Show)

data InnerExpr =
    ElementExpr EgisonExpr
  | SubCollectionExpr EgisonExpr
 deriving (Show)

data InnerTypedExpr =
    ElementTypedExpr EgisonTypedExpr
  | SubCollectionTypedExpr EgisonTypedExpr
 deriving (Show)

type Bindings = [(EgisonExpr, EgisonExpr)]

type RecursiveBindings = [(String, EgisonExpr)]
  
type MatcherInfoExpr = [(PrimitivePatPattern, EgisonExpr, [(PrimitivePattern, EgisonExpr)])]

type ClassInfoExpr = [(String, EgisonTypeExpr)]

--
-- Typed Expression
--

data EgisonTypedExpr =
 Hoge
 deriving (Show)

data DefineTypeExpr =
  Huga
 deriving (Show)

data DefineClassExpr =
  Piyo
 deriving (Show)

data Environment =
  Poyo
 deriving (Show)

--
-- Values
--
type ObjectRef = IORef Object

data Object =
    Closure Env EgisonExpr
  | Pattern EgisonPattern
  | Intermidiate EgisonIntermidiate
  | Value EgisonValue
  
data EgisonType =
    CharType
  | StringType
  | BoolType
  | IntegerType
  | FloatType
  | TypeType
  | ClassType
  | PatternType EgisonType

  | TupleType [EgisonType]
  | CollectionType EgisonType
  | FunType EgisonType EgisonType
  | AppType EgisonType EgisonType

  | PatVarType String
  | VarType String
 deriving (Show)
        
data EgisonPattern =
    WildCard
  | PatVar String [Integer]
  | ValuePat Environment EgisonExpr
  | PredPat Environment EgisonExpr [EgisonExpr]
  | CutPat EgisonPattern
  | NotPat EgisonPattern
  | AndPat [EgisonPattern]
  | OrPat [EgisonPattern]
  | TuplePat [EgisonPattern]
  | InductivePat String [EgisonPattern]

data EgisonIntermidiate =
    IInductiveData String [ObjectRef]
  | ITuple [ObjectRef]
  | ICollection [InnerObject]

data EgisonValue =
    World [Action]
  | Char Char
  | String String
  | Bool Bool
  | Integer Integer
  | Float Double
  | InductiveData String [EgisonValue]
  | Tuple [EgisonValue]
  | Collection [EgisonValue]
  | Matcher MatcherInfo
  | Class ClassInfo
  | Func ObjectRef EgisonExpr Env
  | PrimitiveFunc ([EgisonValue] -> ThrowsError EgisonValue)
  | IOFunc ([EgisonValue] -> IOThrowsError EgisonValue)
  | Port String Handle
  | Something
  | EOF

data InnerObject =
    IElement ObjectRef
  | ISubCollection ObjectRef

data Action =
    OpenInputPort String
  | OpenOutputPort String
  | ClosePort String
  | FlushPort String
  | ReadFromPort String String
  | WriteToPort String String
 deriving (Show)

type MatcherInfo = [(PrimitivePatPattern, ObjectRef, [(Env, PrimitivePattern, EgisonExpr)])]
type ClassInfo = [(String, EgisonType)]

--
-- Internal Data
--
type VarExpr = (String, [EgisonExpr])

type Var = (String, [Integer])

type FrameList = [(Var, ObjectRef)]

type Frame = Data.Map.Map Var ObjectRef

type FrameRef = IORef Frame

data Env = Environment {
        parentEnv :: (Maybe Env), 
        topFrameRef :: FrameRef
    }

data MatchFlag = MAll | MOne
  
data PClosure = PClosure {pcFrame :: FrameList,
                          pcBody :: ObjectRef
                          }

data MAtom = MAtom {pClosure :: PClosure,
                    maTyp :: ObjectRef,
                    maTarget :: ObjectRef
                    }

data MState = MState {msFrame :: FrameList,
                      mAtoms :: [MAtom]
                      }

---
--- Types for Error Handling
---
data EgisonError =
    Parser ParseError
  | TypeMismatch EgisonExpr EgisonType
  | NotImplemented String
  | Default String

showError :: EgisonError -> String
showError (Parser parseErr) = "Parse error at " ++ ": " ++ show parseErr
showError (TypeMismatch expr typ) = "Type error: The type of a expression '" ++ show expr ++ "' is expected to be '" ++ show typ ++ "'"
showError (NotImplemented message) = "Not implemented: " ++ message
showError (Default message) = "Error: " ++ message

instance Show EgisonError where show = showError
instance Error EgisonError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either EgisonError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error "Unexpected error in extractValue; "

type IOThrowsError = ErrorT EgisonError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrowsREPL :: IOThrowsError String -> IO String
runIOThrowsREPL action = runErrorT (trapError action) >>= return . extractValue

runIOThrows :: IOThrowsError String -> IO (Maybe String)
runIOThrows action = do
    runState <- runErrorT action
    case runState of
        Left err -> return $ Just (show err)
        Right _ -> return $ Nothing
