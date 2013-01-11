{-# Language TypeSynonymInstances, FlexibleInstances #-}
module Language.Egison.Types where

import qualified Data.Map
import Data.IORef

--
-- Expression
--
data TopExpr =
    Define String EgisonExpr
  | Test EgisonExpr
  | Execute [String]
  | LoadFile String
  | Load String
 deriving (Show)

data EgisonTypeExpr =
    CharTypeExpr
  | StringTypeExpr
  | BoolTypeExpr
  | IntegerTypeExpr
  | FloatTypeExpr
  | TypeTypeExpr
  | ClassTypeExpr
  | PatternTypeExpr

  | TupleTypeExpr [EgisonTypeExpr]
  | CollectionTypeExpr EgisonTypeExpr
  | FunTypeExpr EgisonTypeExpr EgisonTypeExpr
  | AppTypeExpr EgisonTypeExpr EgisonTypeExpr

  | PatVarTypeExpr String
  | VarTypeExpr String
        
data EgisonType =
    CharType
  | StringType
  | BoolType
  | IntegerType
  | FloatType
  | TypeType
  | ClassType
  | PatternType

  | TupleType [EgisonType]
  | CollectionType EgisonType
  | FunType EgisonType EgisonType
  | AppType EgisonType EgisonType

  | PatVarType String
  | VarType String
        
data EgisonExpr =
    CharExpr Char (Maybe EgisonTypeExpr)
  | StringExpr String (Maybe EgisonTypeExpr)
  | BoolExpr Bool (Maybe EgisonTypeExpr)
  | IntegerExpr Integer (Maybe EgisonTypeExpr)
  | FloatExpr Double (Maybe EgisonTypeExpr)
  | PatVarExpr String [EgisonExpr] (Maybe EgisonTypeExpr)
  | VarExpr String [EgisonExpr] (Maybe EgisonTypeExpr)
  | SymbolExpr String (Maybe EgisonTypeExpr)
  | PatVarOmitExpr String [EgisonExpr] (Maybe EgisonTypeExpr)
  | VarOmitExpr String [EgisonExpr] (Maybe EgisonTypeExpr)

  | WildCardExpr (Maybe EgisonTypeExpr)
  | ValuePatExpr EgisonExpr (Maybe EgisonTypeExpr)
  | CutPatExpr EgisonExpr (Maybe EgisonTypeExpr)
  | NotPatExpr EgisonExpr (Maybe EgisonTypeExpr)
  | AndPatExpr [EgisonExpr] (Maybe EgisonTypeExpr)
  | OrPatExpr [EgisonExpr] (Maybe EgisonTypeExpr)
  | PredPatExpr EgisonExpr [EgisonExpr] (Maybe EgisonTypeExpr)

  | InductiveDataExpr String [EgisonExpr] (Maybe EgisonTypeExpr)
  | TupleExpr [EgisonExpr] (Maybe EgisonTypeExpr)
  | CollectionExpr [InnerExpr] (Maybe EgisonTypeExpr)
  | FuncExpr EgisonExpr EgisonExpr (Maybe EgisonTypeExpr)

  | IfExpr EgisonExpr EgisonExpr EgisonExpr (Maybe EgisonTypeExpr)
  | LetExpr Bindings EgisonExpr (Maybe EgisonTypeExpr)
  | LetRecExpr RecursiveBindings EgisonExpr (Maybe EgisonTypeExpr)

  | TypeExpr TypeInfoExpr (Maybe EgisonTypeExpr)
  | ClassExpr ClassInfoExpr (Maybe EgisonTypeExpr)

  | MatchExpr EgisonExpr EgisonExpr [MatchClause] (Maybe EgisonTypeExpr)
  | MatchAllExpr EgisonExpr EgisonExpr MatchClause (Maybe EgisonTypeExpr)

  | LoopExpr String String EgisonExpr EgisonExpr EgisonExpr (Maybe EgisonTypeExpr)
  | DoExpr Bindings EgisonExpr (Maybe EgisonTypeExpr)

  | ApplyExpr EgisonExpr EgisonExpr (Maybe EgisonTypeExpr)

  | SomethingExpr (Maybe EgisonTypeExpr)
  | UndefinedExpr (Maybe EgisonTypeExpr)
 deriving (Show)

data EgisonTypedExpr =
    CharTypedExpr Char EgisonType
  | StringTypedExpr String EgisonType
  | BoolTypedExpr Bool EgisonType
  | IntegerTypedExpr Integer EgisonType
  | FloatTypedExpr Double EgisonType
  | PatVarTypedExpr String [EgisonTypedExpr] EgisonType
  | VarTypedExpr String [EgisonTypedExpr] EgisonType
  | SymbolTypedExpr String EgisonType
  | PatVarOmitTypedExpr String [EgisonTypedExpr] EgisonType
  | VarOmitTypedExpr String [EgisonTypedExpr] EgisonType

  | WildCardTypedExpr EgisonType
  | ValuePatTypedExpr EgisonTypedExpr EgisonType
  | CutPatTypedExpr EgisonTypedExpr EgisonType
  | NotPatTypedExpr EgisonTypedExpr EgisonType
  | AndPatTypedExpr [EgisonTypedExpr] EgisonType
  | OrPatTypedExpr [EgisonTypedExpr] EgisonType
  | PredPatTypedExpr EgisonTypedExpr [EgisonTypedExpr] EgisonType

  | InductiveDataTypedExpr String [EgisonTypedExpr] EgisonType
  | TupleTypedExpr [EgisonTypedExpr] EgisonType
  | CollectionTypedExpr [InnerTypedExpr] EgisonType
  | FuncTypedExpr EgisonTypedExpr EgisonTypedExpr EgisonType

  | IfTypedExpr EgisonTypedExpr EgisonTypedExpr EgisonTypedExpr EgisonType
  | LetTypedExpr Bindings EgisonTypedExpr EgisonType
  | LetRecTypedExpr RecursiveBindings EgisonTypedExpr EgisonType

  | TypeTypedExpr TypeInfoExpr EgisonType
  | ClassTypedExpr ClassInfoExpr EgisonType

  | MatchTypedExpr EgisonTypedExpr EgisonTypedExpr [MatchClause] EgisonType
  | MatchAllTypedExpr EgisonTypedExpr EgisonTypedExpr MatchClause EgisonType

  | LoopTypedExpr String String EgisonTypedExpr EgisonTypedExpr EgisonTypedExpr EgisonType
  | DoTypedExpr Bindings EgisonTypedExpr EgisonType

  | ApplyTypedExpr EgisonTypedExpr EgisonTypedExpr EgisonType

  | SomethingTypedExpr EgisonType
  | UndefinedTypedExpr EgisonType
 deriving (Show)

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

type Bindings = [(EgisonExpr, EgisonExpr)]

type RecursiveBindings = [(String, EgisonExpr)]
  
type TypeInfoExpr = [(PrimitivePatPattern, EgisonExpr, [(PrimitivePattern, EgisonExpr)])]

type ClassInfoExpr = [(String, EgisonTypeExpr)]

--
-- Value
--
type ObjectRef = IORef Object

data Object =
    Closure Env EgisonExpr
  | Pattern EgisonPattern
  | Intermidiate EgisonIntermidiate
  | Value EgisonVal
  
data EgisonPattern =
    WildCard
  | PatVar String [Integer]
  | ValuePat EgisonExpr
  | PredPat EgisonExpr [EgisonExpr]
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

data EgisonVal =
    World [Action]
  | Char Char
  | String String
  | Bool Bool
  | Integer Integer
  | Float Double
  | InductiveData String [EgisonVal]
  | Tuple [EgisonVal]
  | Collection [EgisonVal]
  | Type TypeInfo
  | Class ClassInfo
  | Func ObjectRef EgisonExpr Env
  | PrimitiveFunc ([EgisonVal] -> ThrowsError EgisonVal)
  | IOFunc ([EgisonVal] -> IOThrowsError EgisonVal)
  | Port String Handle
  | Something
  | EOF

data Action =
    OpenInputPort String
  | OpenOutputPort String
  | ClosePort String
  | FlushPort String
  | ReadFromPort String String
  | WriteToPort String String

data Args =
    AVar String
  | ATuple [Args]
 deriving (Show)
  
data InnerObject =
    IElement ObjectRef
  | ISubCollection ObjectRef

type TypeInfo = [(PrimitivePatPattern, ObjectRef, [(Env, PrimitivePattern, EgisonExpr)])]
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
