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

  | TupleTypeExpr [EgisonTypeExpr]
  | CollectionTypeExpr EgisonTypeExpr
  | FunTypeExpr EgisonTypeExpr EgisonTypeExpr

  | PatVarTypeExpr String
  | VarTypeExpr String
        
data EgisonExpr =
    CharExpr Char
  | StringExpr String
  | BoolExpr Bool
  | IntegerExpr Integer
  | FloatExpr Double
  | PatVarExpr String [EgisonExpr]
  | VarExpr String [EgisonExpr]
  | SymbolExpr String
  | PatVarOmitExpr String [EgisonExpr]
  | VarOmitExpr String [EgisonExpr]

  | WildCardExpr
  | ValuePatExpr EgisonExpr
  | CutPatExpr EgisonExpr
  | NotPatExpr EgisonExpr
  | AndPatExpr [EgisonExpr]
  | OrPatExpr [EgisonExpr]
  | PredPatExpr EgisonExpr [EgisonExpr]

  | InductiveDataExpr String [EgisonExpr]
  | TupleExpr [EgisonExpr]
  | CollectionExpr [InnerExpr]
  | FuncExpr EgisonExpr EgisonExpr

  | IfExpr EgisonExpr EgisonExpr EgisonExpr
  | LetExpr Bindings EgisonExpr
  | LetRecExpr RecursiveBindings EgisonExpr

  | TypeExpr DestructInfoExpr
  | MatchExpr EgisonExpr EgisonExpr [MatchClause]
  | MatchAllExpr EgisonExpr EgisonExpr MatchClause

  | LoopExpr String String EgisonExpr EgisonExpr EgisonExpr
  | DoExpr Bindings EgisonExpr

  | ApplyExpr EgisonExpr EgisonExpr

  | SomethingExpr
  | UndefinedExpr
 deriving (Show)
 
type ArgsExpr = Args
               
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
  
type DestructInfoExpr = [(PrimitivePatPattern, EgisonExpr, [(PrimitivePattern, EgisonExpr)])]

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
  | Type DestructInfo
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

type DestructInfo = [(PrimitivePatPattern, ObjectRef, [(Env, PrimitivePattern, EgisonExpr)])]

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
