-- Abstract Syntax Tree for Go

module GoAST where

data Val =
    TrueVal | FalseVal
  | IntVal Int
  | RealVal Float
  | HexVal String
  | ByteVal String
  | StringVal String
  | ListVal [Val]
  deriving (Eq, Show, Read)

data Exp =
    Const Val
  | Var VName
  | Oper Op Exp Exp
  | Not Exp
  | List [Exp]
  | Conv DType Exp -- conversion, fx BYTE x for converting x to byte
  deriving (Eq, Show, Read)

data Cond =
    CFor
  | CEq 
  | CAnd
  | COr
  | CNot
  | CIfElse
  | CSwitch
  | CSelect
  deriving (Eq, Show, Read) 

data Stmt =
    SDef [Exp] [Exp]
  | SDecl [Exp] Spec
  | SCond Cond
  | SGo FName [Exp]
  | SSend VName Exp
  | SReceive [VName] VName
  deriving (Eq, Show, Read)

data Spec =
    Data DType
  | Chan DType
  deriving (Eq, Show, Read)

data DType = BOOL | BYTE | INT | INT16 | INT32 | INT64 | REAL32 | REAL64
  | DVar VName
  | DArray [Int] DType
  deriving (Eq, Show, Read)

type VName = String
type FName = String

-- Simplified subset of dyadic operators
data Op = Plus | Minus | Times | Div | Mod | Eq | Neq | Less | Greater | Geq | Leq | And | Or
  deriving (Eq, Show, Read)

type Program = [Fun]

data Fun = FFun (FName, FArgs, [Spec]) Body
  deriving(Eq, Show, Read)

type FArgs = [FArg]

data FArg = Arg [Exp] Spec
  deriving (Eq, Show, Read)

type Body = [Stmt]

