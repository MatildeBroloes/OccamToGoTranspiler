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

--data Cond =
--    CEq Exp Exp
--  | CNeq Exp Exp
--  | CLeq Exp Exp
--  | CGeq Exp Exp
--  | CLess Exp Exp
--  | CGreater Exp Exp
--  | CAnd Exp Exp
--  | COr Exp Exp
--  | CNot Exp
--  | CFor -- for replicators?
--  | CIf -- there cannot be else in occam, but there can in go
--  | CSwitch
--  | CSelect -- svarer til en select?
--  deriving (Eq, Show, Read) 

data Stmt =
    SDef [Exp] [Exp]
  | SDecl [Exp] Spec
  | SSeq String [Stmt] -- evt lav disse til 1 type, hvor String siger hvilken slags
  | SIf String [Stmt]
  | SSwitch Exp [Stmt] -- a selector, and a number of options
  | SWhile Exp [Stmt] -- a boolean and a process
  | SCond Exp [Stmt] -- a boolean and a process
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

