-- Abstract Syntax Tree for Go

module GoAST where

data Val =
    TrueVal | FalseVal | NoneVal
  | IntVal Int
  | HexVal String
  | ByteVal Val
  | CharVal Char
  | StringVal String
  deriving (Eq, Show, Read)

data Exp =
    Const Val
  | Var VName
  | Chan VName
  | Oper Op Exp Exp
  | Call FName [Exp]
  | Not Exp
  | Array [Exp]
  | Slice VName [Exp]
  | Conv DType Exp
  deriving (Eq, Show, Read)

data Stmt = 
    SDef [Exp] [Exp]
  | SDecl [Exp] Spec Stmt
  | SSeq [Stmt]
  | SIf [Case]
  | SSwitch Exp [Case]
  | SGo [Stmt]
  | SSelect [Stmt]
  | SWhile Exp Stmt
  | SFor Exp Exp Exp Stmt
  | SCase Case
  | SCall Exp
  | SSend Exp Exp
  | SReceive Exp Exp
  | SContinue
  | SExit
  deriving (Eq, Show, Read)

data Case =
    IfCase Exp Stmt
  | SwitchCase [Exp] Stmt
  | SelectCase (Exp, Stmt) Stmt
  deriving (Eq, Show, Read)

data Spec =
    SVar DType
  | SChan DType
  deriving (Eq, Show, Read)

data DType = BOOL | BYTE | INT
  | DArray [Exp] DType
  | DChan DType
  deriving (Eq, Show, Read)

type VName = String
type FName = String

-- Simplified subset of dyadic operators
data Op = Plus | Minus | Times | Div | Mod | Eq | Neq | Less | Greater | Geq | Leq | Rem | And | Or
  deriving (Eq, Show, Read)

type Program = [Fun]

data Fun = FFun FName FArgs [Spec] Stmt
  deriving(Eq, Show, Read)

type FArgs = [FArg]

data FArg = Arg [Exp] Spec
  deriving (Eq, Show, Read)
