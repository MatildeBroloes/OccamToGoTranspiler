-- Abstract Syntax Tree for Go

module GoAST where

data Val =
    TrueVal | FalseVal | NoneVal
  | IntVal Int
--  | RealVal Float
  | HexVal String
  | ByteVal String
  | StringVal String
 -- | ListVal [Val]
  deriving (Eq, Show, Read)

data Exp =
    Const Val
  | Var VName
  | Chan VName
  | Oper Op Exp Exp
  | Call FName [Exp]
  | Not Exp
  | Array [Exp]
  | Conv DType Exp -- conversion, fx BYTE x for converting x to byte
  deriving (Eq, Show, Read)

data Stmt = -- here, a statement is almost equivalent to a process in Occam
    SDef [Exp] [Exp]
  | SDecl [Exp] Spec Stmt
  | SSeq String [Stmt] -- evt lav disse til 1 type, hvor String siger hvilken slags
  | SIf String [Cond]
  | SSwitch Exp [Cond] -- a selector, and a number of options
  | SGo String [Stmt]
  | SSelect String [Stmt]
  | SWhile Exp Stmt -- a boolean and a process
  | SFor Exp Exp Exp Stmt -- for replicated processes. index name(variable), base, count, process
  | SCase Cond -- for cases in a select or switch
  | SCall Exp
  | SSend Exp Exp -- first exp is where to send, second is message
  | SReceive Exp Exp -- first is where to save message (can be NoneVal), second is which channel
  | SContinue
  | SExit
  deriving (Eq, Show, Read)

data Cond =
    IfCase Exp Stmt
  | SwitchCase [Exp] Stmt
  | SelectCase (Exp, Stmt) Stmt -- (boolean, input)
  deriving (Eq, Show, Read)

data Spec =
    SVar DType
  | SChan DType
  deriving (Eq, Show, Read)

data DType = BOOL | BYTE | INT | INT16 | INT32 | INT64 | REAL32 | REAL64
--  | DVar VName
  | DArray [Exp] DType
  deriving (Eq, Show, Read)

type VName = String
type FName = String

-- Simplified subset of dyadic operators
data Op = Plus | Minus | Times | Div | Mod | Eq | Neq | Less | Greater | Geq | Leq | And | Or
  deriving (Eq, Show, Read)

type Program = [Fun]

data Fun = FFun FName FArgs [Spec] Stmt
  deriving(Eq, Show, Read)

type FArgs = [FArg]

data FArg = Arg [Exp] Spec
  deriving (Eq, Show, Read)
