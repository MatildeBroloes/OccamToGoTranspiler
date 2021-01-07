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
  | Guard Exp Stmt
  | Call FName [Exp]
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

data Stmt = -- here, a statement is almost equivalent to a process in Occam
    SDef [Exp] [Exp]
  | SDecl [Exp] Spec Stmt
  | SSeq String [Stmt] -- evt lav disse til 1 type, hvor String siger hvilken slags
  | SIf String [Stmt]
  | SSwitch Exp [Stmt] -- a selector, and a number of options
  | SWhile Exp [Stmt] -- a boolean and a process
  | SCond Exp [Stmt] -- a boolean and a process
  | SGo String [Stmt]
  | SCall Exp
  | SSelect String [Stmt]
  | SCase Stmt [Stmt] -- for cases in a select that are not just conditions?
  | SSend Exp Exp -- first exp is where to send, second is message
  | SReceive [Exp] Exp -- first is where to save message (can be NoneVal), second is which channel
  | SContinue
  | SExit
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

