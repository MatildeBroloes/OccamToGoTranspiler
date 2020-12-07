-- Abstract Syntax Tree for Go

module GoAST where

data Val =
    TrueVal | FalseVal
  | IntVal Int
  | StringVal String
  | ListVal [Val]
  deriving (Eq, Show, Read)

data Exp =
    Const Val
  | Var VName
  | Oper Op Exp Exp
  deriving (Eq, Show, Read)

data Cond =
    CFor
  | CIfElse
  | CSwitch
  | CSelect
  deriving (Eq, Show, Read) 

data Stmt =
    SDef Vname Spec Exp
  | SCond Cond
  | SGo FName [Exp]
  | SSend VName Exp
  | SReceive [VName] Vname
  deriving (Eq, Show, Read)

type VName = String
type FName = String

type Program = [Fun]

data Fun = FFun FName FArgs [Spec] Body
  deriving(Eq, Show, Read)

type FArgs = [FArg]

data FArg = Arg [VName] Spec
  deriving (Eq, Show, Read)

type Body = [Stmt]
