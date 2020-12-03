-- Abstract Syntax Tree for Go

module GoAST where

data Value =
    TrueVal | FalseVal
  | IntVal Int
  | StringVal String
  | ListVal [Value]
  deriving (Eq, Show, Read)

data Variable =
    Var VName Value
  | SVar VName Specifier Value
  | ZVar VName Specifier
  deriving (Eq, Show, Read)

data Exp =
    Const Value
  | List [Exp] 
  deriving (Eq, Shoe, Read)

data CExp =
    CFor VName Exp -- ??
  | CIf Exp Exp
  | CIfElse Exp Exp Exp
  deriving (Eq, Show, Read)

type VName = String

type Program = [Statement]

data Statement =
    SDef Definition
  | SFun Function
  deriving (Eq, Show, Read)
