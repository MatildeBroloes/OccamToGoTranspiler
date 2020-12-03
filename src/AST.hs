-- Abstract Syntax Tree for Occam

module AST where

data Definition = 
    Proc Name [Formal] Process
  deriving (Eq, Show, Read)

data Process =
    Asign [Variable] [Exp]
  | In Name [Variable]
  | Out Name [Exp]
  | SKIP
  | STOP
  | Seq Sequence
  | Con Conditional
  deriving (Eq, Show, Read)

data Sequence =
    SList [Process]
  | SFor Name Exp Exp Process -- Replicator = Name Exp FOR Exp
  deriving (Eq, Show, Read)

data Conditional =
    IfList [Choice]
  | IfFor Name Exp Exp Choice
  deriving (Eq, Show, Read)

data Choice =
    GChoice Exp Process
  | CChoice Conditional
  | SChoice Specification Choice
  deriving (Eq, Show, Read)

data Declaration =
    DData DataType [Name]
  | DChannel ChannelType [Name]
  -- timer and port
  deriving (Eq, Show, Read)

--data Abbreviation =
--    IsVar Name Variable
--  | IsVarOfType Name Variable Specifier
--  | IsChan Name Channel
--  | IsChanOfType Name Channel Specifier
--  | IsChans Name [Channel]
--  | IsChansOfType Name [Channel] Specifier
--  deriving (Eq, Show, Read)

data Specification =
    Dec Declaration
--  | Abb Abbreviation
  | Def Definition
  deriving (Eq, Show, Read)

--data Operand =
--    Variable
--  | List Name [Exp]
--  | Exp
--  deriving (Eq, Show, Read)

data Specifier = DType DataType | CType ChannelType
  deriving (Eq, Show, Read)

data ChannelType = ChanOf Protocol
  | CList [Exp] ChannelType
  deriving (Eq, Show, Read)

data DataType = BOOL | BYTE | INT | INT16 | INT32 | INT64 | REAL32 | REAL64 | DVar Name
  | DList [Exp] DataType
  deriving (Eq, Show, Read)

data Formal =
-- ZVar standing for zero-valued variable(s)
    ZVar [Variable] Specifier
  deriving (Eq, Show, Read)

type Name = String
type Channel = Name -- To be changed later
             
data Exp =
    EVar Variable
  | List Name [Exp]
--    Oper Operand
  deriving (Eq, Show, Read)

data Variable = Var Name
  deriving (Eq, Show, Read)

data Protocol = 
    ProtName Name
  | Simple DataType
  deriving (Eq, Show, Read)

--data Input =
--    Chan Channel [Variable]
--  deriving (Eq, Show, Read)
--
--data Output =
--    Chan Channel [Exp]
