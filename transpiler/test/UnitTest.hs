module UnitTest (main) where
-- This is a unit test suite for testing the functionality of the project code

import GoAST
import Parser

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec
import Text.Parsec.Indent

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Unit Tests" [parserTests] -- put test trees here

-- For doing negative testing (taken from AP exam 2020)
testCaseBad s t =
  testCase ("*" ++ s) $
    case t of
      Right a -> assertFailure $ "Unexpected success:" ++ show a
      Left _ -> return ()

--------------------
-- Testing Parser --
--------------------

parserTests :: TestTree
parserTests =
 testGroup "Parser Tests"
  [
-- Parse Names
    testCase "Names: names start with letter"
     $ runIndentParser pName () "" "name"
     @?= Right "name",
    testCaseBad "Names: parsing name not starting with letter"
     $ runIndentParser pName () "" "5name",
    testCase "Names: can contain letters, digits and dots"
     $ runIndentParser pName () "" "n4m.e"
     @?= Right "n4m.e",
    testCase "Names: can be loooong"
     $ runIndentParser pName () "" "n4m.eNnn4m.E4m.e4m.e"
     @?= Right "n4m.eNnn4m.E4m.e4m.e",
    testCase "Names: can be short"
     $ runIndentParser pName () "" "a"
     @?= Right "a",
    testCase "Names: cannot contain other characters"
     $ runIndentParser pName () "" "h#Jm_d)1@"
     @?= Right "h",
    testCaseBad "Names: cannot be reserved words"
     $ runIndentParser pName () "" "BOOL",
-- Parse Digits
    testCase "Digits: consist of numerics"
     $ runIndentParser pDigit () "" "2"
     @?= Right 2,
    testCase "Digits: can contain many digits"
     $ runIndentParser pDigit () "" "1234567890"
     @?= Right 1234567890,
    testCase "Digits: can contain only digits"
     $ runIndentParser pDigit () "" "123a567890"
     @?= Right 123,
-- Parse Hex.Digits
    testCase "HexDigits: consist of characters 123456789ABCDEF"
     $ runIndentParser pHex () "" "A"
     @?= Right "A",
    testCase "HexDigits: can contain many characters"
     $ runIndentParser pHex () "" "123456789ABCDEF"
     @?= Right "123456789ABCDEF",
    testCase "HexDigits: can contain both upper and lower case letters"
     $ runIndentParser pHex () "" "1A2b"
     @?= Right "1A2b",
    testCase "HexDigits: can contain only the characters 123456789ABCDEF"
     $ runIndentParser pHex () "" "123#567890"
     @?= Right "123",
-- Parse Characters
    testCase "Characters: parsing *\'"
     $ runIndentParser pChar () "" "*\'"
     @?= Right '\'',
    testCase "Characters: parsing *\""
     $ runIndentParser pChar () "" "*\""
     @?= Right '\"',
    testCase "Characters: parsing **"
     $ runIndentParser pChar () "" "**"
     @?= Right '*',
    testCase "Characters: parsing *n"
     $ runIndentParser pChar () "" "*n"
     @?= Right '\n',
    testCase "Characters: parsing *N"
     $ runIndentParser pChar () "" "*N"
     @?= Right '\n',
    testCase "Characters: parsing *t"
     $ runIndentParser pChar () "" "*t"
     @?= Right '\t',
    testCase "Characters: parsing *T"
     $ runIndentParser pChar () "" "*T"
     @?= Right '\t',
    testCase "Characters: parsing *s"
     $ runIndentParser pChar () "" "*s"
     @?= Right ' ',
    testCase "Characters: parsing *S"
     $ runIndentParser pChar () "" "*S"
     @?= Right ' ',
    testCase "Characters: parsing letters normally"
     $ runIndentParser pChar () "" "a"
     @?= Right 'a',
    testCase "Characters: parsing digits normally"
     $ runIndentParser pChar () "" "5"
     @?= Right '5',
    testCase "Characters: can be all printable ASCII characters"
     $ runIndentParser pChar () "" "#"
     @?= Right '#',
    testCaseBad "Characters: cannot be non-printable ASCII characters"
     $ runIndentParser pChar () "" "\10",
    testCaseBad "Characters: cannot be single *"
     $ runIndentParser pChar () "" "*",
    testCaseBad "Characters: cannot be single \'"
     $ runIndentParser pChar () "" "\'",
    testCaseBad "Characters: cannot be single \""
     $ runIndentParser pChar () "" "\"",
-- Parse Strings
    testCase "Strings: are enclosed in double quotes"
     $ runIndentParser pString () "" "\"Hello World!\""
     @?= Right "Hello World!",
    testCaseBad "Strings: can only contain characters"
     $ runIndentParser pString () "" "\"Hello** W\'orld!\"",
-- Parse Data Types
    testCase "Data types: data types can be BOOL"
     $ runIndentParser parseDType () "" "BOOL"
     @?= Right BOOL,
    testCase "Data types: data types can be BYTE"
     $ runIndentParser parseDType () "" "BYTE"
     @?= Right BYTE,
    testCase "Data types: data types can be INT"
     $ runIndentParser parseDType () "" "INT"
     @?= Right INT,
    testCase "Data types: data types can be 1D arrays"
     $ runIndentParser parseDType () "" "[2]INT"
     @?= Right (DArray [Const (IntVal 2)] INT),
    testCase "Data types: data types can be multi dimensional arrays"
     $ runIndentParser parseDType () "" "[2][3][4]INT"
     @?= Right (DArray [Const (IntVal 2), Const (IntVal 3), Const (IntVal 4)] INT),
    testCaseBad "Data types: data types are case sensitive"
     $ runIndentParser parseDType () "" "int",
-- Parse Specifiers
    testCase "Specifiers: can be of type CHAN OF data type"
     $ runIndentParser parseSpecifier () "" "CHAN OF INT"
     @?= Right (SChan INT),
    testCase "Specifiers: can be data type"
     $ runIndentParser parseSpecifier () "" "INT"
     @?= Right (SVar INT),
-- Parse variables, channels, and lists of variables
    testCase "Var: can parse name of variable"
     $ runIndentParser parseVar () "" "some.variable"
     @?= Right (Var "some.variable"),
    testCase "Var: can parse list of variable seperated by comma"
     $ runIndentParser parseVars () "" "a,b ,c, d , e"
     @?= Right [Var "a", Var "b", Var "c", Var "d", Var "e"],
    testCase "Chan: can parse name of channel"
     $ runIndentParser parseChan () "" "some.channel"
     @?= Right (Chan "some.channel"),
-- Parse Expressions
    testCase "Expressons: can be conversion"
     $ runIndentParser parseExp () "" "BYTE a"
     @?= Right (Conv BYTE (Var "a")),
    testCase "Expressons: can be negations using NOT"
     $ runIndentParser parseExp () "" "NOT a"
     @?= Right (Not (Var "a")),
    testCase "Expressons: can be binary operation \'+\'"
     $ runIndentParser parseExp () "" "a + b"
     @?= Right (Oper Plus (Var "a") (Var "b")),
    testCase "Expressons: can be binary operation \'-\'"
     $ runIndentParser parseExp () "" "a - b"
     @?= Right (Oper Minus (Var "a") (Var "b")),
    testCase "Expressons: can be binary operation \'*\'"
     $ runIndentParser parseExp () "" "a * b"
     @?= Right (Oper Times (Var "a") (Var "b")),
    testCase "Expressons: can be binary operation \'/\'"
     $ runIndentParser parseExp () "" "a / b"
     @?= Right (Oper Div (Var "a") (Var "b")),
    testCase "Expressons: can be binary operation \'\\\'"
     $ runIndentParser parseExp () "" "a \\ b"
     @?= Right (Oper Mod (Var "a") (Var "b")),
    testCase "Expressons: can be boolean operation \'=\'"
     $ runIndentParser parseExp () "" "a = b"
     @?= Right (Oper Eq (Var "a") (Var "b")),
    testCase "Expressons: can be boolean operation \'<>\'"
     $ runIndentParser parseExp () "" "a <> b"
     @?= Right (Oper Neq (Var "a") (Var "b")),
    testCase "Expressons: can be boolean operation \'<\'"
     $ runIndentParser parseExp () "" "a < b"
     @?= Right (Oper Less (Var "a") (Var "b")),
    testCase "Expressons: can be boolean operation \'>\'"
     $ runIndentParser parseExp () "" "a > b"
     @?= Right (Oper Greater (Var "a") (Var "b")),
    testCase "Expressons: can be boolean operation \'>=\'"
     $ runIndentParser parseExp () "" "a >= b"
     @?= Right (Oper Geq (Var "a") (Var "b")),
    testCase "Expressons: can be boolean operation \'<=\'"
     $ runIndentParser parseExp () "" "a <= b"
     @?= Right (Oper Leq (Var "a") (Var "b")),
    testCase "Expressons: can be boolean operation \'AND\'"
     $ runIndentParser parseExp () "" "a AND b"
     @?= Right (Oper And (Var "a") (Var "b")),
    testCase "Expressons: can be boolean operation \'OR\'"
     $ runIndentParser parseExp () "" "a OR b"
     @?= Right (Oper Or (Var "a") (Var "b")),
    testCase "Expressons: can be single operand"
     $ runIndentParser parseExp () "" "a"
     @?= Right (Var "a"),
    testCase "Expressions: can be nested using parentheses"
     $ runIndentParser parseExp () "" "2*(3-4)"
     @?= Right (Oper Times (Const (IntVal 2)) (Oper Minus (Const (IntVal 3)) (Const (IntVal 4)))),
    testCase "Expressons: parsing list of exps, seperated by \',\'"
     $ runIndentParser parseExps () "" "a, 5+5, NOT hej, BOOL b, a = b"
     @?= Right [Var "a", 
                Oper Plus (Const (IntVal 5)) (Const (IntVal 5)), 
                Not (Var "hej"), 
                Conv BOOL (Var "b"), 
                Oper Eq (Var "a") (Var "b")],
-- Parse Operands
    testCase "Operands: can be variable"
     $ runIndentParser parseOperand () "" "a"
     @?= Right (Var "a"),
    testCase "Operands: can be function calls"
     $ runIndentParser parseOperand () "" "fib(5)"
     @?= Right (Call "fib" [Const (IntVal 5)]),
    testCase "Operands: can be digits"
     $ runIndentParser parseOperand () "" "5"
     @?= Right (Const (IntVal 5)),
    testCase "Operands: can be hex digits"
     $ runIndentParser parseOperand () "" "#5F"
     @?= Right (Const (HexVal "5F")),
    testCase "Operands: can be byte values"
     $ runIndentParser parseOperand () "" "*#5F"
     @?= Right (Const (ByteVal "5F")),
    testCase "Operands: can be TRUE"
     $ runIndentParser parseOperand () "" "TRUE"
     @?= Right (Const TrueVal),
    testCase "Operands: can be FALSE"
     $ runIndentParser parseOperand () "" "FALSE"
     @?= Right (Const FalseVal),
    testCase "Operands: can be bytes"
     $ runIndentParser parseOperand () "" "\'c\'"
     @?= Right (Const (ByteVal "c")),
    testCase "Operands: can be lists of expressions"
     $ runIndentParser parseOperand () "" "[a, 5+5, NOT hej]"
     @?= Right (Array [Var "a", 
                      Oper Plus (Const (IntVal 5)) (Const (IntVal 5)), 
                      Not (Var "hej")]), 
    testCase "Operands: can be expressions enclosed in parentheses"
     $ runIndentParser parseOperand () "" "(5+5)"
     @?= Right (Oper Plus (Const (IntVal 5)) (Const (IntVal 5))),
-- Parse Process
---- Assignment
    testCase "Process: assignment of one variable"
     $ runIndentParser parseProcess () "" "a := 1"
     @?= Right (SDef [(Var "a")] [Const (IntVal 1)]),
    testCase "Process: assignment of multiple variables"
     $ runIndentParser parseProcess () "" "a, b ,c := 1, 2 ,3"
     @?= Right (SDef [Var "a", Var "b", Var "c"]
                     [Const (IntVal 1), Const (IntVal 2), Const (IntVal 3)]),
---- Input
    testCase "Process: reading input from channel"
     $ runIndentParser parseProcess () "" "c ? a"
     @?= Right (SReceive (Var "a") (Chan "c")),
---- Output
    testCase "Process: writing output to channel"
     $ runIndentParser parseProcess () "" "c ! a"
     @?= Right (SSend (Chan "c") (Var "a")),
---- SKIP
    testCase "Process: parsing SKIP"
     $ runIndentParser parseProcess () "" "SKIP"
     @?= Right SContinue,
---- STOP
    testCase "Process: parsing STOP"
     $ runIndentParser parseProcess () "" "STOP"
     @?= Right SExit,
---- Sequence
    testCase "Process: a SEQ, followed by an indented block"
     $ runIndentParser parseProcess () "" "SEQ \n  SKIP"
     @?= Right (SSeq "seq" [SContinue]),
    testCaseBad "Process: SEQ only parses correctly indented blocks"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" "SEQ \n  SKIP\nSTOP",
    testCase "Process: sequences can be nested"
     $ runIndentParser parseProcess () "" "SEQ \n  SEQ \n    SKIP\n  SEQ\n    STOP"
     @?= Right (SSeq "seq" [SSeq "seq" [SContinue], SSeq "seq" [SExit]]),
---- Conditional
    testCase "Process: IF, followed by indented block of Cond"
     $ runIndentParser parseProcess () "" "IF \n  a = 5\n    SKIP"
     @?= Right (SIf "if" [IfCase (Oper Eq (Var "a") (Const (IntVal 5))) SContinue]),
    testCase "Process: IF with multiple Conds"
     $ runIndentParser parseProcess () "" "IF \n  a >= 5\n    SKIP\n  a < 5\n    a := a + 1"
     @?= Right (SIf "if" [IfCase (Oper Geq (Var "a") (Const (IntVal 5))) SContinue,
                          IfCase (Oper Less (Var "a") (Const (IntVal 5))) 
                                (SDef [Var "a"] [Oper Plus (Var "a") (Const (IntVal 1))])]),
    testCase "Process: IF with TRUE as a condition"
     $ runIndentParser parseProcess () "" "IF \n  a >= 5\n    SKIP\n  TRUE\n    a := a + 1"
     @?= Right (SIf "if" [IfCase (Oper Geq (Var "a") (Const (IntVal 5))) SContinue,
                          IfCase (Const TrueVal) 
                                (SDef [Var "a"] [Oper Plus (Var "a") (Const (IntVal 1))])]),
    testCaseBad "Process: IF can only have one process per choice"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "IF \n  a = 5\n    b := 1\n    SKIP",
---- Selection
    testCase "Process: CASE, followed by exp and indented block of Options"
     $ runIndentParser parseProcess () "" "CASE dir\n  up\n    x := x + 1\n  down\n    x := x - 1"
     @?= Right (SSwitch (Var "dir") 
                        [SwitchCase [Var "up"] 
                                    (SDef [Var "x"] [Oper Plus (Var "x") (Const (IntVal 1))]),
                         SwitchCase [Var "down"] 
                                    (SDef [Var "x"] [Oper Minus (Var "x") (Const (IntVal 1))])]),
    testCase "Process: CASE with multiple matching expressions"
     $ runIndentParser parseProcess () "" 
                       "CASE l\n  'a','b','c'\n    x := TRUE\n  ELSE\n    x := FALSE"
     @?= Right (SSwitch (Var "l") 
                        [SwitchCase [Const (ByteVal "a"), Const (ByteVal "b"), Const (ByteVal "c")] 
                                    (SDef [Var "x"] [Const TrueVal]),
                         SwitchCase [Const TrueVal] (SDef [Var "x"] [Const FalseVal])]),
    testCaseBad "Process: CASE can have only one process per option"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "CASE dir\n  up\n    x := x + 1\n    x := x - 1",
---- Loop
    testCase "Process: WHILE, followed by boolean expression and indented process"
     $ runIndentParser parseProcess () "" "WHILE a <> 0\n  SEQ\n    a := a - 1"
     @?= Right (SWhile (Oper Neq (Var "a") (Const (IntVal 0))) 
                       (SSeq "seq" [SDef [Var "a"] [Oper Minus (Var "a") (Const (IntVal 1))]])),
    testCaseBad "Process: WHILE can be followed by exactly one process"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "WHILE a <> 0\n  a := a - 1\n  b := a + 1",
---- Parallel
    testCase "Process: PAR, followed by indented block of processes"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "PAR\n  multiply(5)\n  SEQ\n    c1 ! 1\n    c2 ? a"
     @?= Right (SGo "par" [SCall (Call "multiply" [Const (IntVal 5)]),
                           SSeq "seq" [SSend (Chan "c1") (Const (IntVal 1)),
                                       SReceive (Var "a") (Chan "c2")]]),
---- Alternation
    testCase "Process: ALT where alternative guard is input"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT\n  c1 ? a\n    c2 ! a + 1"
     @?= Right (SSelect "alt" [SCase (SelectCase (Const TrueVal, SReceive (Var "a") (Chan "c1")) 
                                          (SSend (Chan "c2") (Oper Plus (Var "a") (Const (IntVal 1)))))]),
    testCase "Process: ALT where alternative guard is 'boolean & input'"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT\n  true & c1 ? a\n    c2 ! a + 1"
     @?= Right (SSelect "alt" [SCase (SelectCase (Var "true", SReceive (Var "a") (Chan "c1"))
                                          (SSend (Chan "c2") (Oper Plus (Var "a") (Const (IntVal 1)))))]),
    testCase "Process: ALT where alternative guard is 'boolean & SKIP'"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT\n  true & SKIP\n    c2 ! a + 1"
     @?= Right (SSelect "alt" [SCase (SelectCase (Var "true", SContinue) 
                                          (SSend (Chan "c2") (Oper Plus (Var "a") (Const (IntVal 1)))))]),
    testCase "Process: ALT with multiple alternatives"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT\n  true & SKIP\n    c2 ! a + 1\n  c1 ? a\n    SKIP"
     @?= Right (SSelect "alt" [SCase (SelectCase (Var "true", SContinue) 
                                          (SSend (Chan "c2") (Oper Plus (Var "a") (Const (IntVal 1))))),
                               SCase (SelectCase (Const TrueVal, SReceive (Var "a") (Chan "c1")) SContinue)]),
    testCaseBad "Process: ALT can only have one process per alternative"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT\n  true & c1 ? a\n    c2 ! a + 1\n    SKIP"
---- Specification with declaration
--    Processes can be assignment, input/output, skip, stop, sequence, conditional (if),
--    selection (case), loop (while), parallel, alternation, declaration (and definition??)
  ]   
-- Parse Program
--    Programs consist of definitions
-- Parse Definition
--    Definitions can be a lot of thing, but in this subset they are Procedures
-- Parse Procedure
--    Procedures have a Name, a list of arguments, and a body og processes
-- Comments
-- Whitespace
-- Indentation
