module UnitTest (main) where
-- This is a unit test suite for testing the functionality of the project code

import GoAST
import Parser
import Generator

import Test.Tasty
import Test.Tasty.HUnit

import Text.Parsec
import Text.Parsec.Indent

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Unit Tests" [parserTests, generatorTests]

parserTests :: TestTree
parserTests = testGroup "Parser Tests" [parsertests]

-- For doing negative testing (taken from AP exam 2020)
testCaseBad s t =
  testCase ("*" ++ s) $
    case t of
      Right a -> assertFailure $ "Unexpected success:" ++ show a
      Left _ -> return ()

--------------------
-- Testing Parser --
--------------------

parsertests :: TestTree
parsertests =
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
    testCase "Expressons: can be array slice"
     $ runIndentParser parseExp () "" "a[i]"
     @?= Right (Slice "a" [Var "i"]),
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
     @?= Right (Const (ByteVal (IntVal 95))),
    testCase "Operands: can be TRUE"
     $ runIndentParser parseOperand () "" "TRUE"
     @?= Right (Const TrueVal),
    testCase "Operands: can be FALSE"
     $ runIndentParser parseOperand () "" "FALSE"
     @?= Right (Const FalseVal),
    testCase "Operands: can be bytes"
     $ runIndentParser parseOperand () "" "\'c\'"
     @?= Right (Const (ByteVal (CharVal 'c'))),
    testCase "Operands: can be lists of expressions"
     $ runIndentParser parseOperand () "" "[a, 5+5, NOT hej]"
     @?= Right (Array [Var "a", 
                      Oper Plus (Const (IntVal 5)) (Const (IntVal 5)), 
                      Not (Var "hej")]), 
    testCase "Operands: can be expressions enclosed in parentheses"
     $ runIndentParser parseOperand () "" "(5+5)"
     @?= Right (Oper Plus (Const (IntVal 5)) (Const (IntVal 5))),
    testCase "Operands: can be slices of arrays"
     $ runIndentParser (do a <- parseOperand; eof; return a) () "" "nums[i+5]"
     @?= Right (Slice "nums" [Oper Plus (Var "i") (Const (IntVal 5))]),
-- Parse Process
---- Assignment
    testCase "Process: assignment of one variable"
     $ runIndentParser parseProcess () "" "a := 1"
     @?= Right (SDef [(Var "a")] [Const (IntVal 1)]),
    testCase "Process: assignment of array slice"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" "a[i] := 1"
     @?= Right (SDef [Slice "a" [Var "i"]] [Const (IntVal 1)]),
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
     @?= Right (SSeq  [SContinue]),
    testCase "Process: a SEQ, followed by an assignment"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" "SEQ \n  nums[i] := 5"
     @?= Right (SSeq  [SDef [Slice "nums" [Var "i"]] [Const (IntVal 5)]]),
    testCaseBad "Process: SEQ only parses correctly indented blocks"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" "SEQ \n  SKIP\nSTOP",
    testCase "Process: sequences can be nested"
     $ runIndentParser parseProcess () "" "SEQ \n  SEQ \n    SKIP\n  SEQ\n    STOP"
     @?= Right (SSeq  [SSeq  [SContinue], SSeq  [SExit]]),
    testCase "Process: sequences can be replicated"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "SEQ i = 0 FOR 4\n  SEQ \n    SKIP\n    STOP"
     @?= Right (SFor (Var "i") (Const (IntVal 0)) (Const (IntVal 4))
                (SSeq  [SSeq  [SContinue,SExit]])),
    testCase "Process: SEQ: replicated sequences can be followed by IF"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "SEQ i = 0 FOR 4\n  SEQ \n    IF\n      TRUE\n        SKIP\n    STOP"
     @?= Right (SFor (Var "i") (Const (IntVal 0)) (Const (IntVal 4))
                (SSeq  [SSeq  [SIf  [IfCase (Const TrueVal) SContinue],SExit]])),
---- Conditional
    testCase "Process: IF, followed by indented block of Cond"
     $ runIndentParser parseProcess () "" "IF \n  a = 5\n    SKIP"
     @?= Right (SIf  [IfCase (Oper Eq (Var "a") (Const (IntVal 5))) SContinue]),
    testCase "Process: IF, condition can consist of array slice"
     $ runIndentParser parseProcess () "" "IF \n  a[i] < 5\n    SKIP"
     @?= Right (SIf  [IfCase (Oper Less (Slice "a" [Var "i"]) (Const (IntVal 5))) SContinue]),
    testCase "Process: IF with multiple Conds"
     $ runIndentParser parseProcess () "" "IF \n  a >= 5\n    SKIP\n  a < 5\n    a := a + 1"
     @?= Right (SIf  [IfCase (Oper Geq (Var "a") (Const (IntVal 5))) SContinue,
                          IfCase (Oper Less (Var "a") (Const (IntVal 5))) 
                                (SDef [Var "a"] [Oper Plus (Var "a") (Const (IntVal 1))])]),
    testCase "Process: IF with TRUE as a condition"
     $ runIndentParser parseProcess () "" "IF \n  a >= 5\n    SKIP\n  TRUE\n    a := a + 1"
     @?= Right (SIf  [IfCase (Oper Geq (Var "a") (Const (IntVal 5))) SContinue,
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
                        [SwitchCase [Const (ByteVal (CharVal 'a')), 
                                     Const (ByteVal (CharVal 'b')),
                                     Const (ByteVal (CharVal 'c'))] 
                                    (SDef [Var "x"] [Const TrueVal]),
                         SwitchCase [Const TrueVal] (SDef [Var "x"] [Const FalseVal])]),
    testCaseBad "Process: CASE can have only one process per option"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "CASE dir\n  up\n    x := x + 1\n    x := x - 1",
---- Loop
    testCase "Process: WHILE, followed by boolean expression and indented process"
     $ runIndentParser parseProcess () "" "WHILE a <> 0\n  SEQ\n    a := a - 1"
     @?= Right (SWhile (Oper Neq (Var "a") (Const (IntVal 0))) 
                       (SSeq  [SDef [Var "a"] [Oper Minus (Var "a") (Const (IntVal 1))]])),
    testCaseBad "Process: WHILE can be followed by exactly one process"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "WHILE a <> 0\n  a := a - 1\n  b := a + 1",
---- Parallel
    testCase "Process: PAR, followed by indented block of processes"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "PAR\n  multiply(5)\n  SEQ\n    c1 ! 1\n    c2 ? a"
     @?= Right (SGo [SCall (Call "multiply" [Const (IntVal 5)]),
                     SSeq  [SSend (Chan "c1") (Const (IntVal 1)),
                                 SReceive (Var "a") (Chan "c2")]]),
---- Alternation
    testCase "Process: ALT where alternative guard is input"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT\n  c1 ? a\n    c2 ! a + 1"
     @?= Right (SSelect  [SCase (SelectCase (Const TrueVal, SReceive (Var "a") (Chan "c1")) 
                                          (SSend (Chan "c2") (Oper Plus (Var "a") (Const (IntVal 1)))))]),
    testCase "Process: ALT where alternative guard is 'boolean & input'"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT\n  true & c1 ? a\n    c2 ! a + 1"
     @?= Right (SSelect  [SCase (SelectCase (Var "true", SReceive (Var "a") (Chan "c1"))
                                          (SSend (Chan "c2") (Oper Plus (Var "a") (Const (IntVal 1)))))]),
    testCase "Process: ALT where alternative guard is 'boolean & SKIP'"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT\n  true & SKIP\n    c2 ! a + 1"
     @?= Right (SSelect  [SCase (SelectCase (Var "true", SContinue) 
                                          (SSend (Chan "c2") (Oper Plus (Var "a") (Const (IntVal 1)))))]),
    testCase "Process: ALT with multiple alternatives"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT\n  true & SKIP\n    c2 ! a + 1\n  c1 ? a\n    SKIP"
     @?= Right (SSelect  [SCase (SelectCase (Var "true", SContinue) 
                                          (SSend (Chan "c2") (Oper Plus (Var "a") (Const (IntVal 1))))),
                               SCase (SelectCase (Const TrueVal, SReceive (Var "a") (Chan "c1")) SContinue)]),
    testCase "Process: ALT with no alternatives behaves as STOP"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT"
     @?= Right (SExit),
    testCase "Process: alternations can be replicated"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT i = 0 FOR 4\n  c1 ? v\n    SKIP"
     @?= Right (SFor (Var "i") (Const (IntVal 0)) (Const (IntVal 4)) (SSelect  
               [SCase (SelectCase (Const TrueVal, SReceive (Var "v") (Chan "c1")) SContinue)])),
    testCaseBad "Process: ALT can only have one process per alternative"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" 
                        "ALT\n  true & c1 ? a\n    c2 ! a + 1\n    SKIP",
---- Specification with declaration
    testCase "Process: parse declaration of one variable"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" "INT a:\nSKIP"
     @?=  Right (SDecl [Var "a"] (SVar INT) (SContinue)),
    testCase "Process: parse declaration of multiple variables"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" "INT a, b, c:\nSKIP"
     @?=  Right (SDecl [Var "a", Var "b", Var "c"] (SVar INT) (SContinue)),
    testCase "Process: parse declaration of channels"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" "CHAN OF INT a, b, c:\nSKIP"
     @?=  Right (SDecl [Var "a", Var "b", Var "c"] (SChan INT) (SContinue)),
    testCase "Process: parse declaration of arrays"
     $ runIndentParser (do a <- parseProcess; eof; return a) () "" "[5]INT a:\nSKIP"
     @?=  Right (SDecl [Var "a"] (SVar (DArray [Const (IntVal 5)] INT)) (SContinue)),
-- Parse formals
    testCase "Formals: parse one formal"
     $ runIndentParser (do a <- parseFormals; eof; return a) () "" "CHAN OF INT in"
     @?=  Right [Arg [Var "in"] (SChan INT)],
    testCase "Formals: parse multiple formals of same type"
     $ runIndentParser (do a <- parseFormals; eof; return a) () "" "CHAN OF INT in, out, err"
     @?=  Right [Arg [Var "in",Var "out",Var "err"] (SChan INT)],
    testCase "Formals: parse multiple formals of different types"
     $ runIndentParser (do a <- parseFormals; eof; return a) () "" 
                        "CHAN OF INT in, out, BOOL check"
     @?=  Right [Arg [Var "in",Var "out"] (SChan INT), Arg [Var "check"] (SVar BOOL)],
    testCase "Formals: parse formals of the same type declared seperately"
     $ runIndentParser (do a <- parseFormals; eof; return a) () "" 
                        "CHAN OF INT in, out, CHAN OF INT err"
     @?=  Right [Arg [Var "in",Var "out"] (SChan INT), Arg [Var "err"] (SChan INT)],
-- Parse Procedure
    testCase "Procedures: parse procedure"
     $ runIndentParser (do a <- parseProc; eof; return a) () "" 
                        "PROC test(CHAN OF BYTE out)\n  BOOL swap:\n  swap := TRUE\n:"
     @?=  Right (FFun "test" [Arg [Var "out"] (SChan BYTE)] [] (SDecl [Var "swap"] (SVar BOOL) (SDef [Var "swap"] [Const TrueVal]))),
-- Parse Program
    testCase "Program: parse list of procedures"
     $ runIndentParser (do a <- parseProg; eof; return a) () "" 
                        (unlines ["PROC test(CHAN OF BOOL out)",
                                  "  BOOL swap:",
                                  "  SEQ",
                                  "    swap := TRUE",
                                  "    out ! swap",
                                  ":",
                                  "PROC check(CHAN OF BYTE out)",
                                  "  CHAN OF BOOL tmp:",
                                  "  BOOL check:",
                                  "  PAR",
                                  "    test(tmp)",
                                  "    tmp ? check",
                                  "    out ! (BYTE check) + 48",
                                  ":"])
     @?=  Right [FFun "test" [Arg [Var "out"] (SChan BOOL)] [] 
                   (SDecl [Var "swap"] (SVar BOOL) 
                      (SSeq  [SDef [Var "swap"] [Const TrueVal], 
                       SSend (Chan "out") (Var "swap")])), 
                 FFun "check" [Arg [Var "out"] (SChan BYTE)] [] 
                   (SDecl [Var "tmp"] (SChan BOOL) 
                      (SDecl [Var "check"] (SVar BOOL) (SGo 
                         [SCall (Call "test" [Var "tmp"]), 
                          SReceive (Var "check") (Chan "tmp"), 
                          SSend (Chan "out") 
                                (Oper Plus (Conv BYTE (Var "check")) (Const (IntVal 48)))])))
                ]
  ]
-- Comments
-- Whitespace
-- Indentation

-----------------------
-- Testing Generator --
-----------------------

generatorTests :: TestTree
generatorTests = testGroup "Generator Tests" [monadOperations, valueFun, typeFun, operatorFun, expFun, fargsFun, specFun, stmtTests, program]


monadOperations :: TestTree
monadOperations = testGroup "Monad operation tests"
  [ testCase "getType: get type of variable in env"
     $ runGen (getType "x") ([("x", BOOL), ("z", DChan BYTE)], 0)
     @?= (Right BOOL, mempty, mempty),
    testCase "getType: fails if variable is not in env"
     $ runGen (getType "x") ([("y", BOOL), ("z", DChan BYTE)], 0)
     @?= (Left (EVar "x"), mempty, mempty),
    testCase "getType: most resent binding is valid"
     $ runGen (getType "y") ([("y", BOOL), ("y", DChan BYTE)], 0)
     @?= (Right BOOL, mempty, mempty),
    testCase "bindVar: variable can be bound in empty env"
     $ runGen (bindVar "x" (INT) (getType "x")) ([], 0)
     @?= (Right INT, mempty, mempty),
    testCase "bindVar: variable can be bound in non-empty env"
     $ runGen (bindVar "x" (INT) (getType "x")) ([("y", BOOL), ("z", DChan BYTE)], 0)
     @?= (Right INT, mempty, mempty),
    testCase "bindVar: newest binding of variable is valid"
     $ runGen (bindVar "x" (INT) (getType "x")) ([("x", BOOL), ("z", DChan BYTE)], 0)
     @?= (Right INT, mempty, mempty),
    testCase "write: writing program data"
     $ runGen (write "test") ([], 0)
     @?= (Right (), ["test"], mempty),
    testCase "write: data is concatenated"
     $ runGen (do write "Hello"; write " World!"; return ()) ([], 0)
     @?= (Right (), ["Hello", " World!"], mempty),
    testCase "addImport: imports can be added"
     $ runGen (addImport "import \"fmt\"") ([], 0)
     @?= (Right (), [], ["import \"fmt\""])
  ]

valueFun :: TestTree
valueFun = testGroup "Names and values"
  [ testCase "name: generate name"
     $ runGen (genName "foo") ([], 0)
     @?= (Right "foo", [], []),
    testCase "name: dots are replaced with underscores"
     $ runGen (genName "foo.bar") ([], 0)
     @?= (Right "foo_bar", [], []),
    testCase "value: generating TrueVal"
     $ runGen (genVal TrueVal) ([], 0)
     @?= (Right "true", [], []),
    testCase "value: generating FalseVal"
     $ runGen (genVal FalseVal) ([], 0)
     @?= (Right "false", [], []),
    testCase "value: generating NoneVal"
     $ runGen (genVal NoneVal) ([], 0)
     @?= (Right "nil", [], []),
    testCase "value: generating IntVal"
     $ runGen (genVal (IntVal 5)) ([], 0)
     @?= (Right "5", [], []),
    testCase "value: generating HexVal"
     $ runGen (genVal (HexVal "1D")) ([], 0)
     @?= (Right "1D", [], []),
    testCase "value: generating ByteVal integer"
     $ runGen (genVal (ByteVal (IntVal 95))) ([], 0)
     @?= (Right "95", [], []),
    testCase "value: generating ByteVal character"
     $ runGen (genVal (ByteVal (CharVal 'a'))) ([], 0)
     @?= (Right "\'a\'", [], []),
    testCase "value: generating StringVal"
     $ runGen (genVal (StringVal "hello world!")) ([], 0)
     @?= (Right "\"hello world!\"", [], [])
  ]

typeFun :: TestTree
typeFun = testGroup "Data types"
  [ testCase "DTypes: generating INTs"
     $ runGen (genDType INT) ([], 0)
     @?= (Right "int", [], []),
    testCase "DTypes: generating BOOLs"
     $ runGen (genDType BOOL) ([], 0)
     @?= (Right "bool", [], []),
    testCase "DTypes: generating BYTEs"
     $ runGen (genDType BYTE) ([], 0)
     @?= (Right "byte", [], []),
    testCase "DTypes: generating one dimensional array"
     $ runGen (genDType (DArray [Const (IntVal 5)] INT)) ([], 0)
     @?= (Right "[5]int", [], []),
    testCase "DTypes: generating multi dimensional array"
     $ runGen (genDType (DArray [Const (IntVal 5),Const (IntVal 4),Const (IntVal 2)] BOOL)) ([], 0)
     @?= (Right "[5][4][2]bool", [], []),
    testCase "DTypes: generating channels"
     $ runGen (genDType (DChan BYTE)) ([], 0)
     @?= (Right "chan byte", [], [])
  ]

operatorFun :: TestTree
operatorFun = testGroup "Operators"
  [ testCase "Operators: generating '+'"
     $ runGen (genOper Plus) ([], 0)
     @?= (Right "+", [], []),
    testCase "Operators: generating '-'"
     $ runGen (genOper Minus) ([], 0)
     @?= (Right "-", [], []),
    testCase "Operators: generating '*'"
     $ runGen (genOper Times) ([], 0)
     @?= (Right "*", [], []),
    testCase "Operators: generating '/'"
     $ runGen (genOper Div) ([], 0)
     @?= (Right "/", [], []),
    testCase "Operators: generating '\\'"
     $ runGen (genOper Mod) ([], 0)
     @?= (Right "\\", [], []),
    testCase "Operators: generating 'Eq'"
     $ runGen (genOper Eq) ([], 0)
     @?= (Right "==", [], []),
    testCase "Operators: generating 'Neq'"
     $ runGen (genOper Neq) ([], 0)
     @?= (Right "!=", [], []),
    testCase "Operators: generating 'Less'"
     $ runGen (genOper Less) ([], 0)
     @?= (Right "<", [], []),
    testCase "Operators: generating 'Greater'"
     $ runGen (genOper Greater) ([], 0)
     @?= (Right ">", [], []),
    testCase "Operators: generating 'Geq'"
     $ runGen (genOper Geq) ([], 0)
     @?= (Right ">=", [], []),
    testCase "Operators: generating 'Leq'"
     $ runGen (genOper Leq) ([], 0)
     @?= (Right "<=", [], []),
    testCase "Operators: generating 'And'"
     $ runGen (genOper And) ([], 0)
     @?= (Right "&&", [], []),
    testCase "Operators: generating 'Or'"
     $ runGen (genOper Or) ([], 0)
     @?= (Right "||", [], [])
  ]

expFun :: TestTree
expFun = testGroup "Expressions"
  [ testCase "Expressions: generate constants"
     $ runGen (genExp (Const TrueVal)) ([], 0)
     @?= (Right "true", [], []),
    testCase "Expressions: generate variables"
     $ runGen (genExp (Var "a")) ([], 0)
     @?= (Right "a", [], []),
    testCase "Expressions: generate channels"
     $ runGen (genExp (Chan "a")) ([], 0)
     @?= (Right "a", [], []),
    testCase "Expressions: generate operations"
     $ runGen (genExp (Oper Plus (Var "a") (Const (IntVal 2)))) ([], 0)
     @?= (Right "a + 2", [], []),
    testCase "Expressions: generate function calls"
     $ runGen (genExp (Call "multiply" [Const (IntVal 4),Const (IntVal 3)])) ([], 0)
     @?= (Right "multiply(4, 3)", [], []),
    testCase "Expressions: generate negated expressions"
     $ runGen (genExp (Not (Var "a"))) ([], 0)
     @?= (Right "!a", [], []),
    testCase "Expressions: generate arrays"
     $ runGen (genExp (Array [Const (IntVal 1), Const (IntVal 2), Const (IntVal 3)])) ([], 0)
     @?= (Right "{1, 2, 3}", [], []),
    testCase "Expressions: generate arrays of arrays"
     $ runGen (genExp (Array [Array [Const (IntVal 1), Const (IntVal 2), Const (IntVal 3)],
                              Array [Const (IntVal 42), Const (IntVal 1337)]])) ([], 0)
     @?= (Right "{{1, 2, 3}, {42, 1337}}", [], []),
    testCase "Expressions: generate array slices"
     $ runGen (genExp (Slice "nums" [Const (IntVal 1)])) ([], 0)
     @?= (Right "nums[1]", [], []),
    testCase "Expressions: generate conversion"
     $ runGen (genExp (Conv INT (Var "a"))) ([], 0)
     @?= (Right "int(a)", [], [])
  ]

fargsFun :: TestTree
fargsFun = testGroup "FArgs"
  [ testCase "FArgs: generate zero arguments"
     $ runGen (genArgs []) ([], 0)
     @?= (Right "", [], []),
    testCase "FArgs: generate one argument"
     $ runGen (genArgs [Arg [Var "b"] (SVar BYTE)]) ([], 0)
     @?= (Right "b byte", [], []),
    testCase "FArgs: generate list of arguments"
     $ runGen (genArgs [Arg [Var "c1",Var "c2"] (SChan INT),Arg [Var "b"] (SVar BOOL)]) ([], 0)
     @?= (Right "c1, c2 chan int, b bool", [], [])
  ]

specFun :: TestTree
specFun = testGroup "Specifications"
  [ testCase "Specs: generate variable"
     $ runGen (genSpec (SVar INT)) ([], 0)
     @?= (Right "int", [], []),
    testCase "Specs: generate channel"
     $ runGen (genSpec (SChan BYTE)) ([], 0)
     @?= (Right "chan byte", [], []),
    testCase "Specs: generate list of one spec"
     $ runGen (genSpecs [SVar BOOL]) ([], 0)
     @?= (Right "bool", [], []),
    testCase "Specs: generate list of specs"
     $ runGen (genSpecs [SVar BOOL, SChan INT, SChan BYTE]) ([], 0)
     @?= (Right "bool, chan int, chan byte", [], []),
    testCase "Specs: generate empty list of specs"
     $ runGen (genSpecx []) ([], 0)
     @?= (Right " ", [], [])
  ]

stmtTests :: TestTree
stmtTests = testGroup "Statements" [assignments, declarations, sequences, conditional, switch, goroutines, select, loops, calls, ioFun, skipstop]

assignments :: TestTree
assignments = testGroup "Assignments"
  [ testCase "Assignment of one variable"
     $ runGen (genStmt (SDef [Var "a"] [Const (IntVal 1)]) "") ([("a", INT)], 0)
     @?= (Right "a = 1\n", [], []),
    testCase "Assignment of multiple variables"
     $ runGen (genStmt (SDef [Var "a", Var "b", Var "c"] 
                             [Const (IntVal 1),Const (IntVal 2),Var "v"]) "") 
                       ([("a", INT), ("b", INT), ("c", INT)], 0)
     @?= (Right "a, b, c = 1, 2, v\n", [], []),
    testCase "Assignment to array indices"
     $ runGen (genStmt (SDef [Slice "a" [Const (IntVal 0)]] [Const (IntVal 5)]) "") 
                       ([("a", DArray [Const (IntVal 5)] INT)], 0)
     @?= (Right "a[0] = 5\n", [], []),
    testCase "Assignment of entire array of ints"
     $ runGen (genStmt (SDef [Var "a"] 
                             [Array [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)]]) "") 
                       ([("a", DArray [Const (IntVal 3)] INT)], 0)
     @?= (Right "a = [3]int{1, 2, 3}\n", [], []),
    testCase "Assignment of entire array of bools"
     $ runGen (genStmt (SDef [Var "a"] [Array [Const TrueVal,Const TrueVal,Const FalseVal]]) "") 
                       ([("a", DArray [Const (IntVal 3)] BOOL)], 0)
     @?= (Right "a = [3]bool{true, true, false}\n", [], []),
    testCase "Assignment of entire array of bytes"
     $ runGen (genStmt (SDef [Var "a"] [Array [Const (ByteVal (CharVal 'a')),
                                               Const (ByteVal (CharVal 'b'))]]) "")
                       ([("a", DArray [Const (IntVal 2)] BYTE)], 0)
     @?= (Right "a = [2]byte{'a', 'b'}\n", [], []),
    testCase "Assignment of entire array of channels"
     $ runGen (genStmt (SDef [Var "a"] [Array [Chan "c1", Chan "c2", Chan "c3"]]) "") 
                       ([("a", DArray [Const (IntVal 3)] (DChan BYTE))], 0)
     @?= (Right "a = [3]chan byte{c1, c2, c3}\n", [], []),
    testCase "Assignment of entire array of arrays"
     $ runGen (genStmt (SDef [Var "a"] [Array [Array [Const (IntVal 1),Const (IntVal 2)],
                                               Array [Const (IntVal 3),Const (IntVal 4)]]]) "") 
                       ([("a", DArray [Const (IntVal 2)] (DArray [Const (IntVal 2)] INT))], 0)
     @?= (Right "a = [2][2]int{{1, 2}, {3, 4}}\n", [], [])
  ]

declarations :: TestTree
declarations = testGroup "Declarations"
  [ testCase "Statements: declaration of one variable"
     $ runGen (genStmt (SDecl [Var "a"] (SVar BOOL) SContinue) "") ([], 0)
     @?= (Right "var a bool\n", [], []),
    testCase "Statements: declaration of multiple variables"
     $ runGen (genStmt (SDecl [Var "a", Var "b", Var "c"] (SVar BOOL) SContinue) "") ([], 0)
     @?= (Right "var a bool\nvar b bool\nvar c bool\n", [], []),
    testCase "Statements: declaration of array"
     $ runGen (genStmt (SDecl [Var "a"] (SVar (DArray [Const (IntVal 5)] INT)) SContinue) "") ([], 0)
     @?= (Right "var a [5]int\n", [], []),
    testCase "Statements: declaration of channel"
     $ runGen (genStmt (SDecl [Var "c"] (SChan INT) SContinue) "") ([], 0)
     @?= (Right "var c = make(chan int)\n", [], [])
  ]

sequences :: TestTree
sequences = testGroup "Sequences"
  [ testCase "Sequence containing one statement"
     $ runGen (genStmt (SSeq  [SDef [Var "a"] [Var "b"]]) "") ([("a", INT)], 0)
     @?= (Right "a = b\n", [], []),
    testCase "Sequence containing multiple statements"
     $ runGen (genStmt (SSeq  [SDef [Var "a"] [Var "b"], SExit]) "") ([("a", INT)], 0)
     @?= (Right "a = b\n\nos.Exit(1)", [], ["import \"os\"\n"]),
    testCase "Sequence containing no statements"
     $ runGen (genStmt (SSeq  []) "") ([], 0)
     @?= (Right "", [], [])
  ]

conditional :: TestTree
conditional = testGroup "Conditionals"
  [ testCase "If-statements containing one case"
     $ runGen (genStmt (SIf [IfCase (Oper Less (Var "a") (Const (IntVal 5))) 
                                    (SSend (Chan "c") (Var "a"))]) "") ([], 0)
     @?= (Right "if a < 5 {\n  c <- a\n} else { os.Exit(1) }", [], ["import \"os\"\n"]),
    testCase "If-statements containing multiple cases"
     $ runGen (genStmt (SIf 
         [IfCase (Oper Less (Var "a") (Const (IntVal 5))) (SSend (Chan "c") (Var "a")),
          IfCase (Oper Geq (Var "a") (Const (IntVal 5))) (SSend (Chan "c") (Const (IntVal 0)))]) "")
         ([], 0)
     @?= (Right "if a < 5 {\n  c <- a\n} else if a >= 5 {\n  c <- 0\n} else { os.Exit(1) }", [], ["import \"os\"\n"])
  ]

switch :: TestTree
switch = testGroup "Switch-statements"
  [ testCase "Switch-statements containing one case"
     $ runGen (genStmt (SSwitch (Var "a") 
                        [SwitchCase [Const (IntVal 5)] (SSend (Chan "c") (Var "a"))]) "") ([], 0)
     @?= (Right "switch a {\ncase 5 :\n  c <- a\n}", [], []),
    testCase "Switch-statements containing multiple cases"
     $ runGen (genStmt (SSwitch (Var "a") 
                        [SwitchCase [Const (IntVal 5)] (SSend (Chan "c") (Var "a")),
                         SwitchCase [Var "b"] (SDef [Var "a"] [Const (IntVal 0)])]) "") ([("a", INT)], 0)
     @?= (Right "switch a {\ncase 5 :\n  c <- a\ncase b :\n  a = 0\n\n}", [], []),
    testCase "Switch-statements with case containing multiple expressions"
     $ runGen (genStmt (SSwitch (Var "a") [SwitchCase [Var "b",Var "c",Const (IntVal 10)] 
                                                       (SSend (Chan "c") (Var "a"))]) "") ([], 0)
     @?= (Right "switch a {\ncase b, c, 10 :\n  c <- a\n}", [], [])
  ]

goroutines :: TestTree
goroutines = testGroup "Goroutines"
  [ testCase "Goroutines containing only one process"
     $ runGen (genStmt (SGo [SSend (Chan "c") (Var "a")]) "") ([], 0)
     @?= (Right (unlines ["var wg_0 sync.WaitGroup",
                          "",
                          "wg_0.Add(1)",
                          "go func() {",
                          "  defer wg_0.Done()",
                          "  c <- a",
                          "}()",
                          "",
                          "wg_0.Wait()"]), [], ["import \"sync\"\n"]),
    testCase "Goroutines containing multiple processes"
     $ runGen (genStmt (SGo [SSend (Chan "c1") (Var "a"), SReceive (Var "v") (Chan "c2")]) "") ([],0)
     @?= (Right (unlines ["var wg_0 sync.WaitGroup",
                          "",
                          "wg_0.Add(1)",
                          "go func() {",
                          "  defer wg_0.Done()",
                          "  c1 <- a",
                          "}()", 
                          "",
                          "wg_0.Add(1)", 
                          "go func() {",
                          "  defer wg_0.Done()",
                          "  v = <-c2",
                          "}()",
                          "",
                          "wg_0.Wait()"]), [], ["import \"sync\"\n"]),
    testCase "Goroutines, mutiples parallels in same sequence"
     $ runGen (genStmt (SSeq [SGo [SSend (Chan "c") (Var "a")],
                              SGo [SSend (Chan "c") (Var "b")]]) "") ([], 0)
     @?= (Right (unlines ["var wg_0 sync.WaitGroup",
                          "",
                          "wg_0.Add(1)",
                          "go func() {",
                          "  defer wg_0.Done()",
                          "  c <- a",
                          "}()",
                          "",
                          "wg_0.Wait()",
                          "",
                          "var wg_1 sync.WaitGroup",
                          "",
                          "wg_1.Add(1)",
                          "go func() {",
                          "  defer wg_1.Done()",
                          "  c <- b",
                          "}()",
                          "",
                          "wg_1.Wait()"]), [], ["import \"sync\"\n", "import \"sync\"\n"])
  ]

select :: TestTree
select = testGroup "Select-statements"
  [ testCase "Select-statements containing one case"
     $ runGen (genStmt (SSelect [SCase (SelectCase (Const TrueVal, SReceive (Var "a") (Chan "c")) 
                                                   (SSend (Chan "c") (Const (IntVal 42))))]) "") 
              ([("c", DChan INT)], 0)
     @?= (Right "select {\ncase a = <-func() chan int {if true {return c} else {return nil}}() :\n  c <- 42\n}", [], []),
    testCase "Select-statements containing multiple cases"
     $ runGen (genStmt (SSelect [SCase (SelectCase (Const TrueVal, SReceive (Var "a") (Chan "c1")) 
                                                   (SSend (Chan "c2") (Const (IntVal 42)))),
                                 SCase (SelectCase (Const TrueVal, SReceive (Var "b") (Chan "c2")) 
                                                   (SSend (Chan "c1") (Var "b")))]) "")
              ([("c1", DChan INT), ("c2", DChan INT)], 0)
     @?= (Right "select {\ncase a = <-func() chan int {if true {return c1} else {return nil}}() :\n  c2 <- 42\ncase b = <-func() chan int {if true {return c2} else {return nil}}() :\n  c1 <- b\n}", [], []),
    testCase "Select-statements where guard contains both boolean and input"
     $ runGen (genStmt (SSelect [SCase (SelectCase (Oper Geq (Var "a") (Const (IntVal 13)), 
                                                    SReceive (Var "a") (Chan "c")) 
                                                   (SSend (Chan "c") (Const (IntVal 42))))]) "") 
              ([("c", DChan INT)], 0)
     @?= (Right "select {\ncase a = <-func() chan int {if a >= 13 {return c} else {return nil}}() :\n  c <- 42\n}", [], [])
  ]

loops :: TestTree
loops = testGroup "Loops"
  [ testCase "While-loop"
     $ runGen (genStmt (SWhile (Oper Neq (Var "a") (Const (IntVal 0))) (SSend (Chan "c") (Var "a"))) "") ([], 0)
     @?= (Right "for a != 0 {\n  c <- a\n}", [], []),
    testCase "For-loops where count > 0"
     $ runGen (genStmt (SFor (Var "a") (Const (IntVal 0)) (Const (IntVal 4)) (SSend (Chan "c") (Var "a"))) "") ([], 0)
     @?= (Right "for a := 0; a < 4; a++ {\n  c <- a\n}", [], [])
  ]

calls :: TestTree
calls = testGroup "Function calls"
  [ testCase "Calls with zero arguments"
     $ runGen (genStmt (SCall (Call "dummy" [])) "") ([], 0)
     @?= (Right "dummy()", [], []),
    testCase "Calls with one argument"
     $ runGen (genStmt (SCall (Call "fib" [Const (IntVal 5)])) "") ([], 0)
     @?= (Right "fib(5)", [], []),
    testCase "Calls with multiple arguments"
     $ runGen (genStmt (SCall (Call "multiply" [Const (IntVal 5),Const (IntVal 10)])) "") ([], 0)
     @?= (Right "multiply(5, 10)", [], [])
  ]

ioFun :: TestTree
ioFun = testGroup "IO functions"
  [ testCase "Output statements"
     $ runGen (genStmt (SSend (Chan "c") (Const (StringVal "Hello World!"))) "") ([], 0)
     @?= (Right "c <- \"Hello World!\"", [], []),
    testCase "Input statements"
     $ runGen (genStmt (SReceive (Var "v") (Chan "c")) "") ([], 0)
     @?= (Right "v = <-c", [], [])
  ]

skipstop :: TestTree
skipstop = testGroup "SKIP and STOP"
  [ testCase "Empty statements"
     $ runGen (genStmt (SContinue) "") ([], 0)
     @?= (Right "", [], []),
    testCase "Exit statements"
     $ runGen (genStmt (SExit) "") ([], 0)
     @?= (Right "os.Exit(1)", [], ["import \"os\"\n"])
  ]

program :: TestTree
program = testGroup "Programs"
  [ testCase "Functions: generating generic function"
     $ runGen (genFun (FFun "wham" [Arg [Var "bam", Var "pow"] (SVar INT)] [] (SDecl [Var "c"] (SChan INT) (SCall (Call "puff" [Var "bam", Var "pow", Var "c"]))))) ([], 0)
     @?= (Right "func wham(bam, pow int) {\n  var c = make(chan int)\n  puff(bam, pow, c)\n}", [], []),
    testCase "Program: generating generic function"
     $ runGen (genProg [FFun "test" [Arg [Var "in", Var "out", Var "err"] (SChan BYTE)] [] (SDecl [Var "c"] (SVar INT) (SDef [Var "c"] [Const (IntVal 5)]))]) ([], 0)
     @?= (Right (), ["func test(in, out, err chan byte) {\n  defer close(out)\n  defer close(err)\n\n  var c int\n  c = 5\n\n}" ++ "\n\n" ++
                     unlines ["func main() {",
                              "  in, out, err := make(chan byte, 10), make(chan byte, 10), make(chan byte, 10)\n",
                              "  var wg_main sync.WaitGroup\n",
                              "  wg_main.Add(1)",
                              "  go func() {",
                              "    test(in, out, err)\n",
                              "    wg_main.Done()",
                              "  }()\n",
                              "  go func() {",
                              "    input := bufio.NewReader(os.Stdin)",
                              "    for {",
                              "      char, _, err := input.ReadRune()",
                              "      if err == nil {in <- byte(char)}",
                              "    }",
                              "  }()\n",
                              "  wg_main.Add(1)",
                              "  go func() {",
                              "    for i := range out {",
                              "      fmt.Print(string(i))",
                              "    }",
                              "    wg_main.Done()",
                              "  }()\n",
                              "  wg_main.Add(1)",
                              "  go func() {",
                              "    for i := range err {",
                              "      fmt.Print(string(i))",
                              "    }",
                              "    wg_main.Done()",
                              "  }()\n",
                              "  wg_main.Wait()",
                              "}"]
                    ], ["import \"fmt\"\n", 
                        "import \"sync\"\n", 
                        "import \"os\"\n", 
                        "import \"bufio\"\n"]),
    testCase "Program: generating empty program"
     $ runGen (genProg []) ([], 0)
     @?= (Right (), [], []),
    testCase "Program: generating program importing multiple libraries"
     $ runGen (genProg [FFun "help" [Arg [Var "in", Var "out", Var "err"] (SChan BYTE)] [] (SSeq [SSend (Chan "out") (Const (ByteVal (CharVal 'a'))),
                                                                             SExit])]) ([], 0)
     @?= (Right (), ["func help(in, out, err chan byte) {\n  defer close(out)\n  defer close(err)\n\n  out <- 'a'\n  os.Exit(1)\n}" ++ "\n\n" ++
                     unlines ["func main() {",
                              "  in, out, err := make(chan byte, 10), make(chan byte, 10), make(chan byte, 10)\n",
                              "  var wg_main sync.WaitGroup\n",
                              "  wg_main.Add(1)",
                              "  go func() {",
                              "    help(in, out, err)\n",
                              "    wg_main.Done()",
                              "  }()\n",
                              "  go func() {",
                              "    input := bufio.NewReader(os.Stdin)",
                              "    for {",
                              "      char, _, err := input.ReadRune()",
                              "      if err == nil {in <- byte(char)}",
                              "    }",
                              "  }()\n",
                              "  wg_main.Add(1)",
                              "  go func() {",
                              "    for i := range out {",
                              "      fmt.Print(string(i))",
                              "    }",
                              "    wg_main.Done()",
                              "  }()\n",
                              "  wg_main.Add(1)",
                              "  go func() {",
                              "    for i := range err {",
                              "      fmt.Print(string(i))",
                              "    }",
                              "    wg_main.Done()",
                              "  }()\n",
                              "  wg_main.Wait()",
                              "}"]
                    ], ["import \"os\"\n",
                        "import \"fmt\"\n", 
                        "import \"sync\"\n", 
                        "import \"os\"\n", 
                        "import \"bufio\"\n"]),
    testCase "Program: generating program importing libraries multipe times"
     $ generate [FFun "help" [Arg [Var "in", Var "out", Var "err"] (SChan BYTE)] [] 
                 (SSeq [SSend (Chan "out") (Const (ByteVal (CharVal 'a'))),
                        SExit, SExit])]
     @?= (["import \"os\"\n", "import \"fmt\"\n", "import \"sync\"\n", "import \"bufio\"\n", "\n",
           "func help(in, out, err chan byte) {\n  defer close(out)\n  defer close(err)\n\n  out <- 'a'\n  os.Exit(1)\n  os.Exit(1)\n}" ++ "\n\n" ++
                     unlines ["func main() {",
                              "  in, out, err := make(chan byte, 10), make(chan byte, 10), make(chan byte, 10)\n",
                              "  var wg_main sync.WaitGroup\n",
                              "  wg_main.Add(1)",
                              "  go func() {",
                              "    help(in, out, err)\n",
                              "    wg_main.Done()",
                              "  }()\n",
                              "  go func() {",
                              "    input := bufio.NewReader(os.Stdin)",
                              "    for {",
                              "      char, _, err := input.ReadRune()",
                              "      if err == nil {in <- byte(char)}",
                              "    }",
                              "  }()\n",
                              "  wg_main.Add(1)",
                              "  go func() {",
                              "    for i := range out {",
                              "      fmt.Print(string(i))",
                              "    }",
                              "    wg_main.Done()",
                              "  }()\n",
                              "  wg_main.Add(1)",
                              "  go func() {",
                              "    for i := range err {",
                              "      fmt.Print(string(i))",
                              "    }",
                              "    wg_main.Done()",
                              "  }()\n",
                              "  wg_main.Wait()",
                              "}"]], Nothing)
  ]

