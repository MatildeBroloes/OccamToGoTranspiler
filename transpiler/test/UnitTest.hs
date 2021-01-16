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
tests = testGroup "Unit Tests" [parserTests, generatorTests] -- put test trees here

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
generatorTests =
 testGroup "Generator Tests"
  [
-- Generate Names
    testCase "Names: generating name from empty string"
     $ generateName ""
     @?= "",
    testCase "Names: generate name consisting of alpha numerics"
     $ generateName "h3ll0Wo4lD"
     @?= "h3ll0Wo4lD",
    testCase "Names: dots are changed to underscores"
     $ generateName "h3.ll0"
     @?= "h3_ll0",
-- Generate Values
    testCase "Values: generating TrueVals"
     $ generateVal TrueVal
     @?= "true",
    testCase "Values: generating FalseVals"
     $ generateVal FalseVal
     @?= "false",
    testCase "Values: generating NoneVals"
     $ generateVal NoneVal
     @?= "nil",
    testCase "Values: generating IntVals"
     $ generateVal (IntVal 42)
     @?= "42",
    testCase "Values: generating HexVals"
     $ generateVal (HexVal "1D")
     @?= "1D",
    testCase "Values: generating ByteVals"
     $ generateVal (ByteVal (IntVal 95))
     @?= "95",
    testCase "Values: generating ByteVals"
     $ generateVal (ByteVal (CharVal 'a'))
     @?= "\'a\'",
    testCase "Values: generating StringVals"
     $ generateVal (StringVal "hello world!")
     @?= "\"hello world!\"",
-- Generate Data Types
    testCase "DTypes: generating INTs"
     $ generateDType INT
     @?= "int",
    testCase "DTypes: generating BOOLs"
     $ generateDType BOOL
     @?= "bool",
    testCase "DTypes: generating BYTEs"
     $ generateDType BYTE
     @?= "byte",
    testCase "DTypes: generating one dimensional array"
     $ generateDType (DArray [Const (IntVal 5)] INT)
     @?= "[5]int",
    testCase "DTypes: generating multi dimensional array"
     $ generateDType (DArray [Const (IntVal 5),Const (IntVal 4),Const (IntVal 2)] BOOL)
     @?= "[5][4][2]bool",
-- Generate Operator
    testCase "Operators: generating '+'"
     $ generateOper Plus
     @?= "+",
    testCase "Operators: generating '-'"
     $ generateOper Minus
     @?= "-",
    testCase "Operators: generating '*'"
     $ generateOper Times
     @?= "*",
    testCase "Operators: generating '/'"
     $ generateOper Div
     @?= "/",
    testCase "Operators: generating '\\'"
     $ generateOper Mod
     @?= "\\",
    testCase "Operators: generating 'Eq'"
     $ generateOper Eq
     @?= "==",
    testCase "Operators: generating 'Neq'"
     $ generateOper Neq
     @?= "!=",
    testCase "Operators: generating 'Less'"
     $ generateOper Less
     @?= "<",
    testCase "Operators: generating 'Greater'"
     $ generateOper Greater
     @?= ">",
    testCase "Operators: generating 'Geq'"
     $ generateOper Geq
     @?= ">=",
    testCase "Operators: generating 'Leq'"
     $ generateOper Leq
     @?= "<=",
    testCase "Operators: generating 'And'"
     $ generateOper And
     @?= "&&",
    testCase "Operators: generating 'Or'"
     $ generateOper Or
     @?= "||",
-- Generate Expression
    testCase "Expressions: generate constants"
     $ generateExp (Const TrueVal)
     @?= "true",
    testCase "Expressions: generate variables"
     $ generateExp (Var "a")
     @?= "a",
    testCase "Expressions: generate channels"
     $ generateExp (Chan "a")
     @?= "a",
    testCase "Expressions: generate operations"
     $ generateExp (Oper Plus (Var "a") (Const (IntVal 2)))
     @?= "a + 2",
    testCase "Expressions: generate function calls"
     $ generateExp (Call "multiply" [Const (IntVal 4),Const (IntVal 3)])
     @?= "multiply(4, 3)",
    testCase "Expressions: generate negated expressions"
     $ generateExp (Not (Var "a"))
     @?= "!a",
    testCase "Expressions: generate arrays"
     $ generateExp (Array [Const (IntVal 1), Const (IntVal 2), Const (IntVal 3)])
     @?= "[...]int{1, 2, 3}",
    testCase "Expressions: generate array slices"
     $ generateExp (Slice "nums" [Const (IntVal 1)])
     @?= "nums[1]",
    testCase "Expressions: generate conversion"
     $ generateExp (Conv INT (Var "a"))
     @?= "int(a)",
-- Generate FArgs
    testCase "FArgs: generate zero arguments"
     $ generateArgs []
     @?= "",
    testCase "FArgs: generate one argument"
     $ generateArgs [Arg [Var "b"] (SVar BYTE)]
     @?= "b byte",
    testCase "FArgs: generate list of arguments"
     $ generateArgs [Arg [Var "c1",Var "c2"] (SChan INT),Arg [Var "b"] (SVar BOOL)]
     @?= "c1, c2 chan int, b bool",
-- Generate Specs
    testCase "Specs: generate variable"
     $ generateSpec (SVar INT)
     @?= "int",
    testCase "Specs: generate channel"
     $ generateSpec (SChan BYTE)
     @?= "chan byte",
    testCase "Specs: generate list of one spec"
     $ generateSpecs [SVar BOOL]
     @?= "bool",
    testCase "Specs: generate list of specs"
     $ generateSpecs [SVar BOOL, SChan INT, SChan BYTE]
     @?= "bool, chan int, chan byte",
    testCase "Specs: generate empty list of specs"
     $ generateSpecx []
     @?= " ",
-- Generate Statements
---- Assignments 
    testCase "Statements: assignment of one variable"
     $ generateStmt (SDef [Var "a"] [Const (IntVal 1)]) ""
     @?= "a = 1",
    testCase "Statements: assignment of multiple variables"
     $ generateStmt (SDef [Var "a", Var "b", Var "c"] [Const (IntVal 1),Const (IntVal 2),Var "v"]) ""
     @?= "a, b, c = 1, 2, v",
    testCase "Statements: assignment to array indices"
     $ generateStmt (SDef [Slice "a" [Const (IntVal 0)]] [Const (IntVal 5)]) ""
     @?= "a[0] = 5",
    testCase "Statements: assignment of entire array"
     $ generateStmt (SDef [Var "a"] [Array [Const (IntVal 1),Const (IntVal 2),Const (IntVal 3)]]) ""
     @?= "a = [...]int{1, 2, 3}",
---- Declarations
    testCase "Statements: declaration of one variable"
     $ generateStmt (SDecl [Var "a"] (SVar BOOL) SContinue) ""
     @?= "var a bool\n",
    testCase "Statements: declaration of multiple variables"
     $ generateStmt (SDecl [Var "a", Var "b", Var "c"] (SVar BOOL) SContinue) ""
     @?= "var a, b, c bool\n",
    testCase "Statements: declaration of array"
     $ generateStmt (SDecl [Var "a", Var "b", Var "c"] (SVar BOOL) SContinue) ""
     @?= "var a, b, c bool\n",
    testCase "Statements: declaration of channel"
     $ generateStmt (SDecl [Var "c"] (SChan INT) SContinue) ""
     @?= "var c = make(chan int)\n",
---- Sequences
    testCase "Statements: sequence containing one statement"
     $ generateStmt (SSeq  [SDef [Var "a"] [Var "b"]]) ""
     @?= "a = b",
    testCase "Statements: sequence containing multiple statements"
     $ generateStmt (SSeq  [SDef [Var "a"] [Var "b"], SExit]) ""
     @?= "a = b\nos.Exit(1)",
    testCase "Statements: sequence containing no statements"
     $ generateStmt (SSeq  []) ""
     @?= "",
---- If-statements
    testCase "Statements: if-statements containing one case"
     $ generateStmt (SIf [IfCase (Oper Less (Var "a") (Const (IntVal 5))) (SSend (Chan "c") (Var "a"))]) ""
     @?= "if a < 5 {\n  c <- a\n} else { os.Exit(1) }",
    testCase "Statements: if-statements containing multiple cases"
     $ generateStmt (SIf 
         [IfCase (Oper Less (Var "a") (Const (IntVal 5))) (SSend (Chan "c") (Var "a")),
          IfCase (Oper Geq (Var "a") (Const (IntVal 5))) (SSend (Chan "c") (Const (IntVal 0)))]) ""
     @?= "if a < 5 {\n  c <- a\n} else if a >= 5 {\n  c <- 0\n} else { os.Exit(1) }",
---- Switch
    testCase "Statements: switch-statements containing one case"
     $ generateStmt (SSwitch (Var "a") [SwitchCase [Const (IntVal 5)] (SSend (Chan "c") (Var "a"))]) ""
     @?= "switch a {\ncase 5 :\n  c <- a\n}",
    testCase "Statements: switch-statements containing multiple cases"
     $ generateStmt (SSwitch (Var "a") [SwitchCase [Const (IntVal 5)] (SSend (Chan "c") (Var "a")),
                                        SwitchCase [Var "b"] (SDef [Var "a"] [Const (IntVal 0)])]) ""
     @?= "switch a {\ncase 5 :\n  c <- a\ncase b :\n  a = 0\n}",
    testCase "Statements: switch-statements with case containing multiple expressions"
     $ generateStmt (SSwitch (Var "a") [SwitchCase [Var "b",Var "c",Const (IntVal 10)] (SSend (Chan "c") (Var "a"))]) ""
     @?= "switch a {\ncase b, c, 10 :\n  c <- a\n}",
---- Goroutines
    testCase "Statements: goroutines containing only one process"
     $ generateStmt (SGo [SSend (Chan "c") (Var "a")]) ""
     @?= "var waitGroup sync.WaitGroup\nwaitGroup.Add(1)\ngo func() {\n  defer waitGroup.Done()\n  c <- a\n}()\n\nwaitGroup.Wait()",
    testCase "Statements: goroutines containing multiple processes"
     $ generateStmt (SGo [SSend (Chan "c1") (Var "a"), SReceive (Var "v") (Chan "c2")]) ""
     @?= "var waitGroup sync.WaitGroup\nwaitGroup.Add(1)\ngo func() {\n  defer waitGroup.Done()\n  c1 <- a\n}()\n\nwaitGroup.Add(1)\ngo func() {\n  defer waitGroup.Done()\n  v = <-c2\n}()\n\nwaitGroup.Wait()",
---- Select
    testCase "Statements: select-statements containing one case"
     $ generateStmt (SSelect [SCase (SelectCase (Const TrueVal, SReceive (Var "a") (Chan "c")) (SSend (Chan "c") (Const (IntVal 42))))]) ""
     @?= "select {\ncase a = <-func() chan byte {if true {return c} else {return nil}}() :\n  c <- 42\n}",
    testCase "Statements: select-statements containing multiple cases"
     $ generateStmt (SSelect [SCase (SelectCase (Const TrueVal, SReceive (Var "a") (Chan "c1")) (SSend (Chan "c2") (Const (IntVal 42)))),
                              SCase (SelectCase (Const TrueVal, SReceive (Var "b") (Chan "c2")) (SSend (Chan "c1") (Var "b")))]) ""
     @?= "select {\ncase a = <-func() chan byte {if true {return c1} else {return nil}}() :\n  c2 <- 42\ncase b = <-func() chan byte {if true {return c2} else {return nil}}() :\n  c1 <- b\n}",
    testCase "Statements: select-statements where guard contains both boolean and input"
     $ generateStmt (SSelect [SCase (SelectCase (Oper Geq (Var "a") (Const (IntVal 13)), 
                                                 SReceive (Var "a") (Chan "c")) 
                                                (SSend (Chan "c") (Const (IntVal 42))))]) ""
     @?= "select {\ncase a = <-func() chan byte {if a >= 13 {return c} else {return nil}}() :\n  c <- 42\n}",
---- While-loops
    testCase "Statements: while-loop"
     $ generateStmt (SWhile (Oper Neq (Var "a") (Const (IntVal 0))) (SSend (Chan "c") (Var "a"))) ""
     @?= "for a != 0 {\n  c <- a\n}",
---- For-loops
    testCase "Statements: for-loops where count = 0"
     $ generateStmt (SFor (Var "a") (Const (IntVal 0)) (Const (IntVal 0)) (SSend (Chan "c") (Var "a"))) ""
     @?= "",
    testCase "Statements: for-loops where count > 0"
     $ generateStmt (SFor (Var "a") (Const (IntVal 0)) (Const (IntVal 4)) (SSend (Chan "c") (Var "a"))) ""
     @?= "for a := 0; a < 4; a++ {\n  c <- a\n}",
---- Calls
    testCase "Statements: calls with zero arguments"
     $ generateStmt (SCall (Call "dummy" [])) ""
     @?= "dummy()",
    testCase "Statements: calls with one argument"
     $ generateStmt (SCall (Call "fib" [Const (IntVal 5)])) ""
     @?= "fib(5)",
    testCase "Statements: calls with multiple arguments"
     $ generateStmt (SCall (Call "multiply" [Const (IntVal 5),Const (IntVal 10)])) ""
     @?= "multiply(5, 10)",
---- Send
    testCase "Statements: output statements"
     $ generateStmt (SSend (Chan "c") (Const (StringVal "Hello World!"))) ""
     @?= "c <- \"Hello World!\"",
---- Receive
    testCase "Statements: input statements"
     $ generateStmt (SReceive (Var "v") (Chan "c")) ""
     @?= "v = <-c",
---- Continue
    testCase "Statements: empty statements"
     $ generateStmt (SContinue) ""
     @?= "",
---- Exit
    testCase "Statements: exit statements"
     $ generateStmt (SExit) ""
     @?= "os.Exit(1)",
-- Generate Functions
    testCase "Functions: generating generic function"
     $ generateFun (FFun "wham" [Arg [Var "bam", Var "pow"] (SVar INT)] [] (SDecl [Var "c"] (SChan INT) (SCall (Call "puff" [Var "bam", Var "pow", Var "c"]))))
     @?= "func wham(bam, pow int) {\n  var c = make(chan int)\n  puff(bam, pow, c)\n}",
-- Generate Program -- tested by running example programs
    testCase "Program: generating empty program"
     $ generateProg []
     @?= ""
  ]

















