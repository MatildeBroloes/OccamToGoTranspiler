-- This is a black boc test suite for testing the functionality of the project code

import GoAST
import Parser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "Unit Tests" [] -- put test trees here

-- For doing negative testing (taken from AP exam 2020)
testCaseBad s t =
  testCase ("*" ++ s) $
    case t of
      Right a -> assertFailure $ "Unexpected success:" ++ show a
      Left e -> assertFailure $ "Error:" show e

--------------------
-- Testing Parser --
--------------------

parserTests :: TestTree
parserTests =
  testGroup "Testing Parser"
   [
-- Parse Program
--    Programs consist of definitions
-- Parse Definition
--    Definitions can be a lot of thing, but in this subset they are Procedures
-- Parse Procedure
--    Procedures have a Name, a list of arguments, and a body og processes
-- Parse Process
--    Processes can be assignment, input/output, skip, stop, sequence, conditional (if),
--    selection (case), loop (while), parallel, alternation, declaration (and definition??)
-- Parse Expressions
--    Expression are either type conversions, negations, binary operations, or boolean exps
-- Parse Operands
--    Operands are variables, function calls, lists of exps, digits, hex.digits, byteval, and bools
--    Parentheses are correctly parsed (maintaining precedence)
-- Parse Specifiers
--    Specifier can be either data type or channel
-- Parse Data Types
--    Bool, byte, various int and real types, and arrays of specific type (fx [3] INT)
-- Parse Strings
--    Strings are enclosed in double quotes
-- Parse Names
--    Names start with letter, and consist of letters, digits, and dots
-- Parse Digits and Hex.Digits
-- Parse Characters
--    Special characters fx *" is parsed specially
-- Comments
-- Whitespace
-- Indentation
   ]
