-- Generator for converting ASTs into executable Go code
module Generator where

import AST
--import Control.Monad.Writer

--type Generator a = Writer [String] (Either String a)



-- Names
-- Types
-- Specifications
-- Operators
-- Values

-- Expresions
-- Conditions

-- Generator for Statements
-- Generator for Function
-- Generator for Program
generateProg :: Program -> [String]
generateProg [FFun name args [] stmt] = let main = unlines ["func main() {",
                                                            "  in := make(chan byte, 10)",
                                                            "  out := make(chan byte, 10)",
                                                            "  err := make(chan byte, 10)",
                                                            "  go " ++ name ++"(in, out,err)",
                                                            "  for i := range out {",
                                                            "    fmt.Print(String(i))",
                                                            "  }",
                                                            "}"]
                                            fun = generateFun (FFun name args [] stmt)
                                         in fun ++ "\n" ++ main -- creating main fun
generateProg ((FFun name args specs stmt):fs) = let fun = generateFun (FFun name args [] stmt)
                                                    funs = generateProg fs
                                                 in fun ++ "\n" ++ funs
