-- Generator for converting ASTs into executable Go code
module Generator where

import GoAST
--import Control.Monad.Writer

--type Generator a = Writer [String] (Either String a)

-- Generator for Values
generateVal :: Val -> String
generateVal TrueVal = "true"
generateVal FalseVal = "false"
generateVal NoneVal = "nil"
generateVal (IntVal i) = show i
generateVal (HexVal h) = h
generateVal (ByteVal b) = b
generateVal (StringVal s) = s

-- Generator for Types
generateDType :: DType -> String
generateDType INT = "int"
generateDType BOOL = "bool"
generateDType BYTE = "byte"
generateDType (DArray exps d) =
  let t = generateDType d
   in (generateArrayType exps) ++ t

generateArrayType :: [Exp] -> String
generateArrayType [e] = let v = generateExp e
                         in "[" ++ v ++ "]"
generateArrayType (e:es) = let v = generateExp e
                               vs = generateArrayType es
                            in "[" ++ v ++ "]" ++ vs
generateArrayType _ = "error"


-- Generator for Operators
generateOper :: Op -> String
generateOper _ = "+"

-- Generator for Expresions
generateExp :: Exp -> String
generateExp (Const v) = generateVal v
generateExp (Var v) = v
generateExp (Chan c) = c
generateExp (Oper o e1 e2) = let op = generateOper o
                                 ex1 = generateExp e1
                                 ex2 = generateExp e2
                              in ex1 ++ " " ++ op ++ " " ++ ex2
generateExp (Not e) = "!" ++ (generateExp e)
generateExp (Call name args) = name ++ "(" ++ (generateExps args) ++ ")"
generateExp (Array es) = "{" ++ (generateExps es) ++ "}" -- this is probably not correct, todo later
generateExp (Conv d e) = let t = generateDType d
                             ex = generateExp e
                          in t ++ "(" ++ ex ++ ")"

generateExps :: [Exp] -> String
generateExps [] = ""
generateExps [e] = generateExp e
generateExps (e:es) = let ex = generateExp e
                          exs = generateExps es
                       in ex ++ ", " ++ exs

-- Conditions

-- Generator for FArgs
generateArgs :: FArgs -> String
generateArgs [] = ""
generateArgs [Arg exps spec] =
  let es = generateExps exps
      s = generateSpec spec
   in es ++ " " ++ s
generateArgs ((Arg exps spec):args) = 
  let es = generateExps exps
      s = generateSpec spec
      as = generateArgs args
   in es ++ " " ++ s ++ ", " ++ as

-- Generator for Specs
generateSpec :: Spec -> String
generateSpec (SVar d) = generateDType d
generateSpec (SChan d) = let t = generateDType d
                          in "chan " ++ t

generateSpecs :: [Spec] -> String
generateSpecs [s] = generateSpec s
generateSpecs (s:ss) = let sp = generateSpec s
                           sps = generateSpecs ss
                        in sp ++ ", " ++ sps
generateSpecs _ = "error" -- should never occur

generateSpecx :: [Spec] -> String
generateSpecx [] = " "
generateSpecx ss = " (" ++ (generateSpecs ss) ++ ") "

-- Generator for cases
generateCase :: Case -> String -> String
generateCase (IfCase e s) i = let c = generateExp e
                                  b = generateStmt s (i ++ "  ")
                               in "if " ++ c ++ " {\n" ++ b ++ "\n" ++ i ++ "}"
generateCase (SwitchCase es s) i = let ms = generateExps es
                                       st = generateStmt s (i ++ "  ")
                                    in i ++ "case " ++ ms ++ " :\n" ++ st
generateCase (SelectCase (e,(SReceive to from)) stmt) i =
  let c = generateExp e
      var = generateExp to
      chan = generateExp from
      body = generateStmt stmt (i ++ "  ")
   in concat[i, "case ", var, " := <- func() {if ", c, " {return ", chan, "} return nil}():\n", body]
generateCase _ _ = "error" -- should never occur

generateCases :: [Case] -> String -> String
generateCases [c] i = generateCase c i
generateCases ((IfCase e s):cs) i = let case1 = generateCase (IfCase e s) i
                                        cases = generateCases cs i
                                     in case1 ++ " else " ++ cases
generateCases ((SwitchCase es s):cs) i = 
  let case1 = generateCase (SwitchCase es s) i
      cases = generateCases cs i
   in case1 ++ "\n" ++ cases
generateCases _ _ = "error"
--generateCases ((SelectCase (e,s) stmt):cs) i = undefined

-- Generator for Statements
generateStmt :: Stmt -> String -> String -- second input argument for indentation
generateStmt (SDef es1 es2) i = let exs1 = generateExps es1
                                    exs2 = generateExps es2
                                 in i ++ exs1 ++ " = " ++ exs2
generateStmt (SDecl es (SChan d) stmt) i = 
  let exs = generateExps es
      t = generateDType d
      st = generateStmt stmt i
   in i ++ exs ++ " := make(chan " ++ t ++ ")\n" ++ i ++ st
generateStmt (SDecl es spec stmt) i = 
  let exs = generateExps es
      sp = generateSpec spec
      st = generateStmt stmt i
   in i ++ "var " ++ exs ++ " " ++ sp ++ "\n" ++ i ++ st
generateStmt (SSeq "seq" (s:ss)) i = let st = generateStmt s i
                                         sts = generateStmt (SSeq "seq" ss) i
                                      in i ++ st ++ "\n" ++ sts
generateStmt (SSeq "seq" []) _ = ""
generateStmt (SIf "if" cs) i = 
  let cases = generateCases cs i
      post = "else {\n" ++ i ++ "os.Exit(1)\n" ++ i ++ "}"
   in i ++ cases ++ post
generateStmt (SSwitch e cs) i = 
  let sel = generateExp e
      opt = generateCases cs i
   in i ++ "switch " ++ sel ++ " {\n" ++ opt ++ "\n}" 
--generateStmt (SGo "par" (c:cs)) i
generateStmt (SSelect "alt" cs) i =
  let cases = generateSelect cs i
   in i ++ "select {\n" ++ cases ++ "\n" ++ i ++ "}"
generateStmt (SWhile e stmt) i = let c = generateExp e
                                     s = generateStmt stmt (i ++ "  ")
                                  in i ++ "for " ++ c ++ "{\n" ++ s ++ "\n" ++ i ++ "}"
generateStmt (SFor e1 e2 e3 stmt) i = 
  let index = generateExp e1
      base = generateExp e2
      count = generateExp e3
      body = generateStmt stmt (i ++ "  ")
      lim = show ((read base) + (read count))
   in case (read count) of
        0 -> ""
        _ -> concat [i, "for ", index, " := ", base, "; ", index, " < ", lim,
                     "; ", index, "++ {\n", body, "\n}"]
generateStmt (SCase c) i = generateCase c i
generateStmt (SCall e) i = i ++ (generateExp e)
generateStmt (SSend e1 e2) i = i ++ (generateExp e1) ++ " <- " ++ (generateExp e2)
generateStmt (SReceive e1 e2) i = let v = generateExp e1
                                      c = generateExp e2
                                   in case v of
                                        "nil" -> i ++ "<- " ++ c
                                        _ -> i ++ v ++ ":= <- " ++ c
generateStmt SContinue _ = ""
generateStmt SExit i = i ++ "os.Exit(1)"
generateStmt _ _ = "error"

-- Helper function for generating select statements
generateSelect :: [Stmt] -> String -> String
generateSelect [c] i = generateStmt c i
generateSelect (c:cs) i = let case1 = generateStmt c i
                              cases = generateSelect cs i
                           in case1 ++ "\n" ++ cases
generateSelect _ _ = "error" -- should never occur as alternation must have one case

-- Generator for Function
generateFun :: Fun -> String
generateFun (FFun name args specs stmt) =
  let as = generateArgs args
      ss = generateSpecx specs -- actually only relevant when occam functions are implemented
      st = generateStmt stmt "  "
   in "func " ++ name ++ "(" ++ as ++ ")" ++ ss ++ "{\n" ++ st ++ "\n}"

-- Generator for Program
generateProg :: Program -> String
generateProg [] = ""
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
                                         in fun ++ "\n\n" ++ main -- creating main fun
generateProg ((FFun name args specs stmt):fs) = let fun = generateFun (FFun name args specs stmt)
                                                    funs = generateProg fs
                                                 in fun ++ "\n" ++ funs

-- function for writing result to file
write :: Program -> String -> IO ()
write p s = let code = generateProg p
                pre = "package main\nimport fmt\nimport os\n"
             in do
                 writeFile (s ++ ".go") (pre ++ code)
