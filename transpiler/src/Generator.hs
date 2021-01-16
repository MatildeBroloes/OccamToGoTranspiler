-- Generator for converting ASTs into executable Go code
module Generator where

import GoAST
--import Text.StringRandom
import Test.RandomStrings
import System.IO
--import System.Random
--import Control.Monad.Writer

--type Generator a = Writer [String] (Either String a)

-- Generator for names -- should ensure that dots are replaced with underscores
generateName :: String -> String
generateName [] = ""
generateName (c:cs) | c == '.' = "_" ++ (generateName cs)
generateName (c:cs) = [c] ++ (generateName cs)

-- Generator for Values
generateVal :: Val -> String
generateVal TrueVal = "true"
generateVal FalseVal = "false"
generateVal NoneVal = "nil"
generateVal (IntVal i) = show i
generateVal (HexVal h) = h
generateVal (ByteVal (CharVal b)) = "\'" ++ [b] ++ "\'"
generateVal (ByteVal (IntVal b)) = show b
generateVal (StringVal s) = "\"" ++ s ++ "\""

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
generateOper Plus = "+"
generateOper Minus = "-"
generateOper Times = "*"
generateOper Div = "/"
generateOper Mod = "\\"
generateOper Eq = "=="
generateOper Neq = "!="
generateOper Less = "<"
generateOper Greater = ">"
generateOper Geq = ">="
generateOper Leq = "<="
generateOper And = "&&"
generateOper Or = "||"

-- Generator for Expresions
generateExp :: Exp -> String
generateExp (Const v) = generateVal v
generateExp (Var v) = generateName v
generateExp (Chan c) = generateName c
generateExp (Oper o e1 e2) = let op = generateOper o
                                 ex1 = generateExp e1
                                 ex2 = generateExp e2
                              in ex1 ++ " " ++ op ++ " " ++ ex2
generateExp (Not e) = "!" ++ (generateExp e)
generateExp (Call name args) = (generateName name) ++ "(" ++ (generateExps args) ++ ")"
generateExp (Array es) = 
  let exps = generateExps es
      t = "int" -- this should be changed to infer the type of the array
   in concat ["[...]", t, "{", exps, "}"] -- this is probably not correct, todo later
generateExp (Slice s exps) = let v = generateName s
                                 indexes = generateSlice exps
                              in v ++ indexes
generateExp (Conv d e) = let t = generateDType d
                             ex = generateExp e
                          in t ++ "(" ++ ex ++ ")"

generateExps :: [Exp] -> String
generateExps [] = ""
generateExps [e] = generateExp e
generateExps (e:es) = let ex = generateExp e
                          exs = generateExps es
                       in ex ++ ", " ++ exs

-- Helper function for generating slices
generateSlice :: [Exp] -> String
generateSlice [e] = let i = generateExp e
                     in "[" ++ i ++ "]"
generateSlice (e:es) = let i = generateExp e
                           is = generateSlice es
                        in i ++ is

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
   in concat[i, "case ", var, " = <-func() chan byte {if ", c, " {return ", chan, "} else {return nil}}() :\n", body]
generateCase (SelectCase (_, SContinue) _) i = i ++ "Not implemented: Case type cannot be generated"
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
generateCases ((SelectCase (e,s) stmt):cs) i = 
  let b = generateExp e
      st = generateStmt stmt (i ++ "  ")
   in case s of
        (SReceive e1 e2) ->
          let v = generateExp e1
              c = generateExp e2
           in concat [i, "case ", " = <-func() chan byte {if ", b, " {return ", c, "} else {return nil}}() :\n", i, st]
        (SContinue) -> "Guards with bool and SKIP not implemented"
generateCases _ _ = "error"

-- Helper function for generating select statements
generateSelect :: [Stmt] -> String -> String
generateSelect [(SCase c)] i = generateCase c i
generateSelect ((SCase c):cs) i = let case1 = generateCase c i
                                      cases = generateSelect cs i
                                   in case1 ++ "\n" ++ cases
generateSelect _ _ = "error" -- should never occur as alternation must have one case

-- Helper function for parallels
generateWGFun :: Stmt -> String -> String -> String
generateWGFun s i wg = let st = generateStmt s (i ++ "  ")
                        in unlines [i ++ wg ++ ".Add(1)",
                                    i ++ "go func() {",
                                    i ++ "  defer " ++ wg ++ ".Done()",
                                    st,
                                    i ++ "}()"]

generateWGFuns :: [Stmt] -> String -> String -> String
generateWGFuns [] _ _ = ""
generateWGFuns (s:ss) i wg = let fun = generateWGFun s i wg
                                 funs = generateWGFuns ss i wg
                              in fun ++ "\n" ++ funs

generateChans :: [Exp] -> String -> String -> String
generateChans [e] t i = let c = generateExp e
                         in i ++ "var " ++ c ++ " = make(chan " ++ t ++ ")"
generateChans (e:es) t i = let c = generateExp e
                               cs = generateChans es t i
                            in i ++ "var " ++ c ++ " = make(chan " ++ t ++ ")\n" ++ cs

-- Generator for Statements
generateStmt :: Stmt -> String -> String -- second input argument for indentation
generateStmt (SDef es1 es2) i = let exs1 = generateExps es1
                                    exs2 = generateExps es2
                                 in i ++ exs1 ++ " = " ++ exs2
generateStmt (SDecl es (SChan d) stmt) i = 
  let t = generateDType d
      exs = generateChans es t i
      st = generateStmt stmt i
   in exs ++ "\n" ++ st
generateStmt (SDecl es spec stmt) i = 
  let exs = generateExps es
      sp = generateSpec spec
      st = generateStmt stmt i
   in i ++ "var " ++ exs ++ " " ++ sp ++ "\n" ++ st
generateStmt (SSeq [s]) i = generateStmt s i
generateStmt (SSeq (s:ss)) i = let st = generateStmt s i
                                   sts = generateStmt (SSeq ss) i
                                in st ++ "\n" ++ sts
generateStmt (SSeq []) _ = ""
generateStmt (SIf cs) i = 
  let cases = generateCases cs i
      post = " else { os.Exit(1) }"
   in i ++ cases ++ post
generateStmt (SSwitch e cs) i = 
  let sel = generateExp e
      opt = generateCases cs i
   in i ++ "switch " ++ sel ++ " {\n" ++ opt ++ "\n}" 
generateStmt (SGo ss) i =
  let wg = "waitGroup" -- should be generated randomly
      pre = i ++ "var " ++ wg ++ " sync.WaitGroup\n"
      funs = generateWGFuns ss i wg
      post = i ++ wg ++ ".Wait()"
   in pre ++ funs ++ post
generateStmt (SSelect cs) i =
  let cases = generateSelect cs i
   in i ++ "select {\n" ++ cases ++ "\n" ++ i ++ "}"
generateStmt (SWhile e stmt) i = let c = generateExp e
                                     s = generateStmt stmt (i ++ "  ")
                                  in i ++ "for " ++ c ++ " {\n" ++ s ++ "\n" ++ i ++ "}"
generateStmt (SFor e1 e2 e3 stmt) i = 
  let index = generateExp e1
      base = generateExp e2
      count = generateExp e3
      body = generateStmt stmt (i ++ "  ")
      lim = show ((read base) + (read count))
   in case (read count) of
        0 -> ""
        _ -> concat [i, "for ", index, " := ", base, "; ", index, " < ", lim,
                     "; ", index, "++ {\n", body, "\n", i, "}"]
--generateStmt (SCase c) i = generateCase c i
generateStmt (SCall e) i = i ++ (generateExp e)
generateStmt (SSend e1 e2) i = i ++ (generateExp e1) ++ " <- " ++ (generateExp e2)
generateStmt (SReceive e1 e2) i = let v = generateExp e1
                                      c = generateExp e2
                                   in case v of
                                        "nil" -> i ++ "<-" ++ c
                                        _ -> i ++ v ++ " = <-" ++ c
generateStmt SContinue _ = ""
generateStmt SExit i = i ++ "os.Exit(1)"
generateStmt _ _ = "error"

-- Generator for Function
generateFun :: Fun -> String
generateFun (FFun name args specs stmt) =
  let nm = generateName name
      as = generateArgs args
      ss = generateSpecx specs -- actually only relevant when occam functions are implemented
      st = generateStmt stmt "  "
   in "func " ++ nm ++ "(" ++ as ++ ")" ++ ss ++ "{\n" ++ st ++ "\n}"

-- Generator for Program
generateProg :: Program -> String
generateProg [] = ""
generateProg [FFun name args [] stmt] = -- only for generating the top-level function
  let n = generateName name
      nm = generateName name
      as = generateArgs args
      cs = "  defer close(out)"
      st = generateStmt stmt "  "
      fun = "func " ++ nm ++ "(" ++ as ++ ") {\n" ++ cs ++ "\n\n" ++ st ++ "\n}"
      main = 
        unlines ["func main() {",
                 "  in, out, err := make(chan byte, 10), make(chan byte, 10), make(chan byte, 10)\n",
                 "  go " ++ n ++"(in, out, err)\n",
                 "  for i := range out {",
                 "    fmt.Print(string(i))",
                 "  }",
                 "}"]
   in fun ++ "\n\n" ++ main
generateProg ((FFun name args specs stmt):fs) = let n = generateName name
                                                    fun = generateFun (FFun n args specs stmt)
                                                    funs = generateProg fs
                                                 in fun ++ "\n" ++ funs

-- function for writing result to file
write :: String -> String -> IO ()
write f s = do
             file <- openFile f ReadMode
             p <- hGetContents file
             let code = generateProg (read p)
                 pre = "package main\n\nimport \"fmt\"\nimport \"sync\"\n\n"
              in do
                  writeFile (s ++ ".go") (pre ++ code)
             hClose file

