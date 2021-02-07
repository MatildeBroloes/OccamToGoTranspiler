-- Generator for converting ASTs into executable Go code
module Generator where

import GoAST
--import Test.RandomStrings
import System.IO
--import System.Random
import Control.Monad
--import Data.UnixTime

type Env = ([(VName, DType)], CountWG)

type CountWG = Int
type ProgData = [String]
type ImportData = [String]

data GenError = EVar VName | EOther String
  deriving (Eq, Show)

newtype Gen a = Gen {runGen :: Env -> (Either GenError a, ProgData, ImportData)}

instance Monad Gen where
  return a = Gen (\_ -> (Right a, mempty, mempty))
  m >>= f = Gen (\e -> case runGen m e of
                         (Left err, p0, i0) -> (Left err, p0, i0)
                         (Right a0, p0, i0) -> case runGen (f a0) e of
                                                 (Left err, p1, i1) -> (Left err, p0<>p1, i0<>i1)
                                                 (Right a1, p1, i1) -> (Right a1, p0<>p1, i0<>i1))

instance Functor Gen where
  fmap = liftM
instance Applicative Gen where
  pure = return; (<*>) = ap

-- Monad functions
abort :: GenError -> Gen a
abort err = Gen (\_ -> (Left err, mempty, mempty))

bindVar :: VName -> DType -> Gen a -> Gen a
bindVar v d m = Gen (\(e0, c) -> let e1 = (v,d):e0
                                  in runGen m (e1, c))

getType :: VName -> Gen DType
getType x = Gen (\(e, _) -> let lst = filter (\(v, _) -> v == x) e
                             in if null lst
                                   then (Left (EVar x), mempty, mempty)
                                else (Right (snd $ head lst), mempty, mempty))

write :: String -> Gen ()
write s = Gen (\_ -> (Right (), [s], mempty))

addImport :: String -> Gen ()
addImport s = Gen (\_ -> (Right (), mempty, [s]))

addWG :: Gen a -> Gen a
addWG m = Gen (\(e, c0) -> let c1 = c0 + 1
                            in runGen m (e, c1))

getWG :: Gen Int
getWG = Gen (\(_, c) -> (Right c, mempty, mempty))

-- Generator functions

-- Generator for names -- should ensure that dots are replaced with underscores
genName :: String -> Gen String
genName [] = return ""
genName (c:cs) | c == '.' = do
                             name <- genName cs
                             return $ "_" ++ name
genName (c:cs) = do
                  name <- genName cs
                  return $ [c] ++ name

-- Generator for Values
genVal :: Val -> Gen String
genVal TrueVal = return "true"
genVal FalseVal = return "false"
genVal NoneVal = return "nil"
genVal (IntVal i) = return $ show i
genVal (HexVal h) = return h
genVal (ByteVal (CharVal b)) = return $ "\'" ++ [b] ++ "\'"
genVal (ByteVal (IntVal b)) = return $ show b
genVal (StringVal s) = return $ "\"" ++ s ++ "\""


-- Generator for Types
genDType :: DType -> Gen String
genDType INT = return "int"
genDType BOOL = return "bool"
genDType BYTE = return "byte"
genDType (DArray exps d) =
  do
   a <- genArrayType exps
   t <- genDType d
   return $ a ++ t
genDType (DChan d) = do t <- genDType d; return $ "chan " ++ t

-- Helper function for generating array types
genArrayType :: [Exp] -> Gen String
genArrayType [e] =  do
                     v <- genExp e
                     return $ "[" ++ v ++ "]"
genArrayType (e:es) = do
                       v <- genExp e
                       vs <- genArrayType es
                       return $ "[" ++ v ++ "]" ++ vs
--genArrayType _ = abort (EOther "Empty array")


-- Generator for Operators
genOper :: Op -> Gen String
genOper Plus = return "+"
genOper Minus = return "-"
genOper Times = return "*"
genOper Div = return "/"
genOper Mod = return "\\"
genOper Eq = return "=="
genOper Neq = return "!="
genOper Less = return "<"
genOper Greater = return ">"
genOper Geq = return ">="
genOper Leq = return "<="
genOper And = return "&&"
genOper Or = return "||"

-- Generator for Expresions
genExp :: Exp -> Gen String
genExp (Const v) = genVal v
genExp (Var v) = genName v
genExp (Chan c) = genName c
genExp (Oper o e1 e2) = do op <- genOper o
                           ex1 <- genExp e1
                           ex2 <- genExp e2
                           return $ ex1 ++ " " ++ op ++ " " ++ ex2
genExp (Not e) = do ex <- genExp e; return $ "!" ++ ex
genExp (Call name args) = 
  do n <- genName name; as <- genExps args; return $ n ++ "(" ++ as ++ ")"
genExp (Array es) = do exs <- genExps es; return $ "{" ++ exs ++ "}"
genExp (Slice s exps) = do v <- genName s; is <- genSlice exps; return $ v ++ is
genExp (Conv d e) = do 
                     t <- genDType d
                     ex <- genExp e
                     return $ t ++ "(" ++ ex ++ ")"

genExps :: [Exp] -> Gen String
genExps [] = return ""
genExps [e] = genExp e
genExps (e:es) = do x <- genExp e; xs <- genExps es; return $ x ++ ", " ++ xs

-- Helper function for generating slices
genSlice :: [Exp] -> Gen String
genSlice [e] = do i <- genExp e; return $ "[" ++ i ++ "]"
genSlice (e:es) = do i <- genExp e; is <- genSlice es; return $ i ++ is

-- Helper function for generating array definitions
genArray :: Exp -> ([Exp], DType) -> Gen String
genArray e ([], d) = do
                      ex <- genExp e
                      t <- genDType d
                      return $ t ++ ex
genArray e ((te:tes), d) = do
                            x <- genExp te
                            r <- genArray e (tes, d)
                            return $ "[" ++ x ++ "]" ++ r

-- Generator for FArgs
genArgs :: FArgs -> Gen String
genArgs [] = return ""
genArgs [Arg exs sp] = do 
                        es <- genExps exs
                        s <- genSpec sp
                        return $ es ++ " " ++ s
genArgs ((Arg exs sp):args) = do 
                               es <- genExps exs
                               s <- genSpec sp
                               as <- genArgs args
                               return $ es ++ " " ++ s ++ ", " ++ as

-- Generator for Specs
genSpec :: Spec -> Gen String
genSpec (SVar d) = genDType d
genSpec (SChan d) = do t <- genDType d; return $ "chan " ++ t

genSpecs :: [Spec] -> Gen String
genSpecs [s] = genSpec s
genSpecs (e:es) = do s <- genSpec e; ss <- genSpecs es; return $ s ++ ", " ++ ss
--genSpecs _ = abort (EOther "Implementation faulty")

genSpecx :: [Spec] -> Gen String
genSpecx [] = return " "
genSpecx es = do ss <- genSpecs es; return $ " (" ++ ss ++ ") "


-- Helper function for generating if-statements
genCase :: [Case] -> String -> Gen String
genCase [] _ = return ""
genCase ((IfCase e s):cs) i = 
  do
   cond <- genExp e
   body <- genStmt s (i ++ "  ")
   cases <- genCase cs i
   return $ concat["if ", cond, " {\n", body, "\n", i, "} else ", cases]
genCase ((SwitchCase es s):cs) i = 
  do
   match <- genExps es
   stmt <- genStmt s (i ++ "  ")
   cases <- genCase cs i
   return $ concat [i, "case ", match, " :\n", stmt, "\n", cases]
genCase [SelectCase (e, (SReceive to from)) stmt] i =
  do
   cond <- genExp e
   var <- genExp to
   chan <- genExp from
   ctype <- (getType chan) >>= genDType
   body <- genStmt stmt (i ++ "  ")
   return $ concat [i, "case ", var, " = <-func() ", ctype, " {if ", 
                    cond, " {return ", chan, "} else {return nil}}() :\n", body]
genCase [SelectCase (_, SContinue) _] _ = 
  abort $ EOther "Select case cannot be generated"
genCase _ _ = abort $ EOther "Case type cannot be generates"


-- Helper functions for generating select statements
genSelect :: [Stmt] -> String -> Gen String
genSelect [] _ = return ""
genSelect ((SCase x):xs) i = do
                              c <- genCase [x] i
                              cs <- genSelect xs i
                              return $ c ++ "\n" ++ cs

                              
-- Helper function for generating wait group functions for parallels
genWG :: Gen String
genWG = do
         num <- getWG
         return $ "wg_" ++ (show num)

genWGFun :: Stmt -> String -> String -> Gen String
genWGFun s i wg = do
                   st <- genStmt s (i ++ "  ")
                   return $ unlines [i ++ wg ++ ".Add(1)",
                                     i ++ "go func() {",
                                     i ++ "  defer " ++ wg ++ ".Done()",
                                     st,
                                     i ++ "}()"]

genWGFuns :: [Stmt] -> String -> String -> Gen String
genWGFuns [] _ _ = return ""
genWGFuns (s:ss) i wg = do
                         fun <- genWGFun s i wg
                         funs <- genWGFuns ss i wg
                         return $ fun ++ "\n" ++ funs

-- Helper function for assignments (used to assign multiple vars at once)
defHelper :: [Exp] -> [Exp] -> Gen (String, String)
defHelper [(Var s)] [e] = 
  do
   v <- genExp (Var s)
   t <- getType v
   exp <- case t of
            DArray exps d -> genArray e (exps, d)
            _ -> genExp e
   return $ (v, exp)
defHelper [(Slice name exps)] [e] = 
  do
   s <- genExp (Slice name exps)
   t <- getType name
   exp <- case t of
            DArray _ (DArray aes d) -> genArray e (aes, d)
            _ -> genExp e
   return $ (s, exp)
defHelper (v:vs) (e:es) =
  do
   (r1, r2) <- defHelper [v] [e]
   (r1s, r2s) <- defHelper vs es
   return $ (r1 ++ ", " ++ r1s, r2 ++ ", " ++ r2s)
   

-- Generator for Statements
-- The second input argument is for ensuring correct indentation
genStmt :: Stmt -> String -> Gen String
genStmt (SDef [] []) _ = return ""
genStmt (SDef vs es) i =
  do
   (vars, exps) <- defHelper vs es
   return $ i ++ vars ++ " = " ++ exps ++ "\n"
genStmt (SDecl [] d stmt) i = genStmt stmt i
genStmt (SDecl (e:es) s stmt) i = 
  do
   v <- genExp e
   case s of
     (SChan d) -> do
                   t <- genDType d
                   r <- bindVar v (DChan d) $ genStmt (SDecl es s stmt) i
                   return $ concat [i, "var ", v, " = make(chan ", t, ")\n", r]
     (SVar d) -> do
                  t <- genDType d
                  r <- bindVar v d $ genStmt (SDecl es s stmt) i
                  return $ concat [i, "var ", v, " ", t, "\n", r]
genStmt (SSeq []) _ = return ""
genStmt (SSeq [s]) i = genStmt s i
genStmt (SSeq ((SGo s):ss)) i = 
  do p <- genStmt (SGo s) i; ps <- addWG $ genStmt (SSeq ss) i; return $ p ++ "\n" ++ ps
genStmt (SSeq (s:ss)) i = 
  do p <- genStmt s i; ps <- genStmt (SSeq ss) i; return $ p ++ "\n" ++ ps
genStmt (SIf xs) i = do
                      cs <- genCase xs i
                      _ <- addImport "import \"os\"\n"
                      return $ i ++ cs ++ "{ os.Exit(1) }"
genStmt (SSwitch e cs) i = do 
                            s <- genExp e
                            o <- genCase cs i
                            return $ concat [i, "switch ", s, " {\n", o, i, "}"]
genStmt (SGo s) i = do
                     wg <- genWG
                     funs <- genWGFuns s i wg
                     _ <- addImport "import \"sync\"\n"
                     return $ unlines [i ++ "var " ++ wg ++ " sync.WaitGroup\n",
                                       funs ++ i ++ wg ++ ".Wait()"]
genStmt (SSelect xs) i = do
                          cs <- genSelect xs i
                          return $ i ++ "select {\n" ++ cs ++ i ++ "}"
genStmt (SWhile e stmt) i = 
  do
   c <- genExp e
   s <- genStmt stmt (i ++ "  ")
   return $ concat [i, "for ", c, " {\n", s, "\n", i, "}"]
genStmt (SFor e1 e2 e3 stmt) i = 
  do
   index <- genExp e1
   base <- genExp e2
   count <- genExp e3
   body <- genStmt stmt (i ++ "  ")
   lim <- return $ show ((read base) + (read count))
   return $ concat [i, "for ", index, " := ", base, "; ", index, " < ", lim, 
                    "; ", index, "++ {\n", body, "\n", i, "}"]
genStmt (SCall e) i = do f <- genExp e; return $ i ++ f
genStmt (SSend e1 e2) i = 
  do c <- genExp e1; m <- genExp e2; return $ i ++ c ++ " <- " ++ m
genStmt (SReceive e1 e2) i = do 
                              v <- genExp e1
                              c <- genExp e2
                              case v of
                                "nil" -> return $ i ++ "<-" ++ c
                                _ -> return $ i ++ v ++ " = <-" ++ c
genStmt SContinue _ = return ""
genStmt SExit i = do
                   _ <- addImport "import \"os\"\n"
                   return $ i ++ "os.Exit(1)"
genStmt _ _ = abort $ EOther "Error when generating statement"


-- Generator for Function
genFun :: Fun -> Gen String
genFun (FFun name args specs stmt) =
  do 
   head <- genHead name args specs
   st <- genStmt stmt "  "
   return $ concat [head, "{\n", st, "\n}"]

-- Generator for function headers
genHead :: FName -> FArgs -> [Spec] -> Gen String
genHead name args specs = do
                           n <- genName name
                           as <- genArgs args
                           ss <- genSpecx specs
                           return $ concat ["func ", n, "(", as, ")", ss]

-- Function for generating program
genProg :: Program -> Gen ()
genProg [FFun name args specs stmt] =
  do
   head <- genHead name args specs
   co <- return "  defer close(out)"
   ce <- return "  defer close(err)"
   st <- genStmt stmt "  "
   fun <- return $ head ++ "{\n" ++ co ++ "\n" ++ ce ++ "\n\n" ++ st ++ "\n}"
   _ <- addImport "import \"fmt\"\n"
   _ <- addImport "import \"sync\"\n"
   _ <- addImport "import \"os\"\n"
   _ <- addImport "import \"bufio\"\n"
   main <- return $ unlines ["func main() {",
                             "  in, out, err := make(chan byte, 10), make(chan byte, 10), make(chan byte, 10)\n",
                             "  var wg_main sync.WaitGroup\n",
                             "  wg_main.Add(1)",
                             "  go func() {",
                             "    " ++ name ++ "(in, out, err)\n",
                             "    wg_main.Done()",
                             "  }()\n",
                             "  go func() {",
                             "    input := bufio.NewReader(os.Stdin)",
                             "    for {",
                             "      char, _, err := input.ReadRune()",
                             "      if err == nil {in <- byte(char)}", -- maybe put a timer on this action?
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
   write $ fun ++ "\n\n" ++ main
genProg (x:xs) = do 
                  f <- genFun x
                  write $ f ++ "\n\n"
                  genProg xs
genProg [] = return ()

-- Function for extracting result of generating program
generate :: Program -> ([String], Maybe GenError)
generate p0 = let (e, p, i) = runGen (genProg p0) ([], 0) -- empty starting environment
                  imports = foldl (\acc x -> if x `elem` acc then acc else acc<>[x]) [] i
               in case e of
                    Left err -> ((imports<>["\n"])<>p, Just err)
                    _ -> ((imports<>["\n"])<>p, Nothing)


-- function for writing result to file
writeGen :: String -> String -> IO ()
writeGen f s = 
  do
   file <- openFile f ReadMode
   p <- hGetContents file
   let (code, m) = generate (read p)
       pre = "package main\n\n"
    in case m of
         Just err -> putStrLn "Error: could not generate program"
         Nothing -> writeFile (s ++ ".go") (pre ++ (concat code))
   hClose file

