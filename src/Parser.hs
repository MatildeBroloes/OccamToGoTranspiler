-- Parser for Occam programs
module Parser where

import GoAST
import Text.Parsec hiding (State) -- exports ParseError type (?)
import Text.Parsec.Indent
import Control.Monad.State

import Data.Char(isDigit, isAlpha, isAlphaNum, isAscii, isPrint)

-- type IndentParser s u a = IndentParser s u (IndentT m) a
-- runIndentParser is equivalent to runParser, but for indentation sensitive parser
type IParser a = IndentParser String () a

-- Helper functions --

-- Skip white space (but not line breaks etc)
onlySpace :: IParser ()
onlySpace = skipMany (satisfy (\x -> x == ' '))

--- Skips whitespace and comments
lexeme :: IParser a -> IParser a
lexeme p = do a <- p; spaces; return a

--- Checks whether a string symbol matches the parser input
symbol :: String -> IParser ()
symbol s = lexeme $ do string s; return ()

-- Reserved keywords in Occam
reserved :: [String]
reserved = ["AFTER", "ALT", "AND", "ANY", "ASM", "AT", "BITAND", "BITNOT", "BITOR",
            "BOOL", "BYTE", "BYTESIN", "CASE", "CHAN OF", "DATA", "ELSE", "FALSE",
            "FOR", "FROM", "FUNCTION", "IF", "INLINE", "INT", "INT16", "INT32",
            "INT64", "IS", "MINUS", "MOSTNEG", "MOSTPOS", "NOT", "OFFSETOF", "OR",
            "PACKED", "PAR", "PLACE", "PLACED", "PLUS", "PORT OF", "PRI", "PROC",
            "PROCESSOR", "PROTOCOL", "REAL32", "REAL64", "RECORD", "REM", "RESHAPES",
            "RESULT", "RETYPES", "ROUND", "SEQ", "SIZE", "SKIP", "STOP", "TIMER",
            "TIMES", "TRUE", "TRUNC", "TYPE", "VAL", "VALOF", "WHILE"]

-- Terminal symbols --
--- Name
pName :: IParser String
pName = lexeme $ do
                  c <- letter
                  cs <- many (alphaNum <|> char '.')
                  notFollowedBy (alphaNum <|> char '.')
                  onlySpace
                  if c:cs `notElem` reserved then return $ c:cs
                  else fail "Name must not be reserved word"

--- Digits (and hex.digits)
pDigit :: IParser Int
pDigit = lexeme $ do ds <- many digit
                     return $ read ds

pHex :: IParser String
pHex = lexeme $ do ds <- many hexDigit
                   return $ ds 

--- Character
pChar :: IParser Char
pChar = do
         char '*'
         choice [(do char '\''; return '\''),
                 (do char '\"'; return '\"'),
                 (do char '*'; return '*'),
                 (do char 'n'; return '\n'),
                 (do char 'N'; return '\n'),
                 (do char 't'; return '\t'),
                 (do char 'T'; return '\t'),
                 (do char 's'; return ' '),
                 (do char 'S'; return ' ')]
        <|>
        do
         c <- satisfy (\x -> isAscii x && isPrint x && not (x == '\'' || x == '\"'))
         return c
--        <?> "Could not parse char"

--- String
pString :: IParser String
pString = do
           string "\""
           s <- many (pChar)
           symbol "\""
           return s

-- Parser functions --
parseDType :: IParser DType
parseDType = 
  try (do string "BOOL"; notFollowedBy (alphaNum <|> char '.'); return $ BOOL) <|>
  try (do string "BYTE"; notFollowedBy (alphaNum <|> char '.'); return $ BYTE) <|>
  try (do string "INT"; notFollowedBy (alphaNum <|> char '.'); return $ INT) <|>
--  try (do string "INT16"; notFollowedBy (alphaNum <|> char '.'); return $ INT16) <|>
--  try (do string "INT32"; notFollowedBy (alphaNum <|> char '.'); return $ INT32) <|>
--  try (do string "INT64"; notFollowedBy (alphaNum <|> char '.'); return $ INT64) <|>
--  try (do string "REAL32"; notFollowedBy (alphaNum <|> char '.'); return $ REAL32) <|>
--  try (do string "REAL64"; notFollowedBy (alphaNum <|> char '.'); return $ REAL64) <?>
  try (do symbol "[";
          e <- parseExp;
          symbol "]";
          d <- parseDType;
          case d of
            DArray es dt -> return $ DArray (e:es) dt
            _ -> return $ DArray [e] d )
    <?> "data type"
--    (do n <- pName; return $ DVar n)]--, // how to include this rule? should it be left out for simplicity?

parseSpecifier :: IParser Spec
parseSpecifier = do
                  symbol "CHAN"
                  symbol "OF"
                  dt <- parseDType
                  return $ SChan dt
                 <|>
                 do
                  dt <- parseDType
                  return $ SVar dt
              
--- Parse variable
parseVar :: IParser Exp
parseVar = do
            v <- pName
            return $ Var v

--- Parse channe
parseChan :: IParser Exp
parseChan = do
             c <- pName
             return $ Chan c

--- Parse variables (there must be at least one variable in a list of variables)
parseVars :: IParser [Exp]
parseVars = do
             v <- parseVar
             try (do symbol ","; vs <- parseVars; return $ v:vs)
               <|> (do return $ [v])

--- Parse Expression
parseExp :: IParser Exp
parseExp = try (do 
                 dt <- lexeme $ parseDType
                 v <- parseOperand
                 return $ Conv dt v)
               <|>
           try (do
                 string "NOT" -- only monadic operator included in simplified subset
                 symbol " "
                 o <- parseOperand
                 return $ Not o)
               <|>
           try (do
                 o1 <- parseOperand
                 onlySpace
                 try (do symbol "+"; o2 <- parseOperand; return $ Oper Plus o1 o2)
                   <|> try (do symbol "-"; o2 <- parseOperand; return $ Oper Minus o1 o2)
                   <|> try (do symbol "*"; o2 <- parseOperand; return $ Oper Times o1 o2)
                   <|> try (do symbol "/"; o2 <- parseOperand; return $ Oper Div o1 o2)
                   <|> try (do symbol "\\"; o2 <- parseOperand; return $ Oper Mod o1 o2)
                   <|> try (do symbol "="; o2 <- parseOperand; return $ Oper Eq o1 o2)
                   <|> try (do symbol "<>"; o2 <- parseOperand; return $ Oper Neq o1 o2)
                   <|> try (do symbol "<="; o2 <- parseOperand; return $ Oper Leq o1 o2)
                   <|> try (do symbol "<"; o2 <- parseOperand; return $ Oper Less o1 o2)
                   <|> try (do symbol ">="; o2 <- parseOperand; return $ Oper Geq o1 o2)
                   <|> try (do symbol ">"; o2 <- parseOperand; return $ Oper Greater o1 o2)
                   <|> try (do symbol "AND"; o2 <- parseOperand; return $ Oper And o1 o2)
                   <|> try (do symbol "OR"; o2 <- parseOperand; return $ Oper Or o1 o2)
                   <|> (do return $ o1))

-- parse list of 0 or more expressions, separated by commas
parseExps :: IParser [Exp]
parseExps = do
             e <- parseExp
             choice [(do symbol ","; es <- parseExps; return $ e:es),
                     (do return $ [e])]

--- TODO: use try to ensure all necessary branches are covered
parseOperand :: IParser Exp
parseOperand =
  try (do
        symbol "("
        e <- parseExp
        symbol ")"
        return $ e)
      <|>
  try (do
        symbol "["
        es <- (do symbol "]"; return $ []) <|> (do a <- parseExps; symbol "]"; return a)
        return $ List es)
      <|>
  try (do
        symbol "#"
        d <- pHex
        return $ Const $ HexVal d)
      <|>
  try (do
        s <- pString
        return $ Const $ StringVal s)
      <|>
  try (do
        symbol "*#"
        b1 <- hexDigit
        b2 <- hexDigit
        notFollowedBy hexDigit
        return $ Const $ ByteVal (b1:[b2]))
      <|>
  try (do
        symbol "TRUE"
        return $ Const TrueVal)
      <|>
  try (do
        symbol "FALSE"
        return $ Const FalseVal)
      <|>
  try (do
        name <- pName
        try (do symbol "("; args <- try parseExps <|> 
                                    (do return []); symbol ")"; return $ Call name args)
          <|> (do return $ Var name))
      <|>
      do
       d <- pDigit
       return $ Const $ IntVal d

-- Parsers for different types of processes
-- OBS: husk at bruge try, så den går ind i alle cases??
parseAsgn :: IParser  Stmt
parseAsgn = do
             vs <- parseVars
             symbol ":="
             es <- parseExps
             return $ SDef vs es

parseIn :: IParser Stmt
parseIn = do
           c <- parseChan
           symbol "?"
           vs <- parseInputItems
           return $ SReceive vs c

parseInputItems :: IParser [Exp]
parseInputItems = do
                   v <- parseVar
                   choice [(do symbol ";"; vs <- parseInputItems; return $ v:vs),
                           (do return $ [v])]

parseOut :: IParser Stmt
parseOut = do
            c <- parseChan
            symbol "!"
            msg <- parseExp
            return $ SSend c msg

parseOutputItems :: IParser [Exp]
parseOutputItems = do
                    e <- parseExp
                    choice [(do symbol ";"; es <- parseOutputItems; return $ e:es),
                            (do return $ [e])]

parseSeq :: IParser Stmt
parseSeq = withBlock SSeq (do symbol "SEQ"; return "seq") parseProcess

parseIf :: IParser Stmt
parseIf = do 
           s <- withBlock SIf (do symbol "IF"; return "if") parseCond
           return $ s

parseCond :: IParser Stmt
parseCond = do
             c <- withBlock SCond parseExp parseProcess
             return $ c

parseCase :: IParser Stmt
parseCase = do
             s <- withBlock SSwitch (do symbol "CASE"; parseExp) parseOpt
             return $ s

parseOpt :: IParser Stmt
parseOpt = try (do
                 o <- withBlock SCond (do symbol "ELSE"; return $ Const TrueVal) parseProcess
                 -- it is probably not necessary to unpack the result of withBlock 
                 return $ o)
                <|>
                try parseCond

parseWhile :: IParser Stmt
parseWhile = do
              s <- withBlock SWhile (do symbol "WHILE"; parseExp) parseProcess
              return $ s

parsePar :: IParser Stmt
parsePar = do
            s <- withBlock SGo (do symbol "PAR"; return "par") (try parseProcess <|> parseCall)
            return $ s

parseCall :: IParser Stmt
parseCall = do
             c <- parseOperand
             return $ SCall c
--             fname <- pName
--             symbol "("
--             args <- try parseExps <|> (do return [])
--             symbol ")"
--             return $ SCall fname args

parseAlt :: IParser Stmt
parseAlt = do
            s <- withBlock SSelect (do symbol "ALT"; return "alt") parseAlternative
            return $ s

parseAlternative :: IParser Stmt
parseAlternative = try (withBlock SCase parseIn parseProcess) <|>
                   try (withBlock SCond parseGuard parseProcess) <?>
                   "alternative"

parseGuard :: IParser Exp
parseGuard = try (do
                   b <- parseExp
                   symbol "&"
                   i <- parseIn
                   return $ Guard b i) <|>
             try (do
                   b <- parseExp -- should there be a seperate parser for boolean expressions?
                   symbol "&"
                   symbol "SKIP"
                   return $ b) <?>
             "guard"

--parsePHeader :: IParser (FName, FArgs, [Spec]) -- the definition of a func requires a [Spec]
--parsePHeader = do
--                symbol "PROC"
--                n <- pName
--                symbol "("
--                args <- parseArgs
--                symbol ")"
--                return $ (n, args, [])

-- this is equvalent to a specification containing a declaration. for true syntax, a specification should be able to contain either a declaration, or a definition
parseDecl :: IParser ([Exp], Spec)
parseDecl = do
             s <- lexeme $ parseSpecifier
             vs <- parseVars
             symbol ":"
             return $ (vs, s)

parseProcess :: IParser Stmt
parseProcess = try parseSeq <|> try parseIf <|> try parseCase <|> 
               try parseWhile <|> try parsePar <|> try parseAsgn <|>
               try parseIn <|> try parseOut <|> try parseAlt <|>
               try (do (es, s) <- parseDecl; p <- parseProcess; return $ SDecl es s p) <|>
               try (do symbol "SKIP"; return $ SContinue) <|> 
               try (do symbol "STOP"; return $ SExit) <?> "process"

-- parsing input arguments for procedures etc
parseFVars :: IParser [Exp]
parseFVars = do
              v <- parseVar
              vs <- try (do symbol ","; parseFVars) <|> (do return $ [])
              return $ v:vs

parseFormals :: IParser FArgs
parseFormals = do
                s <- lexeme $ parseSpecifier
                a <- parseFVars
                choice [(do symbol ","; as <- parseFormals; return $ (Arg a s):as),
                        (do return $ [Arg a s])]
               <|>
               do return $ []

-- Parsing procedure
parseProc :: IParser Fun
parseProc = do
             -- How to ensure that only one proces pr PROC header?
             -- p <- withBlock FFun parsePHeader (try parseDecl <|> parseProcess)
             symbol "PROC"
             fname <- pName
             symbol "("
             args <- parseFormals
             symbol ")"
             spaces
             body <- indented >> parseProcess
             checkIndent >> symbol ":" -- how to use same??
             -- should ensure that the colon is at the same indent
             -- spaces/lexeme?
             return $ FFun fname args [] body

-- Def is PROCedure or FUNCtion (in simplified version)
parseDef :: IParser Fun
parseDef = parseProc -- here we could extend with fx parsing of functions

parseDefs :: IParser Program
parseDefs = do 
             ds <- manyTill parseDef eof -- how to ensure that each def has a line break between
             return $ ds -- manyTill should return list (of functions)

parseProg :: IParser Program -- [Def] where Def is PROCedure or FUNCtion (in simplified version)
parseProg = do
             lexeme $ return () -- looking for white space or comments in the beginning of program
             parseDefs

parseString :: Either ParseError Fun
parseString = runIndentParser (do a <- parseProc; eof; return a) () "" input_text2
-- runIndentParser returns Either ParseError a

-- readFile :: String -> Either ParseError String
-- readFile fileName = do
--                      file <- openFile fileName ReadMode
--                      contents <- hGetContents file
--                      case iParse parseProg "test" contents of
--                        Left err -> putStr err
--                        Right a -> putStr a
--                      hClose file

input_text1 :: String
input_text1 = unlines ["PROC test (INT a)",
                       "  INT b:",
                       "  a,b := 1,2",
                       ":"
                      ]

input_text2 :: String
input_text2 = unlines ["PROC test2 (INT a, b, c, BOOL test)",
                       "  INT x, y, z:",
                       "  a,b := 1,2",
                       ":"
                      ]

input_text3 :: String
input_text3 = unlines ["PROC test3 (INT a, b, c)",
                       "  INT x, y, z:",
                       "  SEQ",
                       "    a := 1",
                       "    b := 2",
                       ":"
                      ]

input_text4 :: String
input_text4 = unlines ["PROC test4 (INT a, b, c)",
                       "  INT x, y, z:",
                       "  IF",
                       "    a <= b",
                       "      SEQ",
                       "        a := 1",
                       ":"
                      ]

input_text5 :: String
input_text5 = unlines ["PROC test5 (INT a,b)",
                       "  CASE a",
                       "    a <= b",
                       "      SEQ",
                       "        a := a + 1",
                       "        b := b - 1",
                       "    ELSE",
                       "      a := a - 1",
                       ":"
                      ]

input_text6 :: String
input_text6 = unlines ["PROC test6 (INT a,b)",
                       "  WHILE a < b",
                       "    SEQ",
                       "      a := a + 1",
                       "  BOOL test:",
                       "  test := TRUE",
                       ":"
                      ]

input_text7 :: String
input_text7 = unlines ["PROC test7 (CHAN OF INT a)",
                       "  INT tmp:",
                       "  a ! (BYTE a)",
                       "  a ? tmp",
                       ":"
                      ]

input_text8 :: String
input_text8 = unlines ["PROC test8 (CHAN OF INT a)",
                       "  PAR",
                       "    a ! (BYTE a)",
                       "    a ? tmp",
                       "    square(a)",
                       "    print (\"hej\")",
                       ":"
                      ]

input_text9 :: String
input_text9 = unlines ["PROC test9 (CHAN OF INT c1, c2, out)",
                       "  INT a,b:",
                       "  ALT",
                       "    c1 ? a",
                       "      out ! a",
                       "    c2 ? b",
                       "      out ! b",
                       ":"
                      ]

