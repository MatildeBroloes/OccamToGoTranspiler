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
                  if c:cs `notElem` reserved then return $ c:cs
                  else fail "Name must not be reserved word"

--         c <- letter
--         cs <- many (satisfy (\x -> isAlphaNum x || (x == '.')))
--         notFollowedBy (alphaNum <|> char '.')
--         n <- lookAhead
-- Here, exploit that Haskell is lazy
--         if c:cs `notElem` reserved && n == mempty then return $ c:cs
--         else
--           if c:cs `elem` reserved || (isAlphaNum (head n) || head n == '.') 
--             then fail
--           else return $ c:cs

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
  choice [(do string "BOOL"; notFollowedBy (alphaNum <|> char '.'); return $ BOOL),
    (do string "BYTE"; notFollowedBy (alphaNum <|> char '.'); return $ BYTE),
    (do string "INT"; notFollowedBy (alphaNum <|> char '.'); return $ INT),
    (do string "INT16"; notFollowedBy (alphaNum <|> char '.'); return $ INT16),
    (do string "INT32"; notFollowedBy (alphaNum <|> char '.'); return $ INT32),
    (do string "INT64"; notFollowedBy (alphaNum <|> char '.'); return $ INT64),
    (do string "REAL32"; notFollowedBy (alphaNum <|> char '.'); return $ REAL32),
    (do string "REAL64"; notFollowedBy (alphaNum <|> char '.'); return $ REAL64)]
--    (do n <- pName; return $ DVar n)]--, // how to include this rule? should it be left out for simplicity?
--                     (do symbol "[";
--                         e <- parseExp;
--                         symbol "]";
--                         d <- parseDType;
--                         case d of
--                           DArray es dt -> return $ DArray e:es dt
--                           _ -> return $ DArray [e] d )]
----                         return $ DArray e d)]

parseSpecifier :: IParser Spec
parseSpecifier = do
                  symbol "CHAN"
                  symbol "OF"
                  dt <- parseDType
                  return $ Chan dt
                 <|>
                 do
                  dt <- parseDType
                  return $ Data dt
              
--- Parse variable
parseVar :: IParser Exp
parseVar = do
            v <- pName
            return $ Var v

--- Parse variables (there must be at least one variable in a list of variables)
parseVariables :: IParser [Exp]
parseVariables = do
                  v <- parseVar
                  choice [(do symbol ","; vs <- parseVariables; return $ v:vs),
                          (do return $ [v])]

--- Parse Expression
parseExp :: IParser Exp
parseExp = try (do 
                 dt <- lexeme $ parseDType
                 v <- parseOperand
                 return $ Conv dt v)
               <|>
               (do
                 string "NOT" -- only monadic operator included in simplified subset
                 symbol " "
                 o <- parseOperand
                 return $ Not o)
               <|>
               (do
                 o1 <- parseOperand
                 choice [(do symbol "+"; o2 <- parseOperand; return $ Oper Plus o1 o2),
                         (do symbol "-"; o2 <- parseOperand; return $ Oper Minus o1 o2),
                         (do symbol "*"; o2 <- parseOperand; return $ Oper Times o1 o2),
                         (do symbol "/"; o2 <- parseOperand; return $ Oper Div o1 o2),
                         (do symbol "\\"; o2 <- parseOperand; return $ Oper Mod o1 o2),
                         (do symbol "="; o2 <- parseOperand; return $ Oper Eq o1 o2),
                         (do symbol "<"; choice [(do symbol ">"; o2 <- parseOperand; return $ Oper Neq o1 o2),
                                                 (do symbol "="; o2 <- parseOperand; return $ Oper Leq o1 o2),
                                                 (do o2 <- parseOperand; return $ Oper Less o1 o2)]),
                         (do symbol ">"; choice [(do symbol "="; o2 <- parseOperand; return $ Oper Geq o1 o2),
                                                 (do o2 <- parseOperand; return $ Oper Greater o1 o2)]),
                         (do symbol "AND"; o2 <- parseOperand; return $ Oper And o1 o2),
                         (do symbol "OR"; o2 <- parseOperand; return $ Oper Or o1 o2),
                         (do return $ o1)])

-- parse list of 0 or more expressions, separated by commas
parseExps :: IParser [Exp]
parseExps = do
             e <- parseExp
             choice [(do symbol ","; es <- parseExps; return $ e:es),
                     (do return $ [e])]

--- TODO: use try to ensure all necessary branches are covered
parseOperand :: IParser Exp
parseOperand = do
                symbol "("
                e <- parseExp
                symbol ")"
                return $ e
               <|>
               do
                symbol "["
                es <- (do symbol "]"; return $ []) <|> (do a <- parseExps; symbol "]"; return a)
                return $ List es
               <|>
               do
                symbol "#"
                d <- pHex
                return $ Const $ HexVal d
               <|>
               do
                s <- pString
                return $ Const $ StringVal s
               <|>
               do
                symbol "*#"
                b <- pHex -- should this control that there are only 2 digits in the hex number?
                return $ Const $ ByteVal b
               <|>
               do
                symbol "TRUE"
                return $ Const TrueVal
               <|>
               do
                symbol "FALSE"
                return $ Const FalseVal
               <|>
               do
                v <- pName
                return $ Var v
               <|>
               do
                d <- pDigit
                return $ Const $ IntVal d

--- OBS: arguments kan kun være af 1 data type - fiks dette senere!!
argVars :: IParser [Exp]
argVars = try (do
                symbol ","
                do
                 v <- parseVar
                 vs <- argVars
                 return $ v:vs
                <|>
                do return $ [])
               <|>
               (do return $ [])

parseArg :: IParser FArg
parseArg = do
            s <- lexeme $ parseSpecifier
            v <- parseVar
            vs <- argVars
            return $ Arg (v:vs) s

parseArgs :: IParser FArgs
parseArgs = do
             a <- parseArg
             as <- parseArgs
             return $ a:as
            <|>
            do return $ []

parsePHeader :: IParser (FName, FArgs, [Spec]) -- the definition of a func requires a [Spec]
parsePHeader = do
                symbol "PROC"
                n <- pName
                symbol "("
                args <- parseArgs
                symbol ")"
                return $ (n, args, [])

parseDecl :: IParser Stmt
parseDecl = do
             s <- lexeme $ parseSpecifier
             vs <- parseVariables
             symbol ":"
             return $ SDecl vs s

-- OBS: husk at bruge try, så den går ind i alle cases??
parseAsgn :: IParser  Stmt
parseAsgn = do
             vs <- parseVariables
             symbol ":="
             es <- parseExps
             return $ SDef vs es

parseSeq :: IParser Stmt
parseSeq = do
            s <- withBlock SSeq (do symbol "SEQ"; return "seq") parseProcess
            return $ s

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
parsePar = undefined

parseAlt :: IParser Stmt
parseAlt = undefined

parseInOut :: IParser Stmt
parseInOut = undefined

parseProcess :: IParser Stmt
parseProcess = try parseSeq <|> try parseIf <|> try parseCase <|> 
               try parseWhile <|> try parseDecl <|> parseAsgn

parseProc :: IParser Fun
parseProc = do
             p <- withBlock FFun parsePHeader parseProcess
             -- spaces/lexeme?
             return p -- the procedure header and body should be wrapped accordingly

-- Def is PROCedure or FUNCtion (in simplified version)
parseDef :: IParser Fun
parseDef = undefined

parseDefs :: IParser Program
parseDefs = do 
             ds <- manyTill parseDef eof
             return $ ds -- manyTill should return list (of functions)


parseProg :: IParser Program -- [Def] where Def is PROCedure or FUNCtion (in simplified version)
parseProg = do
             lexeme $ return () -- looking for white space or comments in the beginning of program
             parseDefs

parseString :: Either ParseError Fun
parseString = runIndentParser (do a <- parseProc; eof; return a) () "" input_text6 --s
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
                       "  a,b := 1,2"
                      ]

input_text2 :: String
input_text2 = unlines ["PROC test2 (INT a, b, c)",
                       "  INT x, y, z:",
                       "  a,b := 1,2"
                      ]

input_text3 :: String
input_text3 = unlines ["PROC test3 (INT a, b, c)",
                       "  INT x, y, z:",
                       "  SEQ",
                       "    a := 1",
                       "    b := 2"
                      ]

input_text4 :: String
input_text4 = unlines ["PROC test4 (INT a, b, c)",
                       "  INT x, y, z:",
                       "  IF",
                       "    a <= b",
                       "      SEQ",
                       "        a := 1"
                      ]

input_text5 :: String
input_text5 = unlines ["PROC test5 (INT a,b)",
                       "  CASE a",
                       "    a <= b",
                       "      SEQ",
                       "        a := a + 1",
                       "        b := b - 1",
                       "    ELSE",
                       "      a := a - 1"
                      ]

input_text6 :: String
input_text6 = unlines ["PROC test6 (INT a,b)",
                       "  WHILE a < b",
                       "    SEQ",
                       "      a := a + 1",
                       "  BOOL test:",
                       "  test := TRUE"
                      ]

