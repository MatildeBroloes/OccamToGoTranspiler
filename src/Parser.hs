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

{-         c <- letter
         cs <- many (satisfy (\x -> isAlphaNum x || (x == '.')))
         notFollowedBy (alphaNum <|> char '.')
         n <- lookAhead
-- Here, exploit that Haskell is lazy
         if c:cs `notElem` reserved && n == mempty then return $ c:cs
         else
           if c:cs `elem` reserved || (isAlphaNum (head n) || head n == '.') 
             then fail
           else return $ c:cs -}

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
           symbol "\""
           s <- many (pChar)
           symbol "\""
           return s

-- Parser functions --
parseDType :: IParser DType
parseDType = choice [(do symbol "BOOL"; return $ BOOL),
                     (do symbol "BYTE"; return $ BYTE),
                     (do symbol "INT"; return $ INT),
                     (do symbol "INT16"; return $ INT16),
                     (do symbol "INT32"; return $ INT32),
                     (do symbol "INT64"; return $ INT64),
                     (do symbol "REAL32"; return $ REAL32),
                     (do symbol "REAL64"; return $ REAL64),
                     (do n <- pName; return $ DVar n)]--,
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
---- Right now this is the same as a Name, for simplicity

--- Parse variables (there must be at least one variable in a list of variables)
parseVariables :: IParser [Exp]
parseVariables = do
                  v <- pName
                  choice [(do symbol ","; vs <- parseVariables; return $ (Var v):vs),
                          (do return $ [Var v])]

--- Parse Expression
parseExp :: IParser Exp
parseExp = do
            string "NOT" -- only monadic operator included in sipmlified subset
            symbol " "
            o <- parseOperand
            return $ Not o
           <|>
           do
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
                    (do return $ o1),
                    (do dt <- parseDType; o <- parseOperand; return $ Conv dt o)]

-- parse list of 0 or more expressions, separated by commas
parseExps :: IParser [Exp]
parseExps = do
             e <- parseExp
             choice [(do symbol ","; es <- parseExps; return $ e:es),
                     (do return $ [e])]

parseOperand :: IParser Exp
parseOperand = do
                symbol "("
                e <- parseExp
                symbol ")"
                return $ e
               <|>
               do
                v <- pName
                return $ Var v
               <|>
               do
                symbol "#"
                d <- pHex
                return $ Const $ HexVal d
               <|>
               do
                d <- pDigit
                return $ Const $ IntVal d
--               <|>
--               do
--                symbol "*#"
--                b <- pHex -- should this control that there are only 2 digits in the hex number?
--                return $ Const $ ByteVal b
--               <|>
--               do
--                s <- pString
--                return $ Const $ StringVal s
               <|>
               do
                symbol "["
                symbol "]"
                return $ List []
            --    choice [(do es <- parseExps; symbol "]"; return $ List es),
            --            (do symbol "]"; return $ List [])]

parseProg :: IParser (Either Int String)
parseProg = try (do
                  s <- pName
                  return $ Right s
                 <|> 
                 do
                  d <- pDigit
                  return $ Left d)

parseString :: String -> Either ParseError Exp
parseString s = runIndentParser (do a <- parseExp; eof; return a) () "" s
-- runIndentParser returns Either ParseError a

-- readFile :: String -> Either ParseError String
-- readFile fileName = do
--                      file <- openFile fileName ReadMode
--                      contents <- hGetContents file
--                      case iParse parseProg "test" contents of
--                        Left err -> putStr err
--                        Right a -> putStr a
--                      hClose file
