-- Parser for Occam programs
module Parser where

import AST
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Char(isDigit, isAlpha, isAlphaNum, isAscii, isPrint)

type Parser a = ReadP a

type ParseError = String

-- Helper functions --

lexeme :: Parser a -> Parser a
lexeme p = do a <- p; skipSpaces; return a

--- Checks whether a string symbol matches the parser input
symbol :: String -> Parser ()
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
pName :: Parser String
pName = lexeme $ do 
         c <- satisfy isAlpha
         cs <- munch (\x -> isAlphaNum x || (x == '.'))
         n <- look
-- Here, exploit that Haskell is lazy
         if c:cs `notElem` reserved && n == mempty then return $ c:cs
         else
           if c:cs `elem` reserved || (isAlphaNum (head n) || head n == '.') 
             then pfail
           else return $ c:cs

--- Digits (and hex.digits)
pDigit :: Parser Int
pDigit = do ds <- munch isDigit
            return $ read ds

hexAlpha :: [Char]
hexAlpha = ['A', 'B', 'C', 'D', 'E', 'F']

pHex :: Parser String
pHex = do ds <- munch (\x -> isDigit x || x `elem` hexAlpha)
          return $ ds 

--- Character
pChar :: Parser Char
pChar = do
         char '*'
         char '\''
         return '\''
        <|>
        do
         char '*'
         char '\"'
         return '\"'
        <|>
        do
         char '*'
         char '*'
         return '*'
        <|>
        do
         char '*'
         _ <- satisfy (\x -> (x == 'n' || x == 'N'))
         return '\n'
        <|>
        do
         char '*'
         _ <- satisfy (\x -> (x == 't' || x == 'T'))
         return '\t'
        <|>
        do
         char '*'
         _ <- satisfy (\x -> (x == 's' || x == 'S'))
         return ' '
-- OBS: need to handle byte literals
        <|>
        do
         c <- get
         if isAscii c && isPrint c && not ((c == '*' || c == '\'') || c == '\"') then
           return c
         else pfail

--- String
pString :: Parser String
pString = do
           symbol "\""
           s <- many (pChar)
           symbol "\""
           return $ s


pCType :: Parser ChannelType
pCType = do
          symbol "CHAN"
          symbol "OF"
          p <- parseProtocol
          return $ ChanOf p
         <|>
         do
          symbol "["
          es <- parseExps
          symbol "]"
          ct <- pCType
          return $ CList es ct

pDType :: Parser DataType
pDType = do
          symbol "BOOL"
          return $ BOOL
         <|>
         do
          symbol "BYTE"
          return $ BYTE
         <|>
         do
          symbol "INT"
          return $ INT
         <|>
         do
          symbol "INT16"
          return $ INT16
         <|>
         do
          symbol "INT32"
          return $ INT32
         <|>
         do
          symbol "INT64"
          return $ INT64
         <|>
         do
          symbol "REAL32"
          return $ BYTE
         <|>
         do
          symbol "REAL64"
          return $ BYTE
         <|>
         do
          n <- pName
          return $ DVar n
         <|>
         do
          symbol "["
          es <- parseExps
          symbol "]"
          dt <- pDType
          return $ DList es dt

pSpecifier :: Parser Specifier
pSpecifier = do
              ct <- pCType
              return $ CType ct
             <|>
             do
              dt <- pDType
              return $ DType dt
              

-- Parser functions --

--- Parse variable
---- Right now this is the same as a Name, for simplicity

--- Parse variables (there must be at least one variable in a list of variables)
parseVariables :: Parser [Variable]
parseVariables = do
                  v <- pName
                  symbol ","
                  vs <- parseVariables
                  return $ (Var v):vs
                 <|>
                 do
                  v <- pName
                  return $ [Var v]

--- Parse Expression
parseExp :: Parser Exp
parseExp = do
            n <- pName
            return $ EVar $ Var n
           <|>
           do
            symbol "("
            e <- parseExp
            symbol ")"
            return $ e
           <|>
           do
            n <- pName
            symbol "("
            es <- parseExps
            symbol ")"
            return $ List n es
           <|>
           do
            n <- pName
            symbol "("
            symbol ")"
            return $ List n []

--- Parse Expressions
parseExps :: Parser [Exp]
parseExps = do
             e <- parseExp
             symbol ","
             es <- parseExps
             return $ e:es
            <|>
            do
             e <- parseExp
             return $ [e]

--- Parse formal
parseFormal :: Parser Formal
parseFormal = do
               s <- pSpecifier
               vs <- parseVariables
               return $ ZVar vs s

--- Parse list of formals
parseFormals :: Parser [Formal]
parseFormals = do
                f <- parseFormal
                symbol ","
                fs <- parseFormals
                return $ f:fs
               <|>
               do
                return $ []

--- Parse protocol
parseProtocol :: Parser Protocol
parseProtocol = do
                 n <- pName
                 return $ ProtName n
                <|>
                do
                 d <- pDType
                 return $ Simple d

--- Parse process
parseProc :: Parser Process
parseProc = do
             vs <- parseVariables
             symbol ":="
             es <- parseExps
             return $ Asign vs es
            <|>
            do
             s <- parseSeq
             return $ Seq s

--- Parse multiple processes in sequence
---- OBS: take indentation into account
parseProcs :: Parser [Process]
parseProcs = do
              p <- parseProc
              --  \n and indentation?
              ps <- parseProcs
              return $ p:ps
             <|>
             do
              return $ []

--- Parse sequence
parseSeq :: Parser Sequence
parseSeq = do
            symbol "SEQ"
            n <- pName
            symbol "="
            b <- parseExp
            symbol "FOR"
            c <- parseExp
            p <- parseProc
            return $ SFor n b c p
           <|>
           do
            symbol "SEQ"
            ps <- parseProcs
            return $ SList $ ps

----- Parse definition
parseDef :: Parser Definition
parseDef = do
            symbol "PROC"
            n <- pName
            symbol "("
            fs <- parseFormals
            symbol ")"
            p <- parseProc
            return $ Proc n fs p

--parseProg :: Parser String
parseProg = undefined


parseString :: String -> Either ParseError String -- Program
parseString s = case readP_to_S (do a <- parseProg; eof; return a) s of
                  [] -> Left "No parse"
                  [(a, _)] -> Right a
                  _ -> Left "Parsing was ambiguous"
