-- Parser for Occam programs
module Parser where

import GoAST
import Text.Parsec hiding (State)
import Text.Parsec.Indent
import System.IO
import Numeric (readHex)

import Data.Char(isAscii, isPrint)

type IParser a = IndentParser String () a

-- Helper functions --

-- Skip white space (but not line breaks etc)
onlySpace :: IParser ()
onlySpace = skipMany (satisfy (\x -> x == ' '))

--- Skips whitespace
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

--- Digits and hex.digits
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

--- String
pString :: IParser String
pString = do
           string "\""
           s <- many (pChar)
           symbol "\""
           return s

-- Parser functions --
--- Parse data type
parseDType :: IParser DType
parseDType = 
  try (do string "BOOL"; notFollowedBy (alphaNum <|> char '.'); return $ BOOL) <|>
  try (do string "BYTE"; notFollowedBy (alphaNum <|> char '.'); return $ BYTE) <|>
  try (do string "INT"; notFollowedBy (alphaNum <|> char '.'); return $ INT) <|>
  try (do symbol "[";
          e <- parseExp;
          symbol "]";
          d <- parseDType;
          case d of
            DArray es dt -> return $ DArray (e:es) dt
            _ -> return $ DArray [e] d )
    <?> "data type"

--- Parse Specifier
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
parseVar = try (do
                 name <- pName
                 do symbol "["
                 indexes <- parseArrayIndex
                 return $ Slice name indexes)
             <|> do
                  v <- pName
                  return $ Var v

--- Parse channel
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
                   <|> try (do symbol "REM"; o2 <- parseOperand; return $ Oper Rem o1 o2)
                   <|> try (do symbol "AND"; o2 <- parseOperand; return $ Oper And o1 o2)
                   <|> try (do symbol "OR"; o2 <- parseOperand; return $ Oper Or o1 o2)
                   <|> (do return $ o1))

--- Parse list of 0 or more expressions, separated by commas
parseExps :: IParser [Exp]
parseExps = do
             e <- parseExp
             choice [(do symbol ","; es <- parseExps; return $ e:es),
                     (do return $ [e])]

--- Parse Operand
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
        return $ Array es)
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
        h1 <- hexDigit
        h2 <- hexDigit
        notFollowedBy hexDigit
        let (x1,_):_ = readHex [h1]
            (x2,_):_ = readHex [h2]
         in return $ Const $ ByteVal $ IntVal ((x1*16) + x2))
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
        do symbol "("
        args <- try parseExps <|> (do return [])
        symbol ")"
        return $ Call name args)
      <|> 
  try parseVar 
      <|>
  try (do
        symbol "\'"
        c <- pChar
        symbol "\'"
        return $ Const $ ByteVal $ CharVal c)
      <|>
      do
       d <- pDigit
       return $ Const $ IntVal d

--- Helper function for array indexing
parseArrayIndex :: IParser [Exp]
parseArrayIndex = do
                   e <- parseExp
                   symbol "]"
                   choice [(do symbol "["; es <- parseArrayIndex; return $ e:es),
                           (do return $ [e])]
                   

--- Replicator
parseReplicator :: IParser (Exp, Exp, Exp)
parseReplicator = do
                   index <- parseVar
                   string "="
                   onlySpace
                   base <- parseExp
                   string "FOR"
                   onlySpace
                   count <- parseExp
                   return $ (index, base, count)

--- Parsers for different types of processes
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
           v <- parseVar
           return $ SReceive v c

parseOut :: IParser Stmt
parseOut = do
            c <- parseChan
            symbol "!"
            msg <- parseExp
            return $ SSend c msg

parseSeq :: IParser Stmt
parseSeq = try (do
                 symbol "SEQ"
                 (index, base, count) <- parseReplicator
                 p <- indented >> parseProcess
                 case count of
                   Const (IntVal 0) -> return SContinue
                   _ -> return $ SFor index base count $ SSeq [p])
             <|> do
                  p <- withBlock' (symbol "SEQ") parseProcess
                  return $ SSeq p

parseIf :: IParser Stmt
parseIf = try (do
                symbol "IF"
                (index, base, count) <- parseReplicator
                p <- indented >> parseBool
                case count of
                  Const (IntVal 0) -> return SContinue
                  _ -> return $ SFor index base count $ SIf [p])
            <|> do
                 p <- withBlock' (symbol "IF") parseBool
                 if null p -- changed so that if-processes with no choices behave as STOP
                    then return $ SExit
                 else return $ SIf p

parseBool :: IParser Case
parseBool = do
             e <- parseExp
             p <- indented >> parseProcess
             return $ IfCase e p
             

parseCase :: IParser Stmt
parseCase = withBlock SSwitch (do symbol "CASE"; parseExp) parseOpt

parseOpt :: IParser Case
parseOpt = try (do
                 symbol "ELSE"
                 p <- indented >> parseProcess
                 return $ SwitchCase [Const TrueVal] p)
                <|>
                do
                 es <- parseExps
                 p <- indented >> parseProcess
                 return $ SwitchCase es p

parseWhile :: IParser Stmt
parseWhile = do
              symbol "WHILE"
              e <- parseExp
              p <- indented >> parseProcess
              return $ SWhile e p

parsePar :: IParser Stmt
parsePar = try (do
                 symbol "PAR"
                 (index, base, count) <- parseReplicator
                 p <- indented >> (try parseProcess <|> parseCall)
                 case count of
                   Const (IntVal 0) -> return SContinue
                   _ -> return $ SFor index base count $ SGo [p])
             <|> do
                  p <- withBlock' (symbol "PAR") (try parseProcess <|> parseCall)
                  return $ SGo p

parseCall :: IParser Stmt
parseCall = do
             c <- parseOperand
             return $ SCall c

parseAlt :: IParser Stmt
parseAlt = try (do
                 symbol "ALT"
                 (index, base, count) <- parseReplicator
                 p <- indented >> parseAlternative
                 a <- case p of
                        SSelect [] -> return $ SExit
                        _ -> return $ p
                 case count of
                   Const (IntVal 0) -> return SContinue
                   _ -> return $ SFor index base count $ SSelect [a])
             <|> (do
                   p <- withBlock' (symbol "ALT") parseAlternative
                   case p of
                     [] -> return $ SExit
                     _ -> return $ SSelect p)

parseAlternative :: IParser Stmt
parseAlternative = try parseAlt <|> (do
                                      g <- parseGuard
                                      p <- indented >> parseProcess
                                      return $ SCase $ SelectCase g p)
                                <?> "alternative"

parseGuard :: IParser (Exp, Stmt)
parseGuard = try (do
                   i <- parseIn
                   return $ (Const TrueVal, i)) <|>
             try (do
                   b <- parseExp
                   symbol "&"
                   i <- parseIn
                   return $ (b, i)) <|>
             try (do
                   b <- parseExp
                   symbol "&"
                   symbol "SKIP"
                   return $ (b, SContinue)) <?>
             "guard"

--- Parse Declaration
parseDecl :: IParser ([Exp], Spec)
parseDecl = do
             s <- parseSpecifier
             onlySpace
             vs <- parseVars
             symbol ":"
             return $ (vs, s)

parseProcess :: IParser Stmt
parseProcess = try parseSeq <|> try parseIf <|> try parseCase <|> 
               try parseWhile <|> try parsePar <|> try parseAsgn <|>
               try parseIn <|> try parseOut <|> try parseAlt <|>
               try (do (es, s) <- parseDecl; p <- parseProcess; return $ SDecl es s p) <|>
               try (do symbol "SKIP"; return $ SContinue) <|> 
               try (do symbol "STOP"; return $ SExit)

--- Parse input arguments for procedures etc
parseFVars :: IParser [Exp]
parseFVars = do
              v <- parseVar
              vs <- try (do symbol ","; notFollowedBy parseFormals; parseFVars) <|> (do return $ [])
              return $ v:vs

parseFormals :: IParser FArgs
parseFormals = do
                s <- parseSpecifier
                onlySpace
                a <- parseFVars
                choice [(do symbol ","; as <- parseFormals; return $ (Arg a s):as),
                        (do return $ [Arg a s])]
               <|>
               do return $ []

--- Parsing procedure
parseProc :: IParser Fun
parseProc = do
             symbol "PROC"
             fname <- pName
             symbol "("
             args <- parseFormals
             symbol ")"
             body <- indented >> parseProcess
             checkIndent >> symbol ":"
             return $ FFun fname args [] body

--- Parsing definitions
parseDef :: IParser Fun
parseDef = parseProc -- here we could extend with fx parsing of functions

parseDefs :: IParser Program
parseDefs = do 
             ds <- manyTill parseDef eof
             return $ ds

-- Parsing Program
parseProg :: IParser Program
parseProg = do
             lexeme $ return () -- looking for white space in the beginning of program
             parseDefs

-- Parseing program as string, returning either error or parsed proram
parseString :: String -> Either ParseError Program
parseString s = runIndentParser (do a <- parseProg; eof; return a) () "" s

-- Function for writing result to file
writeParse :: String -> String -> IO ()
writeParse f s = do
                  file <- openFile f ReadMode
                  p <- hGetContents file
                  let ast = parseString p
                   in case ast of
                        Left err -> putStrLn "hej" --(show err)
                        Right a -> do
                                    writeFile (s ++ ".txt") (show a)
                  hClose file

