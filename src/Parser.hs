module Parser
    ( parseExpr
    , spaces
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
import Common (LispVal(..))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"


spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many $ escapeChars <|> noneOf "\"\\"
    char '"'
    return $ String x

escapeChars :: Parser Char
escapeChars = do
    char '\\' -- backslash
    x <- oneOf "\\\"nrt" -- either backslash or quotation mark
    return $ case x of -- return escaped character
        '\\' -> x
        '\"' -> x
        'n'  -> '\n'
        'r'  -> '\r'
        't'  -> '\t'

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "newline" <|> string "space")
             <|> do {x <- anyChar; notFollowedBy alphaNum; return [x] }
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        otherwise -> (value !! 0)

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ Atom atom

parseBool :: Parser LispVal
parseBool = do
    char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseNumber :: Parser LispVal
parseNumber = parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

parseDecimal1 :: Parser LispVal
parseDecimal1 = many1 digit >>= (return . Number . read)

parseDecimal2 :: Parser LispVal
parseDecimal2 = do  try $ string "#d"
                    x <- many1 digit
                    (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do   try $ string "#x"
                x <- many1 hexDigit
                return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do   try $ string "#o"
                x <- many1 octDigit
                return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do   try $ string "#b"
                x <- many1 (oneOf "10")
                return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst.head $ readFloat (x++"."++y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

toDouble :: LispVal -> Double
toDouble (Float f) = realToFrac f
toDouble (Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do   x <- (try parseFloat <|> parseDecimal1)
                    char '+'
                    y <- (try parseFloat <|> parseDecimal1)
                    char 'i'
                    return $ Complex (toDouble x :+ toDouble y)


parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString 
         <|> try parseComplex
         <|> try parseFloat
         <|> try parseRatio
         <|> try parseNumber -- try is there because of the hash char
         <|> try parseBool
         <|> try parseCharacter
         <|> parseQuoted
         <|> try (do    string "#("
                        x <- parseVector
                        char ')'
                        return x)
         <|> parseList
         <|> parseQuasiQuoted
         <|> parseUnQuote
         <|> parseUnQuoteSplicing


parseList :: Parser LispVal
parseList = between beg end parseList1
    where   beg = (char '(' >> skipMany space)
            end = (skipMany space >> char ')')

parseList1 :: Parser LispVal
parseList1 = do list <- sepEndBy parseExpr spaces
                maybeDatum <- optionMaybe (char '.' >> spaces >> parseExpr)
                return $ case maybeDatum of
                    Nothing -> List list
                    Just datum -> DottedList list datum

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = do
    char ','
    char '@'
    x <- parseExpr
    return $ List [Atom "unquote-splicing", x]

parseVector :: Parser LispVal
parseVector = do    arrayValues <- sepBy parseExpr spaces
                    return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

