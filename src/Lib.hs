module Lib
    ( someFunc
    , readExpr
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal 
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char
    | Float Double
    | Ratio Rational
    | Complex (Complex Double)
-- Constructors and types have different namespaces, so you can have both a constructor named String and a type named String. Both types and constructor tags always begin with capital letters.

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
parseNumber = 
    --do -- ex1.1.1
    --s <- many1 digit
    --return $ (Number . read) s
    --many1 digit >>= \x -> (return . Number . read) x -- ex1.1.2
    parseDecimal1 <|> parseDecimal2 <|> parseHex <|> parseOct <|> parseBin

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


someFunc :: IO ()
someFunc = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

