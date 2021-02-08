module Evaluator
    ( showVal
    , eval
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Common

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =[("+", numericBinop (+))
            ,("-", numericBinop (-))
            ,("*", numericBinop (*))
            ,("/", numericBinop div)
            ,("mod", numericBinop mod)
            ,("quotient", numericBinop quot)
            ,("remainder", numericBinop rem)
            ,("boolean?", unaryOp boolP)
            ,("pair?", unaryOp pairP)
            ,("list?", unaryOp listP)
            ,("vector?", unaryOp vectorP)
            ,("symbol?", unaryOp symbolP)
            ,("string?", unaryOp stringP)
            ,("char?", unaryOp charP)
            ,("number?", unaryOp numberP)
            ,("complex?", unaryOp complexP)
            ,("rational?", unaryOp rationalP)
            ,("integer?", unaryOp integerP)
            ,("procedure?", unaryOp procedureP)
            ,("symbol->string", unaryOp symToStr)
            ,("string->symbol", unaryOp strToSym)
            ]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

boolP :: LispVal -> LispVal
boolP (Bool _)  = Bool True
boolP _         = Bool False

pairP :: LispVal -> LispVal
pairP (DottedList _ _)  = Bool True
pairP _                 = Bool False

listP :: LispVal -> LispVal
listP (List _)  = Bool True
listP _         = Bool False

vectorP :: LispVal -> LispVal
vectorP (Vector _) = Bool True
vectorP _          = Bool False

symbolP :: LispVal -> LispVal
symbolP (Atom _) = Bool True
symbolP _        = Bool False

stringP :: LispVal -> LispVal
stringP (String _) = Bool True
stringP _          = Bool False

charP :: LispVal -> LispVal
charP (Character _) = Bool True
charP _             = Bool False

numberP :: LispVal -> LispVal
numberP (Number _) = Bool True
numberP (Ratio _) = Bool True
numberP (Complex _) = Bool True
numberP _          = Bool False

complexP :: LispVal -> LispVal
complexP (Complex _) = Bool True
complexP _           = Bool False

rationalP :: LispVal -> LispVal
rationalP (Ratio _) = Bool True
rationalP _         = Bool False

integerP :: LispVal -> LispVal
integerP (Number _) = Bool True
integerP _          = Bool False

procedureP :: LispVal -> LispVal
procedureP (List (Atom _:_)) = Bool True
procedureP _               = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

symToStr :: LispVal -> LispVal
symToStr (Atom x) = String x
symToStr _        = String ""

strToSym :: LispVal -> LispVal
strToSym (String x) = Atom x
strToSym _        = String ""
