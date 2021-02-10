module Evaluator
    ( eval
    , unwordsList
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Common
import Control.Monad.Except
import Error

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []    = throwError $ NumArgs 2 []
numericBinop op singleVal@[_]   = throwError $ NumArgs 2 singleVal
numericBinop op params          = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                            if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

symToStr :: LispVal -> LispVal
symToStr (Atom x) = String x
symToStr _        = String ""

strToSym :: LispVal -> LispVal
strToSym (String x) = Atom x
strToSym _        = String ""
