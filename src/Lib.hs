module Lib
    ( someFunc
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Parser (parseExpr)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

someFunc :: IO ()
someFunc = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)

