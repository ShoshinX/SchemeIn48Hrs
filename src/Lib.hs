module Lib
    ( someFunc
    , readExpr
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Parser (parseExpr)
import Evaluator (showVal, eval)
import Common

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val ->  val

someFunc :: IO ()
someFunc = getArgs >>= print . eval . readExpr . head
