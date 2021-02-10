module Lib
    ( someFunc
    , readExpr
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Parser (parseExpr)
import Evaluator 
import Common
import Error
import Control.Monad.Except

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val ->  return val

someFunc :: IO ()
someFunc = do
        args <- getArgs
        evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
        putStrLn $ extractValue $ trapError evaled
