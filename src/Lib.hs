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
import System.IO
import VarAssign

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val ->  return val

someFunc :: IO ()
someFunc = do
        args <- getArgs
        case length args of
            0 -> runRepl
            1 -> runOne $ args !! 0
            otherwise -> putStrLn "Program takes only 0 or 1 argument"

-- Repl functions
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint
