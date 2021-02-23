module Common where

import Numeric
import Data.Ratio
import Data.Complex
import Data.Array
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.IORef
import Control.Monad.Except
import System.IO
    
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
    | Vector (Array Int LispVal)
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func {params :: [String], vararg :: (Maybe String),
            body :: [LispVal], closure :: Env}
    | IOFunc ([LispVal] -> IOThrowsError LispVal) 
    | Port Handle
-- Constructors and types have different namespaces, so you can have both a constructor named String and a type named String. Both types and constructor tags always begin with capital letters.
instance Eq LispVal where
    (Atom x) == (Atom y) = x == y
    (List xs) == (List ys) = xs == ys
    (DottedList xs x) == (DottedList ys y) = xs == ys && x == y
    (Number x) == (Number y) = x == y
    (String x) == (String y) = x == y
    (Bool x) == (Bool y) = x == y
    (Character x) == (Character y) = x == y
    (Float x) == (Float y) = x == y
    (Ratio x) == (Ratio y) = x == y
    (Complex x) == (Complex y) = x == y
    (Vector xa) == (Vector ya) =  xa == ya
    (PrimitiveFunc x) == (PrimitiveFunc y) = False
    (Func {params=paramsx, vararg=varargx, body=bodyx, closure=closurex}) == (Func {params=paramsy, vararg=varargy, body=bodyy, closure=closurey}) = paramsx == paramsy && varargx == varargy && bodyx == bodyy && closurex == closurey


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) = 
    "(lambda (" ++ unwords (map show args) ++
        (case varargs of
            Nothing -> ""
            Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"

instance Show LispVal where show = showVal

data LispError  = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

type ThrowsError = Either LispError
type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispError IO

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected" ++ show expected ++ "args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError


