{-# LANGUAGE ExistentialQuantification #-}

module Evaluator
    ( eval
    , unwordsList
    ) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Common
import Control.Monad.Except
import Error
import Data.IORef
import VarAssign

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "if", pred, conseq , alt]) =
    do  result <- eval env pred
        case result of
            Bool False  -> eval env alt
            Bool True   -> eval env conseq
            _           -> throwError $ TypeMismatch "bool" pred
eval env (List [Atom "quote", val]) = return val
eval env form@(List (Atom "cond" : clauses)) =
    if null clauses
    then throwError $ BadSpecialForm "no true clause in cond expression: " form
    else case head clauses of
        List [Atom "else", expr] -> eval env expr
        List [test, expr]        -> eval env $ List [Atom "if", test, expr, List (Atom "cond": tail clauses)]
        _                        -> throwError $ BadSpecialForm "ill-formed cond expression: " form
eval env form@(List (Atom "case" : key : clauses)) =
    if null clauses
    then throwError $ BadSpecialForm "no true clause in case expression: " form
    else case head clauses of
        List (Atom "else": exprs) -> mapM (eval env) exprs >>= return . last
        List ((List datums) : exprs) -> do
            result <- eval env key
            equality <- mapM (\x -> liftThrows $ eqv [result, x]) datums
            if Bool True `elem` equality
            then mapM (eval env) exprs >>= return . last
            else eval env $ List (Atom "case" : key : tail clauses)
        _ -> throwError $ BadSpecialForm "ill-formed case expression: " form
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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
            ,("=", numBoolBinop (==))
            ,("<", numBoolBinop (<))
            ,(">", numBoolBinop (>))
            ,("/=", numBoolBinop (/=))
            ,(">=", numBoolBinop (>=))
            ,("<=", numBoolBinop (<=))
            ,("&&", boolBoolBinop (&&))
            ,("||", boolBoolBinop (||))
            ,("string=?", strBoolBinop (==))
            ,("string<?", strBoolBinop (<))
            ,("string>?", strBoolBinop (>))
            ,("string<=?", strBoolBinop (<=))
            ,("string>=?", strBoolBinop (>=))
            ,("car", car)
            ,("cdr", cdr)
            ,("cons", cons)
            ,("eqv?", eqv)
            ,("eq?", eqv)
            ,("equal?", equal)
            ,("string-length?", stringLen)
            ,("string-ref?", stringRef)
            ]
stringLen :: [LispVal] -> ThrowsError LispVal
stringLen [(String s)] = Right $ Number $ fromIntegral $ length s
stringLen [notString]  = throwError $ TypeMismatch "string" notString
stringLen badArgList   = throwError $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [(String s), (Number k)]
    | length s < k' + 1 = throwError $ Default "Out of bound error"
    | otherwise         = Right $ String $ [s !! k']
    where k' = fromIntegral k
stringRef [(String s), notNum] = throwError $ TypeMismatch "number" notNum
stringRef [notString, _] = throwError $ TypeMismatch "string" notString
stringRef badArgList = throwError $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x: xs)]          = return x
car [DottedList (x:xs) _]   = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)]           = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_:xs) x]   = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs]  = return $ List $ x:xs
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast
cons [x1,x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)]              = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)]          = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]          = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]              = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)]  = eqv [List $ xs ++ [x] , List $ ys ++ [y]]
eqv listPair@[List _, List _]               = eqvList eqv listPair
eqv [_,_]                                   = return $ Bool False
eqv badArgList                              = throwError $ NumArgs 2 badArgList


boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =    if length args /= 2
                                then throwError $ NumArgs 2 args
                                else do left <- unpacker $ args !! 0
                                        right <- unpacker $ args !! 1
                                        return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

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

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
    do  unpacked1 <- unpacker arg1
        unpacked2 <- unpacker arg2
        return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal listPair@[List _, List _] = eqvList equal listPair
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++[x], List $ ys ++ [y]]
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2) [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1,x2) = case eqvFunc [x1,x2] of
                                Left err -> False
                                Right (Bool val) -> val

