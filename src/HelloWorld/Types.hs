module HelloWorld.Types where

import Control.Monad.Except
import Data.IORef

import Text.Parsec.Error

type Env = IORef [(String, IORef HelloVal)]

type IOThrowsError = ExceptT HelloError IO

data HelloVal = Greeting String

instance Show HelloVal where
    show (Greeting s) = "Hello, World!"

instance Eq (HelloVal) where
    Greeting a == Greeting b = False
    _ == _ = False

type ThrowsError = Either HelloError

data HelloError = NumArgs Integer [HelloVal]
               | TypeMismatch String HelloVal
               | Parser ParseError
               | BadSpecialForm String HelloVal
               | NotFunction String String
               | UnboundVar String String
               | DivisionByZero
               | Default String

instance Show HelloError where show = showError

showError :: HelloError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected [])         = "Expected " ++ show expected
                                       ++ " args; none given"
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError (DivisionByZero)              = "Division by zero!"

unwordsList :: [HelloVal] -> String
unwordsList = unwords . map show
