module HelloWorld.Interpreter
    (
      evalString
    , nullEnv
    ) where

import Control.Monad
import Control.Monad.Except
import Control.Applicative hiding ((<|>), many, optional)
import Data.IORef

import Text.ParserCombinators.Parsec hiding (spaces)

import HelloWorld.Types

parseGreeting :: Parser HelloVal
parseGreeting = do
                x <- string "  Hello, World!"
                return $ Greeting x

parseExpr :: Parser HelloVal
parseExpr = parseGreeting

readExpr :: String -> ThrowsError HelloVal
readExpr input = case parse parseExpr "your face" input of
    Left err -> throwError $ Parser err
    Right val -> return val

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

trapError :: IOThrowsError String -> IOThrowsError String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

eval :: Env -> HelloVal -> IOThrowsError HelloVal
eval env val@(Greeting _) = return val
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError HelloVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> HelloVal -> IOThrowsError HelloVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> HelloVal -> IOThrowsError HelloVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

bindVars :: Env -> [(String, HelloVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)
