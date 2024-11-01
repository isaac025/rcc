{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Semantics where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Language

type Env = [(String, Value)]

data Stack = Stack
    { procs :: IORef [(String, Procedure)]
    , env :: IORef Env
    }

data EvalError
    = TypeError Value Value
    | UndeclaredVariable String
    | RedeclaredVariable String
    | UndeclaredProcedure String
    | ArgumentMismatch String Int Int
    | GeneralError String
    deriving (Show)

errorMessage :: EvalError -> String
errorMessage (TypeError expected actual) =
    "Type error: expected " ++ show expected ++ ", but got " ++ show actual ++ "."
errorMessage (UndeclaredVariable v) =
    "Semantic error: undeclared variable '" ++ v ++ "'."
errorMessage (RedeclaredVariable v) =
    "Semantic error: variable '" ++ v ++ "' is already declared in the current scope."
errorMessage (UndeclaredProcedure p) =
    "Semantic error: procedure '" ++ p ++ "' is not defined."
errorMessage (ArgumentMismatch p expected actual) =
    "Argument error in procedure '"
        ++ p
        ++ "': expected "
        ++ show expected
        ++ " arguments, but got "
        ++ show actual
        ++ "."
errorMessage (GeneralError msg) = msg

newtype SemanticsM a = SemanticsM {unSemantics :: ExceptT EvalError (ReaderT Stack IO) a}
    deriving (Functor, Applicative, Monad, MonadReader Stack, MonadIO, MonadError EvalError)

semanticsEval :: Stack -> SemanticsM a -> IO (Either String a)
semanticsEval s a = do
    result <- runReaderT (runExceptT (unSemantics a)) s
    pure $ either (Left . errorMessage) Right result

class (Monad m) => MonadStack m where
    updateStack :: String -> Procedure -> m ()
    updateEnv :: String -> Value -> m ()
    lookupStack :: String -> m Procedure
    lookupEnv :: String -> m Value

instance MonadStack SemanticsM where
    updateStack name proc = do
        p <- asks procs
        liftIO $ modifyIORef' p ((name, proc) :)
    updateEnv name val = do
        e <- asks env
        environment <- liftIO $ readIORef e
        case lookup name environment of
            Just _ -> throwError $ RedeclaredVariable name
            Nothing -> liftIO $ modifyIORef' e ((name, val) :)
    lookupStack name = do
        p <- asks procs
        procedures <- liftIO $ readIORef p
        case lookup name procedures of
            Nothing -> throwError $ UndeclaredProcedure name
            Just proc -> pure proc
    lookupEnv name = do
        e <- asks env
        environment <- liftIO $ readIORef e
        case lookup name environment of
            Nothing -> throwError $ UndeclaredVariable name
            Just v -> pure v

initStack :: IO Stack
initStack = do
    e <- newIORef []
    ps <- newIORef []
    pure $ Stack ps e

analyzeProgram :: Program -> SemanticsM ()
analyzeProgram (Program []) = pure ()
analyzeProgram (Program (p : ps)) = analyzeProcedure p >> analyzeProgram (Program ps)

analyzeProcedure :: Procedure -> SemanticsM ()
analyzeProcedure p@(Procedure name _ _ stms) = do
    updateStack name p
    mapM_ analyzeStm stms
analyzeStm :: Stm -> SemanticsM ()
analyzeStm (ExprStm expr) = analyzeExpr expr
analyzeStm (Return expr) = analyzeExpr expr
analyzeStm (FunStm name _) = do
    proc <- lookupStack name
    pure ()

analyzeExpr :: Expr -> SemanticsM ()
analyzeExpr (I64 _) = pure ()
analyzeExpr (I32 _) = pure ()
analyzeExpr (F64 _) = pure ()
analyzeExpr (F32 _) = pure ()
analyzeExpr (U64 _) = pure ()
analyzeExpr (U32 _) = pure ()
analyzeExpr (Boolean _) = pure ()
analyzeExpr (Str _) = pure ()
analyzeExpr (Var n) = do
    n <- lookupEnv n
    pure ()

-- analyzeExpr (Fun String [Expr])
