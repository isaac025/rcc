{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Eval where

import Control.Monad.State.Lazy
import Language

type Env = [(String, Expr)]

data Stack = Stack
    { procs :: [(String, [Stm])]
    , env :: Env
    }

class (Monad m) => StackMonad m where
    updateStack :: String -> [Stm] -> m ()
    updateEnv :: String -> Expr -> m ()
    lookupStack :: String -> m (Maybe [Stm])
    lookupEnv :: String -> m (Maybe Expr)

newtype EvalM a = EvalM {unEval :: StateT Stack (Either String) a}
    deriving (Functor, Applicative, Monad, MonadState Stack)

runEval :: EvalM a -> Either String a
runEval a = evalStateT (unEval a) emptyStack
  where
    emptyStack :: Stack
    emptyStack = Stack [] []

instance StackMonad EvalM where
    updateStack name stms = do
        Stack{..} <- get
        put (Stack ((name, stms) : procs) env)
    updateEnv name expr = do
        Stack{..} <- get
        put (Stack procs ((name, expr) : env))
    lookupStack name = do
        procedures <- gets procs
        pure $ lookup name procedures
    lookupEnv name = do
        e <- gets env
        pure $ lookup name e

eval :: Program -> EvalM ()
eval (Prog []) = pure ()
eval (Prog (x : xs)) = eval x >> eval (Prog xs)
eval (Procedure "Main" [] Nothing stms) = mapM_ execStm stms
eval (Procedure "Main" [] (Just (VarStm v _)) stms) = do
    updateEnv v (Var v)
    mapM_ execStm stms
eval (Procedure "Main" _ (Just s) _) = error $ "Expecting variable declaration, got: " ++ show s
eval (Procedure "Main" ls _ _) = error $ "Main does not have arguments, cannot pass: " ++ show ls
eval (Procedure name [] Nothing stms) = updateStack name stms
eval (Procedure name vars Nothing stms) = do
    mapM_ (\(v, _) -> updateEnv v (Var v)) vars
    updateStack name stms
eval (Procedure _name [] (Just _) _stms) = undefined
eval (Procedure _name (_ : _) (Just _) _stms) = undefined

execStm :: Stm -> EvalM ()
execStm = undefined {-(ExprStm e) = evalExpr e
                    execStm (Return e) = evalExpr e
                    execStm (VarStm s _) = updateEnv s (Var s)
                    -}

{-
execStm (FunStm n es) = undefined
ConStm String Type
    | AssignStm String Stm
    | If Expr Stm Stm
    | Do Expr [Stm]
-}
evalExpr :: Expr -> EvalM ()
evalExpr = undefined

{- = I64 Int
    | I32 Int32
    | F64 Double
    | F32 Float
    | U64 Word64
    | U32 Word32
    | Boolean Bool
    | Str String
    | Var String
    | Fun String [Expr]
    | BinE BinOp Expr Expr
    deriving (Show)-}
