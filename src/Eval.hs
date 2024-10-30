{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval where

import Control.Monad.Trans.State.Lazy
import Language

type Env = [(String, Expr)]

newtype EvalM a = EvalM {unEval :: StateT Env (Either String) a}
    deriving (Functor, Applicative, Monad)

runEval :: EvalM a -> Either String a
runEval a = evalStateT (unEval a) emptyEnv
  where
    emptyEnv :: Env
    emptyEnv = []

eval :: Program -> EvalM ()
eval (Prog []) = pure ()
eval (Prog (x : xs)) = eval x >> eval (Prog xs)
eval (Procedure _name [] Nothing _stms) = undefined
eval (Procedure _name (_ : _) Nothing _stms) = undefined
eval (Procedure _name [] (Just _) _stms) = undefined
eval (Procedure _name (_ : _) (Just _) _stms) = undefined
