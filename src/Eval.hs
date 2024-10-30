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
eval (Procedure name [] Nothing stms) = undefined
