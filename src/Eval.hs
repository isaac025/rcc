{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Eval where

import Control.Monad.State.Lazy
import Data.Fixed (mod')
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
eval (Procedure "Main" [] Nothing stms) = mapM_ evalStm stms
eval (Procedure "Main" [] (Just (VarStm v _)) stms) = do
    updateEnv v (Var v)
    mapM_ evalStm stms
eval (Procedure "Main" _ (Just s) _) = error $ "Expecting variable declaration, got: " ++ show s
eval (Procedure "Main" ls _ _) = error $ "Main does not have arguments, cannot pass: " ++ show ls
eval (Procedure name [] Nothing stms) = updateStack name stms
eval (Procedure name vars Nothing stms) = do
    mapM_ (\(v, _) -> updateEnv v (Var v)) vars
    updateStack name stms
eval (Procedure _name [] (Just _) _stms) = undefined
eval (Procedure _name (_ : _) (Just _) _stms) = undefined

evalStm :: Stm -> EvalM Value
evalStm = undefined {-(ExprStm e) = evalExpr e
                    evalStm (Return e) = evalExpr e
                    evalStm (VarStm s _) = updateEnv s (Var s)
                    -}

{-
evalStm (FunStm n es) = undefined
ConStm String Type
    | AssignStm String Stm
    | If Expr Stm Stm
    | Do Expr [Stm]
-}
evalExpr :: Expr -> EvalM Value
evalExpr (I64 i) = pure (I64V i)
evalExpr (I32 i) = pure (I32V i)
evalExpr (U64 i) = pure (U64V i)
evalExpr (U32 i) = pure (U32V i)
evalExpr (F64 i) = pure (F64V i)
evalExpr (F32 i) = pure (F32V i)
evalExpr (Boolean i) = pure (BoolV i)
evalExpr (Str i) = pure (StrV i)
evalExpr (Var i) = pure (VarV i)
evalExpr (BinE op e1 e2) = binaryDecision op <$> evalExpr e1 <*> evalExpr e2
evalExpr (Fun name _args) = do
    mstms <- lookupStack name
    case mstms of
        Nothing -> error $ "Procedure " ++ name ++ " not defined"
        Just stms -> last <$> mapM evalStm stms

binaryDecision :: BinOp -> (Value -> Value -> Value)
binaryDecision Add = add'
binaryDecision Sub = sub'
binaryDecision Mul = mul'
binaryDecision Div = div'
binaryDecision Mod = mod''
binaryDecision And = and'
binaryDecision Or = or'
binaryDecision XOr = xor'
binaryDecision Gt = gt'
binaryDecision GtE = gte'
binaryDecision Lt = lt'
binaryDecision LtE = lte'
binaryDecision Equal = eq'
binaryDecision NEqual = neq'

add' :: Value -> Value -> Value
add' (I64V x) (I64V y) = I64V (x + y)
add' (I32V x) (I32V y) = I32V (x + y)
add' (U64V x) (U64V y) = U64V (x + y)
add' (U32V x) (U32V y) = U32V (x + y)
add' (F64V x) (F64V y) = F64V (x + y)
add' (F32V x) (F32V y) = F32V (x + y)
add' _ _ = error "+ expects number"

sub' :: Value -> Value -> Value
sub' (I64V x) (I64V y) = I64V (x - y)
sub' (I32V x) (I32V y) = I32V (x - y)
sub' (U64V x) (U64V y) = U64V (x - y)
sub' (U32V x) (U32V y) = U32V (x - y)
sub' (F64V x) (F64V y) = F64V (x - y)
sub' (F32V x) (F32V y) = F32V (x - y)
sub' _ _ = error "- expects number"

mul' :: Value -> Value -> Value
mul' (I64V x) (I64V y) = I64V (x * y)
mul' (I32V x) (I32V y) = I32V (x * y)
mul' (U64V x) (U64V y) = U64V (x * y)
mul' (U32V x) (U32V y) = U32V (x * y)
mul' (F64V x) (F64V y) = F64V (x * y)
mul' (F32V x) (F32V y) = F32V (x * y)
mul' _ _ = error "* expects number"

div' :: Value -> Value -> Value
div' (I64V x) (I64V y) = I64V (x `div` y)
div' (I32V x) (I32V y) = I32V (x `div` y)
div' (U64V x) (U64V y) = U64V (x `div` y)
div' (U32V x) (U32V y) = U32V (x `div` y)
div' (F64V x) (F64V y) = F64V (x / y)
div' (F32V x) (F32V y) = F32V (x / y)
div' _ _ = error "/ expects number"

mod'' :: Value -> Value -> Value
mod'' (I64V x) (I64V y) = I64V (x `mod` y)
mod'' (I32V x) (I32V y) = I32V (x `mod` y)
mod'' (U64V x) (U64V y) = U64V (x `mod` y)
mod'' (U32V x) (U32V y) = U32V (x `mod` y)
mod'' (F64V x) (F64V y) = F64V (x `mod'` y)
mod'' (F32V x) (F32V y) = F32V (x `mod'` y)
mod'' _ _ = error "+ expects number"

and' :: Value -> Value -> Value
and' (BoolV x) (BoolV y) = BoolV (x && y)
and' _ _ = error "/\\ expects boolean"

or' :: Value -> Value -> Value
or' (BoolV x) (BoolV y) = BoolV (x || y)
or' _ _ = error "\\/ expects boolean"

xor' :: Value -> Value -> Value
xor' (BoolV x) (BoolV y) = BoolV (x /= y)
xor' _ _ = error "^ expects boolean"

gt' :: Value -> Value -> Value
gt' (I64V x) (I64V y) = BoolV (x > y)
gt' (I32V x) (I32V y) = BoolV (x > y)
gt' (U64V x) (U64V y) = BoolV (x > y)
gt' (U32V x) (U32V y) = BoolV (x > y)
gt' (F64V x) (F64V y) = BoolV (x > y)
gt' (F32V x) (F32V y) = BoolV (x > y)
gt' _ _ = error "> expects number"

gte' :: Value -> Value -> Value
gte' (I64V x) (I64V y) = BoolV (x >= y)
gte' (I32V x) (I32V y) = BoolV (x >= y)
gte' (U64V x) (U64V y) = BoolV (x >= y)
gte' (U32V x) (U32V y) = BoolV (x >= y)
gte' (F64V x) (F64V y) = BoolV (x >= y)
gte' (F32V x) (F32V y) = BoolV (x >= y)
gte' _ _ = error ">= expects number"

lt' :: Value -> Value -> Value
lt' (I64V x) (I64V y) = BoolV (x < y)
lt' (I32V x) (I32V y) = BoolV (x < y)
lt' (U64V x) (U64V y) = BoolV (x < y)
lt' (U32V x) (U32V y) = BoolV (x < y)
lt' (F64V x) (F64V y) = BoolV (x < y)
lt' (F32V x) (F32V y) = BoolV (x < y)
lt' _ _ = error "< expects number"

lte' :: Value -> Value -> Value
lte' (I64V x) (I64V y) = BoolV (x <= y)
lte' (I32V x) (I32V y) = BoolV (x <= y)
lte' (U64V x) (U64V y) = BoolV (x <= y)
lte' (U32V x) (U32V y) = BoolV (x <= y)
lte' (F64V x) (F64V y) = BoolV (x <= y)
lte' (F32V x) (F32V y) = BoolV (x <= y)
lte' _ _ = error "<= expects number"

eq' :: Value -> Value -> Value
eq' (I64V x) (I64V y) = BoolV (x == y)
eq' (I32V x) (I32V y) = BoolV (x == y)
eq' (U64V x) (U64V y) = BoolV (x == y)
eq' (U32V x) (U32V y) = BoolV (x == y)
eq' (F64V x) (F64V y) = BoolV (x == y)
eq' (F32V x) (F32V y) = BoolV (x == y)
eq' (BoolV x) (BoolV y) = BoolV (x == y)
eq' (StrV x) (StrV y) = BoolV (x == y)
eq' _ _ = error "= expects number, string or boolean"

neq' :: Value -> Value -> Value
neq' (I64V x) (I64V y) = BoolV (x /= y)
neq' (I32V x) (I32V y) = BoolV (x /= y)
neq' (U64V x) (U64V y) = BoolV (x /= y)
neq' (U32V x) (U32V y) = BoolV (x /= y)
neq' (F64V x) (F64V y) = BoolV (x /= y)
neq' (F32V x) (F32V y) = BoolV (x /= y)
neq' (BoolV x) (BoolV y) = BoolV (x /= y)
neq' (StrV x) (StrV y) = BoolV (x /= y)
neq' _ _ = error "!= expects number, string or boolean"
