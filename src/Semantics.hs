{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Semantics where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import Language

type Env = [(String, (Value, Type))]

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
    updateEnv :: String -> Value -> Type -> m ()
    lookupStack :: String -> m Procedure
    lookupEnv :: String -> m (Value, Type)

instance MonadStack SemanticsM where
    updateStack name proc = do
        p <- asks procs
        liftIO $ modifyIORef' p ((name, proc) :)
    updateEnv name val typ = do
        e <- asks env
        environment <- liftIO $ readIORef e
        case lookup name environment of
            Just _ -> throwError $ RedeclaredVariable name
            Nothing -> liftIO $ modifyIORef' e ((name, (val, typ)) :)
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
analyzeProcedure p@(Procedure name params Nothing stms) = do
    updateStack name p
    mapM_ (\(v, t) -> updateEnv v None t) params
    mapM_ analyzeStm stms
analyzeProcedure p@(Procedure name params (Just (VarStm v t)) stms) = do
    updateStack name p
    mapM_ (\(va, ty) -> updateEnv va None ty) params
    updateEnv v None t
    mapM_ analyzeStm stms
analyzeProcedure (Procedure _ _ (Just s) _) = throwError $ GeneralError $ "statement not permitted: " ++ show s
analyzeStm :: Stm -> SemanticsM ()
analyzeStm (ExprStm expr) = analyzeExpr expr
analyzeStm (Return expr) = analyzeExpr expr
analyzeStm (FunStm name _) = do
    _ <- lookupStack name
    pure ()
analyzeStm (VarStm name t) = do
    updateEnv name None t
    pure ()
analyzeStm (AssignStm _ stm) = do
    analyzeStm stm
analyzeStm (If e stm1 stm2) = analyzeExpr e >> analyzeStm stm1 >> analyzeStm stm2
analyzeStm (Do e stm1) = analyzeExpr e >> mapM_ analyzeStm stm1

-- pa despues
-- valueOf :: Expr -> Value
-- valueOf (I64 x) = (I64

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
    _ <- lookupEnv n
    pure ()
analyzeExpr (Fun n _) = do
    _ <- lookupStack n
    pure ()
analyzeExpr (BinE op e1 e2) = do
    if isVar e1 || isVar e2
        then analyzeVarType e1 e2
        else binaryAnalysis op e1 e2
  where
    isVar :: Expr -> Bool
    isVar (Var _) = True
    isVar _ = False

analyzeVarType :: Expr -> Expr -> SemanticsM ()
analyzeVarType (Var n) e = do
    (_, t) <- lookupEnv n
    compareTypes t e
analyzeVarType e (Var n) = do
    (_, t) <- lookupEnv n
    compareTypes t e

compareTypes :: Type -> Expr -> SemanticsM ()
compareTypes I64T (I64 _) = pure ()
compareTypes I32T (I32 _) = pure ()
compareTypes F64T (F64 _) = pure ()
compareTypes F32T (F32 _) = pure ()
compareTypes U64T (U64 _) = pure ()
compareTypes U64T (U32 _) = pure ()
compareTypes BoolT (Boolean _) = pure ()
compareTypes StrT (Str _) = pure ()
compareTypes t1 (Var n) = do
    (_, t2) <- lookupEnv n
    if t1 == t2 then pure () else throwError $ GeneralError "type mismatch"
compareTypes _ _ = throwError $ GeneralError "type mismatch"

binaryAnalysis :: BinOp -> (Expr -> Expr -> SemanticsM ())
binaryAnalysis Add = add'
binaryAnalysis Sub = sub'
binaryAnalysis Mul = mul'
binaryAnalysis Div = div'
binaryAnalysis Mod = mod''
binaryAnalysis And = and'
binaryAnalysis Or = or'
binaryAnalysis XOr = xor'
binaryAnalysis Gt = gt'
binaryAnalysis GtE = gte'
binaryAnalysis Lt = lt'
binaryAnalysis LtE = lte'
binaryAnalysis Equal = eq'
binaryAnalysis NEqual = neq'

add' :: Expr -> Expr -> SemanticsM ()
add' (I64 _) (I64 _) = pure ()
add' (I32 _) (I32 _) = pure ()
add' (U64 _) (U64 _) = pure ()
add' (U32 _) (U32 _) = pure ()
add' (F64 _) (F64 _) = pure ()
add' (F32 _) (F32 _) = pure ()
add' _ _ = throwError $ GeneralError "+ expects number"

sub' :: Expr -> Expr -> SemanticsM ()
sub' (I64 _) (I64 _) = pure ()
sub' (I32 _) (I32 _) = pure ()
sub' (U64 _) (U64 _) = pure ()
sub' (U32 _) (U32 _) = pure ()
sub' (F64 _) (F64 _) = pure ()
sub' (F32 _) (F32 _) = pure ()
sub' _ _ = throwError $ GeneralError "- expects number"

mul' :: Expr -> Expr -> SemanticsM ()
mul' (I64 _) (I64 _) = pure ()
mul' (I32 _) (I32 _) = pure ()
mul' (U64 _) (U64 _) = pure ()
mul' (U32 _) (U32 _) = pure ()
mul' (F64 _) (F64 _) = pure ()
mul' (F32 _) (F32 _) = pure ()
mul' _ _ = throwError $ GeneralError "* expects number"

div' :: Expr -> Expr -> SemanticsM ()
div' (I64 _) (I64 _) = pure ()
div' (I32 _) (I32 _) = pure ()
div' (U64 _) (U64 _) = pure ()
div' (U32 _) (U32 _) = pure ()
div' (F64 _) (F64 _) = pure ()
div' (F32 _) (F32 _) = pure ()
div' _ _ = throwError $ GeneralError "/ expects number"

mod'' :: Expr -> Expr -> SemanticsM ()
mod'' (I64 _) (I64 _) = pure ()
mod'' (I32 _) (I32 _) = pure ()
mod'' (U64 _) (U64 _) = pure ()
mod'' (U32 _) (U32 _) = pure ()
mod'' (F64 _) (F64 _) = pure ()
mod'' (F32 _) (F32 _) = pure ()
mod'' _ _ = throwError $ GeneralError "% expects number"

and' :: Expr -> Expr -> SemanticsM ()
and' (Boolean _) (Boolean _) = pure ()
and' _ _ = throwError $ GeneralError "/\\ expects boolean"

or' :: Expr -> Expr -> SemanticsM ()
or' (Boolean _) (Boolean _) = pure ()
or' _ _ = throwError $ GeneralError "\\/ expects boolean"

xor' :: Expr -> Expr -> SemanticsM ()
xor' (Boolean _) (Boolean _) = pure ()
xor' _ _ = throwError $ GeneralError "^ expects boolean"

gt' :: Expr -> Expr -> SemanticsM ()
gt' (I64 _) (I64 _) = pure ()
gt' (I32 _) (I32 _) = pure ()
gt' (U64 _) (U64 _) = pure ()
gt' (U32 _) (U32 _) = pure ()
gt' (F64 _) (F64 _) = pure ()
gt' (F32 _) (F32 _) = pure ()
gt' _ _ = throwError $ GeneralError "> expects number"

gte' :: Expr -> Expr -> SemanticsM ()
gte' (I64 _) (I64 _) = pure ()
gte' (I32 _) (I32 _) = pure ()
gte' (U64 _) (U64 _) = pure ()
gte' (U32 _) (U32 _) = pure ()
gte' (F64 _) (F64 _) = pure ()
gte' (F32 _) (F32 _) = pure ()
gte' _ _ = throwError $ GeneralError ">= expects number"

lt' :: Expr -> Expr -> SemanticsM ()
lt' (I64 _) (I64 _) = pure ()
lt' (I32 _) (I32 _) = pure ()
lt' (U64 _) (U64 _) = pure ()
lt' (U32 _) (U32 _) = pure ()
lt' (F64 _) (F64 _) = pure ()
lt' (F32 _) (F32 _) = pure ()
lt' _ _ = throwError $ GeneralError "< expects number"

lte' :: Expr -> Expr -> SemanticsM ()
lte' (I64 _) (I64 _) = pure ()
lte' (I32 _) (I32 _) = pure ()
lte' (U64 _) (U64 _) = pure ()
lte' (U32 _) (U32 _) = pure ()
lte' (F64 _) (F64 _) = pure ()
lte' (F32 _) (F32 _) = pure ()
lte' _ _ = throwError $ GeneralError "<= expects number"

eq' :: Expr -> Expr -> SemanticsM ()
eq' (I64 _) (I64 _) = pure ()
eq' (I32 _) (I32 _) = pure ()
eq' (U64 _) (U64 _) = pure ()
eq' (U32 _) (U32 _) = pure ()
eq' (F64 _) (F64 _) = pure ()
eq' (F32 _) (F32 _) = pure ()
eq' (Boolean _) (Boolean _) = pure ()
eq' (Str _) (Str _) = pure ()
eq' _ _ = throwError $ GeneralError "= expects number"

neq' :: Expr -> Expr -> SemanticsM ()
neq' (I64 _) (I64 _) = pure ()
neq' (I32 _) (I32 _) = pure ()
neq' (U64 _) (U64 _) = pure ()
neq' (U32 _) (U32 _) = pure ()
neq' (F64 _) (F64 _) = pure ()
neq' (F32 _) (F32 _) = pure ()
neq' (Boolean _) (Boolean _) = pure ()
neq' (Str _) (Str _) = pure ()
neq' _ _ = throwError $ GeneralError "!= types do not match"
