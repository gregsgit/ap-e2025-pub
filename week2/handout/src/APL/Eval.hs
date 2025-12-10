module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

askEnv :: EvalM Env
askEnv = EvalM (\ env -> Right env)

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM f') = EvalM $ (\ env -> f' (f env))

type Error = String

newtype EvalM a = EvalM (Env -> Either Error a)

catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \ env ->
                                        case (m1 env) of
                                          Left _err -> (m2 env)
                                          Right v -> Right v

-- (<$>) :: Functor f => (a -> b) -> f a -> f b 
-- ap :: Monad m => m (a -> b) -> m a -> m b
-- Monad
-- (>>=) :: m a -> (a -> m b) -> m b
instance Monad EvalM where
  EvalM m >>= f = EvalM $ \ env ->
                            case (m env) of
                              Left err -> Left err
                              Right x ->
                                let EvalM f' = f x
                                in f' env

-- Applicative
-- pure :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
instance Applicative EvalM where
  pure x = EvalM (\ _env -> (Right x))
  (EvalM f) <*> (EvalM x) = EvalM $ \ env ->
                                      case (f env) of
                                        Left err -> Left err
                                        Right f' -> case (x env) of
                                          Left err -> Left err
                                          Right x' -> Right (f' x')

-- Functor
-- fmap :: (a -> b) -> f a -> f b
instance Functor EvalM where
  fmap f (EvalM x) = EvalM $ \ env ->
                               case (x env) of
                                 Left err -> Left err
                                 Right x' -> Right (f x')

runEval :: EvalM a -> Either Error a
runEval (EvalM x) = x envEmpty

failure :: String -> EvalM a
failure s = EvalM (\ _env -> (Left s))

evalIntBinop :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinop op e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt n1, ValInt n2) -> ValInt <$> op n1 n2
    (_, _) -> failure "Non integer operand(s)"

errPlus :: Integer -> Integer -> EvalM Integer
errPlus x y = pure $ x + y
errSub :: Integer -> Integer -> EvalM Integer
errSub x y = pure $ x - y
errMul :: Integer -> Integer -> EvalM Integer
errMul x y = pure $ x * y
errDiv :: Integer -> Integer -> EvalM Integer
errDiv x y = if (y == 0) then failure "division by 0"
             else pure $ x `div` y
errPow :: Integer -> Integer -> EvalM Integer
errPow x y = if (y < 0) then failure "negative exponent"
             else pure $ x ^ y

eval :: Exp -> EvalM Val

eval (CstInt n) = pure (ValInt n)
eval (CstBool b) = pure (ValBool b)
eval (Add e1 e2) = evalIntBinop errPlus e1 e2
eval (Sub e1 e2) = evalIntBinop errSub e1 e2
eval (Mul e1 e2) = evalIntBinop errMul e1 e2
eval (Div e1 e2) = evalIntBinop errDiv e1 e2
eval (Pow e1 e2) = evalIntBinop errPow e1 e2

eval (Eql e1 e2) = do
  x1 <- eval e1
  x2 <- eval e2
  case (x1, x2) of
    (ValInt n1, ValInt n2) -> pure $ ValBool $ (n1 == n2)
    (ValBool n1, ValBool n2) -> pure $ ValBool $ (n1 == n2)
    (_, _) -> failure "Incompatible types for eql"

eval (If e1 e2 e3) = do
  v1 <- eval e1
  case v1 of
    (ValBool True) -> eval e2
    (ValBool False) -> eval e3
    _ -> failure "If predicate not a boolean"

eval (Var v) = do
  env <- askEnv
  case (envLookup v env) of
    Nothing -> failure "variable not found"
    Just x -> pure x

eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (\ env -> envExtend var v1 env) $ eval e2

eval (ForLoop (loopparam, initial) (iv, bound) body) = do
  paramInit <- eval initial
  boundVal <- eval bound
  case boundVal of
    (ValInt boundInt) -> loop 0 boundInt paramInit 
    _ -> failure "non-integer loop bound"
  where loop count boundInt param
          | count < boundInt = do
              param' <- (localEnv (\env -> (envExtend iv (ValInt count) (envExtend loopparam param env)))) $
                eval body
              loop (count + 1) boundInt param'
          | otherwise = pure param        

eval (Lambda vname body) = do
  env <- askEnv
  pure $ ValFun env vname body

eval (Apply e1 e2) = do
  f <- eval e1
  v <- eval e2
  case f of
    (ValFun env' vname body) -> localEnv (\ _env -> (envExtend vname v env')) $ eval body
    _ -> failure "Not a lambda in function position"
  
eval (TryCatch e1 e2) = (eval e1) `catch` (eval e2)
