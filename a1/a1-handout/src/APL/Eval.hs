module APL.Eval
  ( Val (..),
    Env,
    envEmpty,
    eval,
  )
where

import APL.AST (Exp (..), VName)

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

type Error = String

evalIntBinOp :: (Integer -> Integer -> Either Error Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp f env e1 e2 =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> case f x y of
      Left err -> Left err
      Right z -> Right $ ValInt z
    (Right _, Right _) -> Left "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp' f env e1 e2 =
  evalIntBinOp f' env e1 e2
  where
    f' x y = Right $ f x y

eval :: Env -> Exp -> Either Error Val
eval _env (CstInt x) = Right $ ValInt x
eval _env (CstBool b) = Right $ ValBool b
eval env (Var v) = case envLookup v env of
  Just x -> Right x
  Nothing -> Left $ "Unknown variable: " ++ v
eval env (Add e1 e2) = evalIntBinOp' (+) env e1 e2
eval env (Sub e1 e2) = evalIntBinOp' (-) env e1 e2
eval env (Mul e1 e2) = evalIntBinOp' (*) env e1 e2
eval env (Div e1 e2) = evalIntBinOp checkedDiv env e1 e2
  where
    checkedDiv _ 0 = Left "Division by zero"
    checkedDiv x y = Right $ x `div` y
eval env (Pow e1 e2) = evalIntBinOp checkedPow env e1 e2
  where
    checkedPow x y =
      if y < 0
        then Left "Negative exponent"
        else Right $ x ^ y
eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Invalid operands to equality"
eval env (If cond e1 e2) =
  case eval env cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "Non-boolean conditional."
eval env (Let var e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right v -> eval (envExtend var v env) e2

eval env0 (ForLoop (outVar, outVarInitExp) (counterVar, boundExp) bodyExp) =
  case (eval env0 outVarInitExp, eval env0 boundExp) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right v, Right (ValInt n)) ->
      loop (envExtend counterVar (ValInt 0) (envExtend outVar v env0)) n
    (Right _, Right _notAnInt) -> Left "Non-integral loop bound"
  where
    loop env n =
      case (envLookup counterVar env) of
        Nothing-> Left "bug: env error counterVar"
        Just (ValInt i) ->
          if (i >= n) then case (envLookup outVar env) of
            Nothing -> Left "bug: env error outVar"
            Just v -> Right v
          else
            case (eval env bodyExp) of
              Left err -> Left err
              Right p' -> loop (envExtend "p" p' (envExtend "i" (ValInt (i + 1)) env)) n
        Just _notAnInt -> Left "bug: counterVar not an int"

eval env (Lambda var bodyExp) = Right $ ValFun env var bodyExp

eval env (Apply funExp valExp) =
  case (eval env funExp) of
    Left err -> Left err
    Right (ValFun funEnv argVar bodyExp) ->
      case (eval env valExp) of
        Left err -> Left err
        Right argVal ->
          eval (envExtend argVar argVal funEnv) bodyExp
    Right _ -> Left "First argument to an apply must be a lambda"

eval env (TryCatch mainExp catchExp) =
  case (eval env mainExp) of
    Right v -> Right v
    Left _ -> eval env catchExp
