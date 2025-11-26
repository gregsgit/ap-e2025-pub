module APL.Eval
  ( eval,
    Val (..),
    Env, envEmpty, envExtend, envLookup
  )
where

import APL.AST (Exp(..))

type Error = String

type VName = String

type Env = [(VName, Val)]
envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend var val env = (var, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup = lookup

data Val
  = ValInt Integer
  | ValBool Bool
  deriving (Eq, Show)

evalIntBinOp :: Env -> (Integer -> Integer -> Either Error Integer) -> Exp -> Exp -> Either Error Val
evalIntBinOp env op e1 e2 =
  case (eval e1 env, eval e2 env) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt i1), Right (ValInt i2)) -> case (i1 `op` i2) of
      (Left err) -> Left err
      (Right v) -> Right (ValInt v)
    _ -> Left "at least one operand to int binop did not evaluate to an integer"

eval :: Exp -> Env -> Either Error Val
eval (CstInt i) _ = Right $ ValInt i
eval (CstBool b) _ = Right $ ValBool b
eval (Add e1 e2) env = evalIntBinOp env (\ i1 i2 -> Right $ i1 + i2) e1 e2
eval (Sub e1 e2) env = evalIntBinOp env (\ i1 i2 -> Right $ i1 - i2) e1 e2
eval (Mul e1 e2) env = evalIntBinOp env (\ i1 i2 -> Right $ i1 * i2) e1 e2
eval (Div e1 e2) env = evalIntBinOp env checkedDiv e1 e2
        where checkedDiv i1 i2
                | i2 == 0 = Left "division by 0"
                | otherwise = Right $ i1 `div` i2
eval (Pow e1 e2) env = evalIntBinOp env checkedPow e1 e2
  where checkedPow i1 i2
          | i2 < 0 = Left "cannot raise to negative power"
          | otherwise = Right $ i1 ^ i2
eval (Eql e1 e2) env = case (eval e1 env, eval e2 env) of
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  (Right (ValBool b1), Right (ValBool b2)) -> Right $ ValBool $ (b1 == b2)
  (Right (ValInt i1), Right (ValInt i2)) -> Right $ ValBool $ (i1 == i2)
  _ -> Left "operands to eql do not match types"

eval (If e1 e2 e3) env = case (eval e1 env) of
  (Right (ValBool b)) -> if b then (eval e2 env) else (eval e3 env)
  (Left err) -> Left err
  _ -> Left "if predicate did not evaluate to a boolean"

eval (Var vname) env = case (envLookup vname env) of
  Nothing -> Left $ "variable " ++ vname ++ " not found in environment"
  Just val -> Right val
  
eval (Let vname val_e in_e) env = case (eval val_e env) of
  (Left err) -> Left err
  (Right var_val) -> case (eval in_e (envExtend vname var_val env)) of
    (Left err) -> Left err
    (Right val) -> Right val
