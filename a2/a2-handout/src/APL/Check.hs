module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

type Error = String

-- stack of vars in scope. head is most nested
type Vars = [VName]
emptyVars :: Vars
emptyVars = []

newtype CheckM a = CheckM (Vars -> (Either Error a))

getVars :: CheckM Vars
getVars = CheckM $ \ vars -> Right vars

bindVar :: VName -> CheckM a -> CheckM a
bindVar var (CheckM m) = CheckM $ \ vars -> m (var : vars) 

failure :: String -> CheckM ()
failure err = CheckM $ \ _vars -> Left err

instance Functor CheckM where
  fmap = liftM

instance Applicative CheckM where
  pure x = CheckM $ \ _vars -> Right x
  (<*>) = ap

instance Monad CheckM where
  -- (>>=) :: m a -> (a -> m b) -> m b
  CheckM x >>= f = CheckM $ \ vars ->
                              case (x vars) of
                                Left err -> Left err
                                Right y ->
                                  let CheckM z = f y
                                  in z vars
                                  
checkExp :: Exp -> Maybe Error
checkExp e = 
  let CheckM x = check e
  in 
    case (x emptyVars) of
      Left err -> Just err
      Right _x -> Nothing

check :: Exp -> CheckM ()
check (CstInt _x) = pure ()
check (CstBool _b) = pure ()
check (Var v) = do
  vars <- getVars
  case (elem v vars) of
    False -> failure $ "Variable not in scope: " ++ v
    True -> pure ()
check (Add e1 e2) = do
  _v1 <- check e1
  _v2 <- check e2
  pure ()
check (Sub e1 e2) = do
  _v1 <- check e1
  _v2 <- check e2
  pure ()
check (Mul e1 e2) = do
  _v1 <- check e1
  _v2 <- check e2
  pure ()
check (Div e1 e2) = do
  _v1 <- check e1
  _v2 <- check e2
  pure ()
check (Pow e1 e2) = do
  _v1 <- check e1
  _v2 <- check e2
  pure ()
check (Eql e1 e2) = do
  _v1 <- check e1
  _v2 <- check e2
  pure ()
check (If cond e1 e2) = do
  _vcond <- check cond
  _v1 <- check e1
  _v2 <- check e2
  pure ()
check (Let var e1 e2) = do
  _v1 <- check e1
  _v2 <- bindVar var (check e2)
  pure ()
check (ForLoop (loopparam, initial) (iv, bound) body) = do
  _initVal <- check initial
  _boundVal <- check bound
  _bodyVal <- bindVar loopparam (bindVar iv (check body))
  pure ()
check (Lambda var body) = do
  _bodyVal <- bindVar var (check body)
  pure ()
check (Apply e1 e2) = do
  _v1 <- check e1
  _v2 <- check e2
  pure ()
check (TryCatch e1 e2) = do
  _v1 <- check e1
  _v2 <- check e2
  pure ()
check (Print _str e) = do
  _v1 <- check e
  pure ()
check (KvPut keyExp valExp) = do
  _v1 <- check keyExp
  _v2 <- check valExp
  pure ()
check (KvGet keyExp) = do
  _v1 <- check keyExp
  pure ()
