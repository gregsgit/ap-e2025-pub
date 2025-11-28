module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | ForLoop (VName , Exp) (VName , Exp) Exp
  | Lambda VName Exp
  | Apply Exp Exp
  | TryCatch Exp Exp
  deriving (Eq, Show)

printBinOpExp :: String -> Exp -> Exp -> String
printBinOpExp opSym e1 e2 = "(" ++ printExp e1 ++ ") " ++ opSym ++ " (" ++ printExp e2 ++ ")"

printExp :: Exp -> String
printExp (CstInt n) = show n
printExp (CstBool True) = "true"
printExp (CstBool False) = "false"
printExp (Add e1 e2) = printBinOpExp "+" e1 e2
printExp (Sub e1 e2) = printBinOpExp "-" e1 e2
printExp (Mul e1 e2) = printBinOpExp "*" e1 e2
printExp (Div e1 e2) = printBinOpExp "/" e1 e2
printExp (Pow e1 e2) = printBinOpExp "**" e1 e2
printExp (Eql e1 e2) = "(" ++ printExp e1 ++ ") == (" ++ printExp e2 ++ ")"
printExp (If test_e then_e else_e) = "if (" ++ printExp test_e ++ ") then (" ++ printExp then_e
  ++ ") else (" ++ printExp else_e ++ ")"
printExp (Var v) = v
printExp (Let var valExp bodyExp) = "let " ++ var ++ " = (" ++ printExp valExp ++ ") in ("
  ++ printExp bodyExp ++ ")"
printExp (ForLoop (outVar, outInitExp) (boundVar, boundExp) bodyExp) =
          "loop " ++ outVar ++ " = (" ++ printExp outInitExp ++ ") for " ++ boundVar ++ " < ("
          ++ printExp boundExp ++ ") do (" ++ printExp bodyExp ++ ")"
printExp (Lambda argVar bodyExp) = "\\" ++ argVar ++ " -> (" ++ printExp bodyExp ++ ")"
printExp (Apply funExp valExp) = "(" ++ printExp funExp ++ ") (" ++ printExp valExp ++ ")"
printExp (TryCatch tryExp catchExp) = "try (" ++ printExp tryExp ++ ") catch ("
                                      ++ printExp catchExp ++ ")"
