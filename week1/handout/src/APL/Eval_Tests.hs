module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), Env, eval, envEmpty, envExtend, envLookup)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

testEvalCstInt :: TestTree
testEvalCstInt = testCase "CstInt 3 => ValInt 3" $ (eval (CstInt 3) envEmpty) @?= (Right $ ValInt 3)
testEvalAdd :: TestTree
testEvalAdd = testCase "add 1, 2 = 3" $ (eval (Add (CstInt 1) (CstInt 2)) envEmpty) @?= (Right $ ValInt 3)
testEvalSub :: TestTree
testEvalSub = testCase "sub 1, 2 = -1" $ (eval (Sub (CstInt 1) (CstInt 2)) envEmpty) @?= (Right $ ValInt (-1))
testEvalMul :: TestTree
testEvalMul = testCase "mul 1, 2 = 2" $ (eval (Mul (CstInt 1) (CstInt 2)) envEmpty) @?= (Right $ ValInt 2)
testEvalDiv :: TestTree
testEvalDiv = testCase "div 1, 2 = 0" $ (eval (Div (CstInt 1) (CstInt 2)) envEmpty) @?= (Right $ ValInt 0)
testEvalPow :: TestTree
testEvalPow = testCase "pow 1, 2 = 1" $ (eval (Pow (CstInt 1) (CstInt 2)) envEmpty) @?= (Right $ ValInt 1)
testEvalDivZero :: TestTree
testEvalDivZero = testCase "div 1 0 => error" $ (eval (Div (CstInt 1) (CstInt 0)) envEmpty) @?= (Left "division by 0")
testEvalPowNeg :: TestTree
testEvalPowNeg = testCase "pow 1, -2 => error" $
                 (eval (Pow (CstInt 1) (CstInt (-2))) envEmpty) @?= (Left $ "cannot raise to negative power")
testEvalCstBool :: TestTree
testEvalCstBool = testCase "CstBool True => ValBool True" $ (eval (CstBool True) envEmpty) @?= (Right $ ValBool True)
testEvalEql :: TestTree
testEvalEql = testCase "Eql (CstInt 1) (Sub (CstInt 2) (CstInt 1)) == (ValBool True)" $
              (eval (Eql (CstInt 1) (Sub (CstInt 2) (CstInt 1))) envEmpty) @?= (Right $ ValBool True)
testEvalEqlNeg1 :: TestTree
testEvalEqlNeg1 = testCase "Eql (CstInt 1) (Sub (CstBool True) (CstInt 1)) == error" $
              (eval (Eql (CstInt 1) (Sub (CstBool True) (CstInt 1))) envEmpty) @?= 
              (Left "at least one operand to int binop did not evaluate to an integer")
testEvalEqlNeg2 :: TestTree
testEvalEqlNeg2 = testCase "Eql (CstInt 1) (CstBool True) == error" $
              (eval (Eql (CstInt 1) (CstBool True)) envEmpty) @?= 
              (Left "operands to eql do not match types")
testLetVar :: TestTree
testLetVar = testCase "Let x (Add 1 2) (Add x 3) == 6" $
             (eval (Let "x" (Add (CstInt 1) (CstInt 2)) (Add (Var "x") (CstInt 3))) envEmpty) @?=
             (Right $ ValInt 6)
testLetVarErr :: TestTree
testLetVarErr = testCase "Let x (Add 1 2) (Add y 3) => error" $
             (eval (Let "x" (Add (CstInt 1) (CstInt 2)) (Add (Var "y") (CstInt 3))) envEmpty) @?=
             (Left "variable y not found in environment")

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testEvalCstInt,
      testEvalAdd,
      testEvalSub,
      testEvalMul,
      testEvalDiv,
      testEvalPow,
      testEvalDivZero,
      testEvalPowNeg,
      testEvalCstBool,
      testEvalEql,
      testEvalEqlNeg1,
      testEvalEqlNeg2,
      testLetVar,
      testLetVarErr
    ]
