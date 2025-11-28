module APL.AST_Tests (tests) where

import APL.AST (Exp (..), printExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [ testCase "pretty CstInt" $ printExp (CstInt 2)
      @?= "2",
      --
      testCase "pretty true" $ printExp (CstBool True)
      @?= "true",
      --
      testCase "pretty false" $ printExp (CstBool False)
      @?= "false",
      --
      testCase "pretty add" $ printExp (Add (CstInt 2) (Add (CstInt 3) (CstInt 4)))
      @?= "(2) + ((3) + (4))",
      --
      testCase "pretty sub" $ printExp (Sub (CstInt 2) (Add (CstInt 3) (CstInt 4)))
      @?= "(2) - ((3) + (4))",
      --
      testCase "pretty mul" $ printExp (Mul (CstInt 2) (Add (CstInt 3) (CstInt 4)))
      @?= "(2) * ((3) + (4))",
      --
      testCase "pretty div" $ printExp (Div (CstInt 2) (Add (CstInt 3) (CstInt 4)))
      @?= "(2) / ((3) + (4))",
      --
      testCase "pretty pow" $ printExp (Pow (CstInt 2) (Add (CstInt 3) (CstInt 4)))
      @?= "(2) ** ((3) + (4))",
      --
      testCase "pretty eql" $ printExp (Eql (CstInt 2) (Add (CstInt 1) (CstInt 2)))
      @?= "(2) == ((1) + (2))",
      --
      testCase "pretty if" $ printExp (If (Eql (CstInt 2) (Add (CstInt 1) (CstInt 2)))
                                       (CstInt 1) (CstInt 0))
      @?= "if ((2) == ((1) + (2))) then (1) else (0)",
      --
      testCase "pretty var" $ printExp (Var "x") @?= "x",
      --
      testCase "pretty let" $ printExp (Let "x" (CstInt 1) (Add (Var "x") (CstInt 2)))
      @?= "let x = (1) in ((x) + (2))",
      --
      testCase "pretty for loop" $ printExp (ForLoop ("p", (CstInt 0)) ("i", (CstInt 10))
                                             (Add (Var "p") (Var "i")))
      @?= "loop p = (0) for i < (10) do ((p) + (i))",
      --
      testCase "pretty lambda" $ printExp (Lambda "x" (Add (CstInt 1) (Var "x")))
      @?= "\\x -> ((1) + (x))",
      --
      testCase "pretty apply" $ printExp (Apply (Lambda "x" (Add (CstInt 1) (Var "x"))) (CstInt 3))
      @?= "(\\x -> ((1) + (x))) (3)",
      --
      testCase "pretty try catch" $ printExp (TryCatch (Var "missing") ( CstInt 1))
      @?= "try (missing) catch (1)"
    ]
