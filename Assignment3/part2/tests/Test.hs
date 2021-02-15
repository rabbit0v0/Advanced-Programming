-- Rudimentary test suite. Feel free to replace anything.

import BoaAST
import BoaParser

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests = testGroup "tests" [
  testCase "simple success" $
    parseString "2 + two" @?=
      Right [SExp (Oper Plus (Const (IntVal 2)) (Var "two"))],

  testCase "Oper + * %" $
    parseString "1 + 2 * 3 % 4" @?=
      Right [SExp (Oper Plus (Const (IntVal 1)) (Oper Mod (Oper Times (Const (IntVal 2)) (Const (IntVal 3))) (Const (IntVal 4))))],

  testCase "Oper - // ()" $
    parseString "1 - 2 // (1+1)" @?=
      Right [SExp (Oper Minus (Const (IntVal 1)) (Oper Div (Const (IntVal 2)) (Oper Plus (Const (IntVal 1)) (Const (IntVal 1)))))],

  testCase "Oper not in, []" $
    parseString "a not in [1, 2]" @?=
      Right [SExp (Not (Oper In (Var "a") (List [Const (IntVal 1),Const (IntVal 2)])))],

  testCase "Oper >=" $
    parseString "a >= 6" @?=
      Right [SExp (Not (Oper Less (Var "a") (Const (IntVal 6))))],

  testCase "Oper continues >= ==" $
    case parseString "a >= 6 == 6" of
    	Left e -> return ()
    	Right p -> assertFailure $ "Unexpected parse: " ++ show p,

  testCase "Exp Not" $
    parseString "not a" @?=
      Right [SExp (Not (Var "a"))],

  testCase "Exp no space Not -> Var" $
  	parseString "nota" @?=
      Right [SExp (Var "nota")],

  testCase "Num, Stmts ;" $
  	parseString "-1234; 1" @?=
      Right [SExp (Const (IntVal (-1234))),SExp (Const (IntVal 1))],

  testCase "String" $
  	parseString "'\\'asf124\\\n\\n  f'" @?=
      Right [SExp (Const (StringVal "'asf124\n  f"))],

  testCase "String failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "'\'123" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p,

  testCase "Exp indent ()" $
  	parseString "print (123)" @?=
      Right [SExp (Call "print" [Const (IntVal 123)])],

  testCase "CClause" $
  	parseString "[a for k in 1 if c]" @?=
      Right [SExp (Compr (Var "a") [CCFor "k" (Const (IntVal 1)),CCIf (Var "c")])],

  testCase "Stmt indent" $
  	parseString "a = 1" @?=
      Right [SDef "a" (Const (IntVal 1))],

  testCase "simple failure" $
    -- avoid "expecting" very specific parse-error messages
    case parseString "wow!" of
      Left e -> return ()  -- any message is OK
      Right p -> assertFailure $ "Unexpected parse: " ++ show p]
