-- Skeleton test suite using Tasty.
-- Fell free to modify or replace anything in this file

import BoaAST
import BoaInterp

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "tests"
  [
   testCase "Def & Const test" $
    execute [SDef "x" (Const (IntVal 1)), SExp (Call "print" [(Var "x")])]
      @?= (["1"], Nothing),

   testCase "Def & Not test" $
    execute [SDef "x" (Not (Const (IntVal 1))), SExp (Call "print" [(Var "x")])]
      @?= (["False"], Nothing),

   testCase "badVar & Oper Plus test" $
    execute [SExp (Call "print" [Oper Plus (Const (IntVal 2))
                                           (Const (IntVal 2))]),
             SExp (Var "hello")]
      @?= (["4"], Just (EBadVar "hello")),
   testCase "Oper Div test" $
    execute [SExp (Call "print" [Oper Div (Const (IntVal 6)) (Const (IntVal 2))]),
    SExp (Call "print" [Oper Div (Const (IntVal 2)) (Const (IntVal 0))])]
      @?= (["3"], Just (EBadArg "Error: Div by 0.")),
   testCase "Oper Mod test" $
    execute [SExp (Call "print" [Oper Mod (Const (IntVal 3)) (Const (IntVal 2))]),
    SExp (Call "print" [Oper Mod (Const (IntVal 2)) (Const (IntVal 0))])]
      @?= (["1"], Just (EBadArg "Error: Mod by 0.")),
   testCase "Oper Value test" $
    execute [SExp (Call "print" [Oper Mod (Const TrueVal) (Const (IntVal 1))])]
      @?= ([], Just (EBadArg "Error: operate with invalid data type.")),
   testCase "Oper Minus test" $ 
    execute [SExp (Call "print" [Oper Minus (Const (IntVal 3)) (Const (IntVal 1))])]
      @?= (["2"], Nothing),
   testCase "Oper Times test" $ 
    execute [SExp (Call "print" [Oper Times (Const (IntVal 3)) (Const (IntVal 2))])]
      @?= (["6"], Nothing),
   testCase "Oper Eq test" $ 
    execute [SExp (Call "print" [Oper Eq (Not (Const (IntVal 3))) (Const FalseVal),
     Oper Eq (Const (IntVal 3)) (Const FalseVal)])]
      @?= (["True False"], Nothing),
   testCase "Oper Less Greater test" $
    execute [SExp (Call "print" [Oper Greater (Const (IntVal 3)) (Const (IntVal 5)),
     Oper Less (Const (IntVal 3)) (Const (IntVal 5))]),
     SExp (Call "print" [ Oper Less (Const (IntVal 0)) (Const TrueVal)])]
      @?= (["False True"], Just (EBadArg "Error: operate with invalid data type.")),
   testCase "Oper In test" $
    execute [SExp (Call "print" [Oper In (Const (IntVal 3)) (Const (ListVal [IntVal 1, IntVal 3]))])]
      @?= (["True"], Nothing),
   testCase "badFunc test" $
    execute [SExp (Call "a" [(Const TrueVal)])]
      @?= ([], Just (EBadFun "a")),
   testCase "range test" $
    execute [SDef "x1" (Call "range" [(Const (IntVal 3))]),
    SDef "x2" (Call "range" [(Const (IntVal 1)), (Const (IntVal 0))]),
    SDef "x3" (Call "range" [(Const (IntVal 5)), (Const (IntVal 1)), (Const (IntVal (-2)))]),
    SExp (Call "print" [Var "x1", Var "x2", Var "x3"])]
      @?= (["[0, 1, 2] [] [5, 3]"], Nothing),
   testCase "range value test" $
    execute [SDef "x1" (Call "range" [(Const TrueVal)])]
      @?= ([], Just (EBadArg "Error: Range function can only use IntVal.")),
   testCase "range 0 test" $
    execute [SDef "x3" (Call "range" [(Const (IntVal 5)), (Const (IntVal 1)), (Const (IntVal 0))])]
      @?= ([], Just (EBadArg "Error :Range function will not terminate for x3 == 0.")),
   testCase "List test" $
    execute [SDef "x" (List [Oper Plus (Const (IntVal 1)) (Const (IntVal 4)), Const FalseVal]),
    SExp (Call "print" [Var "x"])]
      @?= (["[5, False]"], Nothing)
   ]
