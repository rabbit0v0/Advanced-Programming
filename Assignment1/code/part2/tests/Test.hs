-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone


--1.1
tests :: [(String, Bool)]
tests = [test1, test2, test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,test21,test22,test23,test24,test25] where
  --1.1
  test1 = ("test1",showExp(Div (Cst 0) (Cst 1))=="(0/1)" )                          
  test2 = ("test2",showExp(Add (Mul (Cst 2) (Cst 3)) (Cst 4)) =="((2*3)+4)"  )       
  test3 = ("test3",showExp(Pow (Div (Cst 2) (Cst 3)) (Sub (Cst 4) (Cst 5)))=="((2/3)^(4-5))" )                          
  test4 = ("test4",showExp(Add (Sub (Cst 2) (Cst 3)) (Cst 4)) =="((2-3)+4)")                                         
  test5 = ("test5",showExp(Sub (Cst 2) (Add (Cst 3) (Cst 4))) =="(2-(3+4))")
  --1.2
  test6 = ("test6",evalSimple(Mul (Add (Cst 2) (Cst 3)) (Cst 4)) ==20)                            
  test7 = ("test7",evalSimple(Div (Mul (Cst 2) (Cst 3)) (Cst 4)) ==1)
  test8 = ("test8",evalSimple(Div (Cst 0) (Cst 1))==0 )                          
  test9 = ("test9",evalSimple(Pow (Cst 2) (Pow (Cst 3) (Cst 4)))==2417851639229258349412352 )                               
  test10 = ("test10",evalSimple(Pow (Pow (Cst 2) (Cst 3)) (Cst 4))==4096 )
  --2.1
  test11 = ("test11",(extendEnv "x" 5 initEnv) "x"==Just 5 )
  test12 = ("test12",(extendEnv "x" 5 initEnv) "y"==Nothing )
  test13 = ("test13",(extendEnv "x" 5 (extendEnv "y" 6 initEnv) "x")==Just 5 )
  test14 = ("test14",(extendEnv "x" 5 (extendEnv "y" 6 initEnv) "y")==Just 6 )
  test15 = ("test15",(extendEnv "x" 5 (extendEnv "x" 6 initEnv) "x")==Just 5 ) 
  --2.2
  
  test16 = ("test16",evalFull (Div (Cst 4) (Cst 1)) initEnv==4 )
  test17 = ("test17",evalFull(Pow (Div (Cst 4) (Cst 0)) (Cst 0))initEnv== 1 )
  test18 = ("test18",evalFull(Mul (Cst 1) (Div (Cst 0) (Cst 1)))initEnv== 0 )
  test19 = ("test19",evalFull(If (Sub (Cst 3) (Cst 3)) (Cst 4) (Cst 5))initEnv== 5)
  test20 =  ("test20",evalFull(Var "x")(extendEnv "x" 5 initEnv)==5)

  --3.1 

  test21 = ("test21",evalErr (Sum "x" (Sub (Cst 3) (Cst 2)) (Add (Cst 3) (Cst 2)) (Var "x")) initEnv==Right 15 )
  test22 = ("test22",evalErr ( Let "x" (Let "y" (Cst 3) (Sub (Var "x") (Var "y"))) (Mul (Var "x") (Var "y")))initEnv==Left (EBadVar "x") )
  test23 = ("test23",evalErr ( Let "z" (Add (Cst 2) (Cst 3)) (Var "z"))initEnv== Right 5)
  test24 = ("test24",evalErr (Let "x" (Add (Cst 3) (Var "y")) (Var "y"))initEnv==Left (EBadVar "y") )
  test25 = ("test25",evalErr (Let "x" (Add (Cst 3) (Var "y")) (Let "y" (Mul (Var "x") (Cst 2)) (Var "x")))initEnv==Left (EBadVar "y") )



main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
