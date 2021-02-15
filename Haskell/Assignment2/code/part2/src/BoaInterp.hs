-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\env -> (Right a, []))
  x >>= f = Comp (\env -> case runComp x env of 
    (Left a, s) -> (Left a, s)
    (Right a, s) -> case runComp (f a) env of
      (Left x, s') -> (Left x, s<>s')
      (Right x, s') -> (Right x, s<>s'))


-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return; (<*>) = ap

-- function added
findList :: Env -> VName -> Maybe Value
findList [] v = Nothing
findList e v = 
  if fst (head e) == v
    then Just (snd (head e))
  else
    findList (tail e) v 

-- Operations of the monad
abort :: RunError -> Comp a
abort (EBadVar v) = Comp (\env -> (Left (EBadVar v), []))
abort (EBadFun f) = Comp (\env -> (Left (EBadFun f), []))
abort (EBadArg s) = Comp (\env -> (Left (EBadArg s), []))

look :: VName -> Comp Value
look v = Comp (\env -> case findList env v of 
                    Nothing -> (Left (EBadVar v), [])
                    Just x -> (Right x, []))

withBinding :: VName -> Value -> Comp a -> Comp a
withBinding v x m = Comp (\env -> runComp m ((v, x):env))

output :: String -> Comp ()
output s = Comp(\env -> (Right (), [s]))

-- Helper functions for interpreter
truthy :: Value -> Bool
truthy x
  | x == NoneVal = False
  | x == FalseVal = False
  | x == IntVal 0 = False
  | x == StringVal "" = False
  | x == ListVal [] = False
  | otherwise = True


operate :: Op -> Value -> Value -> Either String Value
operate Plus (IntVal v1) (IntVal v2) = Right (IntVal (v1 + v2))
operate Minus (IntVal v1) (IntVal v2) = Right (IntVal (v1 - v2))
operate Times (IntVal v1) (IntVal v2) = Right (IntVal (v1 * v2))
operate Div (IntVal v1) (IntVal v2)
  | v2 == 0 = Left "Error: Div by 0."
  | otherwise = Right (IntVal (v1 `div` v2))
operate Mod (IntVal v1) (IntVal v2)
  | v2 == 0 = Left "Error: Mod by 0."
  | otherwise = Right (IntVal (v1 `mod` v2))
operate Eq v1 v2
  | v1 == v2 = Right TrueVal
  | otherwise = Right FalseVal
operate Less (IntVal v1) (IntVal v2)
  | v1 < v2 = Right TrueVal
  | otherwise = Right FalseVal
operate Greater (IntVal v1) (IntVal v2)
  | v1 > v2 = Right TrueVal
  | otherwise = Right FalseVal
operate In v1 (ListVal []) = Right FalseVal
operate In v1 (ListVal (x:xs)) = 
    if (operate Eq v1 x == Right TrueVal)
      then Right TrueVal
    else
      operate In v1 (ListVal xs)
operate o v1 v2 = Left "Error: operate with invalid data type."

listHelper :: [Value] -> String
listHelper [] = ""
listHelper (x:[]) = printHelper x
listHelper (x:xs) = printHelper x ++ ", " ++ listHelper xs

printHelper :: Value -> String
printHelper NoneVal = "None"
printHelper TrueVal = "True"
printHelper FalseVal = "False"
printHelper (IntVal c) = show c
printHelper (StringVal x1) = x1
printHelper (ListVal x) = "[" ++ listHelper x ++ "]"

printFunc :: [Value] -> String
printFunc [] = ""
printFunc (x:[]) = printHelper x
printFunc (x:xs) = printHelper x ++ " " ++ printFunc xs

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

getRight :: Either a b -> b
getRight (Right v) = v

getLeft :: Either a b -> a
getLeft (Left s) = s

rangeFunc :: Value -> Value -> Value -> Either String [Value]
rangeFunc (IntVal x1) (IntVal x2) (IntVal x3)
  | x3 == 0 = Left "Error :Range function will not terminate for x3 == 0."
  | x1 <= x2 && x3 < 0 = Right []
  | x1 >= x2 && x3 > 0 = Right []
  | otherwise = Right ((IntVal x1):(getRight (rangeFunc (IntVal (x1+x3)) (IntVal x2) (IntVal x3))))
rangeFunc x1 x2 x3 = Left "Error: Range function can only use IntVal."

apply :: FName -> [Value] -> Comp Value
apply "range" [e1, e2, e3] =
  let tmp = rangeFunc e1 e2 e3 in
    if isRight tmp
      -- then withBinding "_" (ListVal (getRight tmp)) (look "_")
      then wrapComp (ListVal (getRight tmp))
    else
      abort (EBadArg (getLeft tmp))
apply "range" [e1, e2] = apply "range" [e1, e2, IntVal 1]
apply "range" [e2] = apply "range" [IntVal 0, e2, IntVal 1]
apply "range" x = abort (EBadArg "Error: range should have 1-3 arguments.")
-- apply "print" x = withBinding "_" (StringVal (printFunc x)) (look "_")
-- apply "print" x = wrapComp (StringVal (printFunc x))
apply "print" x = 
  do 
  {
    output (printFunc x)
    ;return NoneVal
  }

apply a b = abort (EBadFun a)

wrapComp :: Value -> Comp Value
wrapComp x = withBinding "_" x (look "_")

extractList :: Value -> [Value]
extractList (ListVal x) = x
extractList x = []

-- Main functions of interpreter
eval :: Exp -> Comp Value
eval (Const x) = wrapComp x
eval (Var v) = look v
eval (Oper op e1 e2) =
  do
  {
    x1 <- eval e1
    ;x2 <- eval e2
    ;let tmp = operate op x1 x2 in
      if isRight tmp
        then wrapComp (getRight tmp)
      else
        abort (EBadArg (getLeft tmp))
  }

eval (Not e) =
  do
  {
    tmp <- eval e
    ;
    if truthy tmp
      then wrapComp FalseVal
    else
      wrapComp TrueVal
  }

eval (Call f []) = apply f []
eval (Call f e) = 
  do 
  {
    x <- eval (List e)
    ;apply f (extractList x)
  }

eval (List []) = wrapComp (ListVal [])
eval (List (e:es)) = 
  do 
  {
    x <- eval e
    ;xs <- eval (List es)
    ;wrapComp (ListVal (x:(extractList xs)))
  }

eval (Compr e []) = wrapComp NoneVal
eval (Compr e cc) = undefined
-- Don't quite understand the function

exec :: Program -> Comp ()
exec [] = return mempty

exec ((SDef v e):xs) =
  do {
    x <- eval e
    ;withBinding v x (exec xs)
  }
exec ((SExp e):xs) =
  do {
    eval e
    ;exec xs
  }

execute :: Program -> ([String], Maybe RunError)
execute x = 
  if isRight (fst (runComp (exec x) []))
    then
      (snd (runComp (exec x) []), Nothing)
  else
    (snd (runComp (exec x) []), Just (getLeft (fst (runComp (exec x) []))))



