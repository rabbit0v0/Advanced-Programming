-- This is a skeleton file for you to edit

{-# OPTIONS_GHC -W #-}  -- Just in case you forgot...

module Arithmetic
  (
  showExp,
  evalSimple,
  extendEnv,
  evalFull,
  evalErr,
  showCompact,
  evalEager,
  evalLazy
  )

where

import Definitions

showExp :: Exp -> String
showExp (Cst x)
  | x >= 0 = show x
  | otherwise = "(" ++ show x ++ ")"
showExp (Add x y) = "(" ++ showExp x ++ "+" ++ showExp y ++ ")"
showExp (Sub x y) = "(" ++ showExp x ++ "-" ++ showExp y ++ ")"
showExp (Mul x y) = "(" ++ showExp x ++ "*" ++ showExp y ++ ")"
showExp (Div x y) = "(" ++ showExp x ++ "/" ++ showExp y ++ ")"
showExp (Pow x y) = "(" ++ showExp x ++ "^" ++ showExp y ++ ")"
showExp _ = error "Can only use Cst, Add, Mul, Div and Pow here in showExp."

evalSimple :: Exp -> Integer
evalSimple (Cst x) = x
evalSimple (Add x y) = evalSimple x + evalSimple y
evalSimple (Sub x y) = evalSimple x - evalSimple y
evalSimple (Mul x y) = evalSimple x * evalSimple y
evalSimple (Div x y) = (evalSimple x) `div` evalSimple y
evalSimple (Pow x y) = (evalSimple x) ^ evalSimple y
evalSimple _ = error "Can only use Cst, Add, Mul, Div and Pow here in evalSimple."

extendEnv :: VName -> Integer -> Env -> Env
extendEnv v n r = \x->if x == v then Just n else r x

-- Extract the Interger from (Just Interger) expression.
getJust :: Maybe Integer -> Integer
getJust (Just x) = x
getJust _ = undefined

-- Extract the value from (Right Integer).
-- Only value checked by checkEither would use this function. 
-- Thus there is only Right value here.
getEither :: Either ArithError Integer -> Integer
getEither (Right x) = x

-- Check if the Either expression is like (Right something).
checkEither :: Either ArithError Integer -> Bool
checkEither (Right _) = True
checkEither (Left _) = False

-- Pass the error message.
-- If the first expression returns (Left somthing), then return it,
-- else return the result of the second expression.
showEither :: Either ArithError Integer -> Either ArithError Integer -> Either ArithError Integer
showEither a b = 
  if not (checkEither a)
      then a
    else b

evalFull :: Exp -> Env -> Integer
evalFull (Cst x) _ = x
evalFull (Var v) r = if r v == Nothing then error "Variable is not bound." else getJust (r v)
evalFull (Add x y) r = (evalFull x r) + (evalFull y r)
evalFull (Sub x y) r = (evalFull x r) - (evalFull y r)
evalFull (Mul x y) r = (evalFull x r) * (evalFull y r) -- if the fst one is 0, don't need to calculate the snd one?
evalFull (Div x y) r = (evalFull x r) `div` (evalFull y r)
evalFull (Pow x y) r = (evalFull x r) ^ (evalFull y r)
-- Add () to all x, y to make it more clear.
evalFull (If e1 e2 e3) r
  | evalFull e1 r == 0 = evalFull e3 r
  | otherwise = evalFull e2 r
evalFull (Let v e1 e2) r = evalFull e2 (extendEnv v (evalFull e1 r) r)
evalFull (Sum v e1 e2 e3) r
  | evalFull e1 r > evalFull e2 r = 0
  | otherwise = (evalFull e3 (extendEnv v (evalFull e1 r) r)) + (evalFull (Sum v (Add e1 (Cst 1)) e2 e3) r)  -- the snd don't need a extendEnv because v is covered later
evalFull _ _ = error "Undefined arithmetic operations is called in evalFull."

evalErr :: Exp -> Env -> Either ArithError Integer
evalErr (Cst x) _ = Right x

evalErr (Var v) r = 
  if r v == Nothing
    then Left (EBadVar v) 
  else Right (getJust (r v))

evalErr (Add x y) r = 
  if checkEither (evalErr x r) && checkEither (evalErr y r)
    then Right $ getEither (evalErr x r) + getEither (evalErr y r)
  else
    showEither (evalErr x r) (evalErr y r)

evalErr (Sub x y) r = 
  if checkEither (evalErr x r) && checkEither (evalErr y r)
    then Right $ getEither (evalErr x r) - getEither (evalErr y r)
  else
    showEither (evalErr x r) (evalErr y r)

evalErr (Mul x y) r = 
  if checkEither (evalErr x r) && checkEither (evalErr y r)
   then Right $ getEither (evalErr x r) * getEither (evalErr y r)
  else
    showEither (evalErr x r) (evalErr y r)

evalErr (Div x y) r = 
  if checkEither (evalErr x r) && checkEither (evalErr y r)
    then
      if getEither (evalErr y r) == 0 
        then Left EDivZero 
      else Right $ (getEither (evalErr x r)) `div` (getEither (evalErr y r))
  else
    showEither (evalErr x r) (evalErr y r)

evalErr (Pow x y) r = 
  if checkEither (evalErr x r) && checkEither (evalErr y r)
    then
      if getEither (evalErr y r) < 0 
        then Left ENegPower 
      else Right $ (getEither(evalErr x r)) ^ (getEither(evalErr y r))
  else
    showEither (evalErr x r) (evalErr y r)

evalErr (If e1 e2 e3) r = 
  if checkEither (evalErr e1 r)
    then
      if getEither (evalErr e1 r) == 0
        then 
          evalErr e3 r
      else
        evalErr e2 r
  else
    evalErr e1 r

evalErr (Let v e1 e2) r = 
  if checkEither (evalErr e1 r)
    then evalErr e2 (extendEnv v (getEither(evalErr e1 r)) r)
  else 
    evalErr e1 r

evalErr (Sum v e1 e2 e3) r = 
  if checkEither (evalErr e1 r) && checkEither (evalErr e2 r)
    then
      if getEither (evalErr e1 r) > getEither (evalErr e2 r)
        then Right 0
      else
        if checkEither (evalErr e3 (extendEnv v (getEither(evalErr e1 r)) r)) && checkEither (evalErr (Sum v (Add e1 (Cst 1)) e2 e3) r)
          then Right $ (getEither (evalErr e3 (extendEnv v (getEither(evalErr e1 r)) r))) + (getEither (evalErr (Sum v (Add e1 (Cst 1)) e2 e3) r))
        else
          showEither (evalErr e3 (extendEnv v (getEither(evalErr e1 r)) r)) (evalErr (Sum v (Add e1 (Cst 1)) e2 e3) r)
  else
    showEither (evalErr e1 r) (evalErr e2 r)



-- optional parts (if not attempted, leave them unmodified)

showCompact :: Exp -> String
showCompact = undefined

evalEager :: Exp -> Env -> Either ArithError Integer
evalEager = undefined

evalLazy :: Exp -> Env -> Either ArithError Integer
evalLazy = undefined
