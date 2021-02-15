module Warmup where

type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1, y)
-- complete the definition

moves :: [Direction] -> Pos -> Pos
moves [] (x,y) = (x,y)
moves a (x,y) = moves (tail a) (move (head a) (x,y))
-- replace with actual definition of moves, and likewise for the
-- other 'undefined' functions

data Nat = Zero | Succ Nat
  deriving (Eq, Show, Read, Ord)

add :: Nat -> Nat -> Nat
add x Zero = x
add x (Succ y) = add (Succ x) y

mult :: Nat -> Nat -> Nat
mult Zero y = Zero
mult (Succ x) y = add (mult x y) y

-- Do not use these to define add/mult!
nat2int :: Nat -> Int
nat2int Zero = 0::Int
nat2int (Succ x) = nat2int x + 1::Int

int2nat :: Int -> Nat
int2nat 0 = Zero::Nat
int2nat x = Succ (int2nat (x-1))

data Tree = Leaf | Node Int Tree Tree
  deriving (Eq, Show, Read, Ord)

insert :: Int -> Tree -> Tree
insert x Leaf = Node x Leaf Leaf
insert x (Node a t1 t2)
  | x == a = Node a t1 t2
  | x < a = Node a (insert x t1) t2
  | x > a = Node a t1 (insert x t2)


