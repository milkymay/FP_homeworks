module HW0.T4 where

import Data.Function (fix)
import GHC.Natural (Natural)

repeat' :: a -> [a] -- behaves like Data.List.repeat
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b] -- behaves like Data.List.map
map' = fix (\rec f x -> if null x then [] else f (head x) : rec f (tail x))

fib :: Natural -> Natural -- computes the n-th Fibonacci number
fib = fix (\rec latest prev cnt n -> if n < 1 then 0 else 
                                     if n < 3 then 1 else
                                     if cnt == n then latest + prev else 
                                     rec (latest + prev) latest (cnt + 1) n) 1 1 3

fac :: Natural -> Natural -- computes the factorial
fac = fix (\rec n -> if n <= 1 then 1 else n * rec (n - 1))
