module HW0.T5 where

import GHC.Natural (Natural)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ x = x

ns :: Nat a -> Nat a
ns n f x = f (n f x)

nplus, nmult :: Nat a -> Nat a -> Nat a
nplus a b f x = a f (b f x)
nmult a b = a . b

nFromNatural :: Natural -> Nat a
nFromNatural = doNTimes nz

doNTimes :: Nat a -> Natural -> Nat a
doNTimes x 0 = x
doNTimes x n = doNTimes (ns x) (n - 1) 

nToNum :: Num a => Nat a -> a
nToNum x = x (+1) 0