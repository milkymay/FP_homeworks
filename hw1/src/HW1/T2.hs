module HW1.T2
  ( N (..),
    nplus,
    nmult,
    nsub,
    ncmp,
    nFromNatural,
    nToNum,
    nEven,
    nOdd,
    ndiv,
    nmod,
  )
where

import Numeric.Natural (Natural)

data N = Z | S N

nplus :: N -> N -> N -- addition
nplus Z right = right
nplus (S left) right = S (nplus left right)

nmult :: N -> N -> N -- multiplication
nmult Z _ = Z
nmult (S left) right = nplus right (nmult left right)

nsub :: N -> N -> Maybe N -- subtraction     (Nothing if result is negative)
nsub Z Z = Just Z
nsub Z _ = Nothing
nsub (S left) Z = Just (S left)
nsub (S left) (S right) = nsub left right

nsubjust :: N -> N -> N
nsubjust Z Z = Z
nsubjust Z _ = Z
nsubjust (S left) Z = S left
nsubjust (S left) (S right) = nsubjust left right

ncmp :: N -> N -> Ordering -- comparison      (Do not derive Ord)
ncmp Z Z = EQ
ncmp Z _ = LT
ncmp _ Z = GT
ncmp (S left) (S right) = ncmp left right

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural a = S (nFromNatural (a - 1))

nToNum :: Num a => N -> a
nToNum Z = 0
nToNum (S n) = nToNum n + 1

nEven, nOdd :: N -> Bool -- parity checking
nEven Z = True
nEven (S Z) = False
nEven (S (S a)) = nEven a
nOdd Z = False
nOdd (S Z) = True
nOdd (S (S a)) = nOdd a

ndiv :: N -> N -> N -- integer division
ndiv _ Z = error "Division by zero"
ndiv left right = if ncmp left right == LT then Z else S (ndiv (nsubjust left right) right)

nmod :: N -> N -> N -- modulo operation
nmod _ Z = error "Division by zero"
nmod left right = if ncmp left right == LT then left else nmod (nsubjust left right) right
