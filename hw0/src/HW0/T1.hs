{-# LANGUAGE TypeOperators #-}
module HW0.T1 where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Left a) = (Left a, Left a)
distrib (Right (b, c)) = (Right b, Right c)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso funcF funcG

funcF :: (a, (b, c)) -> ((a, b), c)
funcF (a, (b, c)) = ((a, b), c)

funcG :: ((a, b), c) -> (a, (b, c))
funcG ((a, b), c) = (a, (b, c))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither = Iso fEither gEither

fEither :: Either a (Either b c) -> Either (Either a b) c
fEither (Left a) = Left (Left a)
fEither (Right (Left b)) = Left (Right b)
fEither (Right (Right c)) = Right c

gEither :: Either (Either a b) c -> Either a (Either b c)
gEither (Right c) = Right (Right c)
gEither (Left (Left a)) = Left a
gEither (Left (Right b)) = Right (Left b)
