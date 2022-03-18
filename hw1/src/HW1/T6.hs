module HW1.T6
  ( mcat,
    epart,
  )
where

fromMaybe :: (Monoid a) => Maybe a -> a
fromMaybe (Just a) = a
fromMaybe Nothing = mempty

mcat :: Monoid a => [Maybe a] -> a
mcat x = mconcat (map fromMaybe x)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart [] = (mempty, mempty)
epart (Left a : xs) = let p = epart xs in (mappend a (fst p), snd p)
epart (Right b : xs) = let p = epart xs in (fst p, mappend b (snd p))
