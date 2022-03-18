module HW1.T4
  ( tfoldr,
    treeToList,
  )
where

import HW1.T3

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ b Leaf = b
tfoldr f b (Branch _ left node right) = tfoldr f (f node (tfoldr f b right)) left

treeToList :: Tree a -> [a] -- output list is sorted
treeToList = tfoldr (:) []
