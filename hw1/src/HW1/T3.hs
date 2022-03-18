module HW1.T3
  ( Tree (..),
    tsize,
    tdepth,
    tmember,
    tinsert,
    tFromList,
  )
where

data Tree a = Leaf | Branch (Int, Int) (Tree a) a (Tree a) deriving (Show)

-- | Size of the tree, O(1).
tsize :: Tree a -> Int
tsize Leaf = 0
tsize (Branch (sz, _) _ _ _) = sz

-- | Depth of the tree.
tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth (Branch (_, depth) _ _ _) = depth

-- | Check if the element is in the tree, O(log n)
tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember el (Branch _ leftTree a rightTree)
  | el == a = True
  | el > a = tmember el rightTree
  | otherwise = tmember el leftTree

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch leftTree node rightTree =
  Branch (tsize leftTree + 1 + tsize rightTree, max (tdepth leftTree) (tdepth rightTree) + 1) leftTree node rightTree

-- | Insert an element into the tree, O(log n)
tinsert :: Ord a => a -> Tree a -> Tree a
tinsert el tree
  | tmember el tree = tree
  | otherwise = taddinginsert el tree

taddinginsert :: Ord a => a -> Tree a -> Tree a
taddinginsert el Leaf = mkBranch Leaf el Leaf
taddinginsert el (Branch _ leftTree node rightTree)
  | el > node = manageBalance (mkBranch leftTree node (taddinginsert el rightTree))
  | el < node = manageBalance (mkBranch (taddinginsert el leftTree) node rightTree)
  | otherwise = manageBalance (mkBranch leftTree node rightTree)

-- | Build a tree from a list, O(n log n)
tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

balance :: Tree a -> Int
balance Leaf = 0
balance (Branch _ left _ right) = tdepth right - tdepth left 

leftRotate :: Tree a -> Tree a
leftRotate (Branch _ leftTree node (Branch _ leftGranny child rightGranny)) = 
  mkBranch (mkBranch leftTree node leftGranny) child rightGranny
leftRotate x = x
  
rightRotate :: Tree a -> Tree a
rightRotate (Branch _ (Branch _ leftGranny child rightGranny) node rightTree) = 
  mkBranch leftGranny child (mkBranch rightGranny node rightTree)
rightRotate x = x

bigLeftRotate :: Tree a -> Tree a
bigLeftRotate (Branch _ leftTree node rightTree) = leftRotate (mkBranch leftTree node (rightRotate rightTree)) 
bigLeftRotate x = x

bigRightRotate :: Tree a -> Tree a
bigRightRotate (Branch _ leftTree node rightTree) = rightRotate (mkBranch (leftRotate leftTree) node rightTree) 
bigRightRotate x = x
  
manageBalance :: Tree a -> Tree a
manageBalance (Branch _ leftTree node rightTree) =
  case balance (mkBranch leftTree node rightTree) of
    2 -> case balance rightTree of 
          -1 -> bigLeftRotate (mkBranch leftTree node rightTree) 
          _ -> leftRotate (mkBranch leftTree node rightTree) 
    -2 -> case balance leftTree of 
          1 -> bigRightRotate (mkBranch leftTree node rightTree)
          _ -> rightRotate (mkBranch leftTree node rightTree)
    _ -> mkBranch leftTree node rightTree
manageBalance x = x
