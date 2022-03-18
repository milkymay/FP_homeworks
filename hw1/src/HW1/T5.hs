module HW1.T5
  ( splitOn,
    joinWith,
  )
where

import Data.List.NonEmpty (NonEmpty (..))

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn delimiter =
  foldr
    ( \curChar (curList :| listOfProcessed) ->
        if curChar == delimiter
          then [] :| (curList : listOfProcessed)
          else (curChar : curList) :| listOfProcessed
    )
    ([] :| [])

joinWith :: a -> NonEmpty [a] -> [a]
joinWith joiner (curToken :| listOfUnprocessed) =
  mappend curToken (foldMap (joiner :) listOfUnprocessed)
