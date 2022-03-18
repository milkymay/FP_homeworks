module HW2.T3 where
  
import HW2.T1
  
joinOption    :: Option (Option a) -> Option a
joinOption (Some a) = a
joinOption None = None

joinExcept    :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success (Error e)) = Error e
joinExcept (Success (Success a)) = Success a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e2) :# e1) = a :# (e1 <> e2) 

joinList      :: List (List a) -> List a
joinList Nil = Nil
joinList (Nil :. Nil) = Nil
joinList (Nil :. b) = joinList b
joinList ((a :. Nil) :. Nil) = a :. Nil
joinList ((a :. b) :. Nil) = a :. b
joinList ((a :. b) :. c) = a :. joinList (b :. c)

joinFun       :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F (\x -> case f x of F g -> g x)