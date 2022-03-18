module HW2.T2 where

import HW2.T1

distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _) = None
distOption (_, None) = None
distOption (Some a, Some b) = Some (a, b)

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a a1, P b b1) = P (a, b) (a1, b1)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a a1 a2 a3, Q b b1 b2 b3) = Q (a, b) (a1, b1) (a2, b2) (a3, b3)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# (e1 <> e2)

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _) = Error e
distExcept (_, Error e) = Error e
distExcept (Success a, Success b) = Success (a, b)

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, High b) = High (a, b)
distPrioritised (High a, Medium b) = High (a, b)
distPrioritised (High a, Low b) = High (a, b)
distPrioritised (Medium a, High b) = High (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b) = Medium (a, b)
distPrioritised (Low a, High b) = High (a, b)
distPrioritised (Low a, Medium b) = Medium (a, b)
distPrioritised (Low a, Low b) = Low (a, b)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (h1 :> rest1, h2 :> rest2) = (h1, h2) :> distStream (rest1, rest2)

distList :: (List a, List b) -> List (a, b)
distList (Nil, Nil) = Nil
distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (a, b) = foldList (a, b, b)

foldList :: (List a, List b, List b) -> List (a, b)
foldList (Nil, _, _) = Nil
foldList (_, Nil, _) = Nil
foldList (h1 :. rest1, h2 :. Nil, stacked) = (h1, h2) :. foldList (rest1, stacked, stacked)
foldList (h1 :. rest1, h2 :. rest2, stacked) = (h1, h2) :. foldList (h1 :. rest1, rest2, stacked)

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F f, F g) = F (\x -> (f x, g x))

wrapOption :: a -> Option a
wrapOption = Some

wrapPair :: a -> Pair a
wrapPair a = P a a

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept :: a -> Except e a
wrapExcept = Success

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

wrapList :: a -> List a
wrapList a = a :. Nil

wrapFun         :: a -> Fun i a
wrapFun a = F (const a)
