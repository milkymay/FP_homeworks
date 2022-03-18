module HW2.T4 where

import HW2.T1
import Control.Monad

data State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f (S g) = S (mapAnnotated f . g)

wrapState :: a -> State s a
wrapState a = S (a :#)

mapAnnotatedS  :: (a -> State s a) -> (Annotated s a -> Annotated s (State s a))
mapAnnotatedS f (a :# s) = f a :# s

getArgS :: Annotated s a -> a
getArgS (a :# _) = a

joinState :: State s (State s a) -> State s a
joinState (S f) = S (\x -> case runS (S f) x of h :# rest -> runS h rest)

wrapAnnotatedMod   :: a -> Annotated a ()
wrapAnnotatedMod a = () :# a

modifyState :: (s -> s) -> State s ()
modifyState f = S (wrapAnnotatedMod . f)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

data Prim a =
    Add a a      -- (+)
  | Sub a a      -- (-)
  | Mul a a      -- (*)
  | Div a a      -- (/)
  | Abs a        -- abs
  | Sgn a        -- signum

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)
  
evalBin :: (Double -> Double -> Prim Double) -> Expr -> Expr -> (Double -> Double -> Double) -> State [Prim Double] Double
evalBin oper a b sign = 
  do { l <- eval a
     ; r <- eval b
     ; modifyState(\s -> oper l r : s)
     ; return (sign l r) }
  
evalUn :: (Double -> Prim Double) -> Expr -> (Double -> Double) -> State [Prim Double] Double
evalUn oper a sign = 
  do { res <- eval a
     ; modifyState(\s -> oper res : s)
     ; return(sign res) }

eval :: Expr -> State [Prim Double] Double
eval (Val a) = return a
eval (Op (Add a b)) = evalBin Add a b (+) 
eval (Op (Mul a b)) = evalBin Mul a b (*) 
eval (Op (Sub a b)) = evalBin Sub a b (-) 
eval (Op (Div a b)) = evalBin Div a b (/) 
eval (Op (Abs a)) = evalUn Abs a abs
eval (Op (Sgn a)) = evalUn Sgn a signum  
