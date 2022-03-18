module HW2.T5 where

import HW2.T1
import HW2.T2
import Control.Monad
import HW2.T4 hiding (eval)

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f (ES g) = ES (mapExcept (mapAnnotated f) . g)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES (\x -> wrapExcept (a :# x))

wrapAnnotatedModES   :: a -> Except e (Annotated a ())
wrapAnnotatedModES a = Success (() :# a)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES (\x -> case runES (ES f) x of 
  Success (h :# rest) -> runES h rest
  Error e -> Error e)

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES (wrapAnnotatedModES . f)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES (\_ -> Error e) 

data EvaluationError = DivideByZero

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)
  
evalBinES :: (Double -> Double -> Prim Double) -> Expr -> Expr -> (Double -> Double -> ExceptState EvaluationError [Prim Double] Double) -> ExceptState EvaluationError [Prim Double] Double
evalBinES oper a b sign = 
  do { l <- eval a
     ; r <- eval b
     ; modifyExceptState(\s -> oper l r : s)
     ; sign l r }
  
evalUnES :: (Double -> Prim Double) -> Expr -> (Double -> Double) -> ExceptState EvaluationError [Prim Double] Double
evalUnES oper a sign = 
  do { res <- eval a
     ; modifyExceptState(\s -> oper res : s)
     ; return(sign res) }

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val a) = return a
eval (Op (Add a b)) = evalBinES Add a b (\x y -> return (x + y)) 
eval (Op (Mul a b)) = evalBinES Mul a b (\x y -> return (x * y))
eval (Op (Sub a b)) = evalBinES Sub a b (\x y -> return (x - y))
eval (Op (Div a b)) = evalBinES Div a b (\x y -> 
  case y of 0 -> throwExceptState DivideByZero
            _ -> return (x / y)) 
eval (Op (Abs a)) = evalUnES Abs a abs
eval (Op (Sgn a)) = evalUnES Sgn a signum 