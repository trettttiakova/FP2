{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module HW2.T4 
  ( Expr (..)
  , State (..)
  , Prim (..)
  , calculateValue
  , eval
  , evalBinary
  , evalUnary
  , getAnnotatedValue
  , getRunS
  , joinState
  , mapState
  , modifyState
  , wrapState
  ) where

import HW2.T1 ( Annotated(..), mapAnnotated )
import Control.Monad ( ap )

data State s a = S { runS :: s -> Annotated s a }

-- |Returns a new State with a runS function which works as follows:
-- 1) applies the given State's runS function
-- 2) applies the given functoin to the annotated value of the result.
mapState :: (a -> b) -> State s a -> State s b
mapState func S {runS = run} = S {runS = mapAnnotated func . run}

-- |Wraps value in State.
wrapState :: a -> State s a
wrapState a = S {runS = (a :#)}

-- |Returns value ignoring annotation.
getAnnotatedValue :: Annotated s a -> a
getAnnotatedValue (a :# _) = a

-- |Returns runS function from State.
getRunS :: State s a -> (s -> Annotated s a)
getRunS S {runS = run} = run

-- |Flattens two nested States. 
joinState :: State s (State s a) -> State s a
joinState S {runS = run} = S {runS = \s -> getRunS (getAnnotatedValue $ run s) s}

-- |Returns State with runS function that applies 
-- the given function to its argument.
modifyState :: (s -> s) -> State s ()
modifyState modify = S {runS = \s -> () :# modify s}

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
  x + y         = Op (Add x y)
  x * y         = Op (Mul x y)
  x - y         = Op (Sub x y)
  abs x         = Op (Abs x)
  signum x      = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)

-- |Calculates double value depending on constructor and arguments.
calculateValue :: Prim Double -> Double
calculateValue (Add x y) = x + y
calculateValue (Sub x y) = x - y
calculateValue (Mul x y) = x * y
calculateValue (Div x y) = x / y
calculateValue (Abs x)   = abs x
calculateValue (Sgn x)   = signum x

-- |Evaluates expression with unary operation (Abs, Sgn).
-- It takes two arguments:
-- x - expression under unary operation
-- constructor - unary operation constructor
evalUnary :: Expr -> (Double -> Prim Double) -> State [Prim Double] Double
evalUnary x constructor = do
  let (value :# lst)  = runS (eval x) []
  let newPrim         = constructor value
  let newState        = modifyState (++ newPrim : lst)
  let calculatedValue = calculateValue newPrim
  mapState (const calculatedValue) newState

-- |Evaluates expression with binary operation (Add, Sub, Mul, Div).
-- It takes three arguments:
-- x, y - expressions under binary operation
-- constructor - binary operation constructor
evalBinary :: Expr -> Expr -> (Double -> Double -> Prim Double) -> State [Prim Double] Double
evalBinary x y constructor = do
  let (valueX :# lstX)  = runS (eval x) []
  let (valueY :# lstY)  = runS (eval y) lstX
  let newPrim           = constructor valueX valueY
  let newState          = modifyState (++ newPrim : lstY)
  let calculatedValue   = calculateValue newPrim
  mapState (const calculatedValue) newState

-- |Evaluates expression and returns State 
-- with sequence of sub-evaluations and evaluated value.
eval :: Expr -> State [Prim Double] Double
eval (Val value)    = pure value
eval (Op (Abs x))   = evalUnary x Abs
eval (Op (Sgn x))   = evalUnary x Sgn
eval (Op (Add x y)) = evalBinary x y Add
eval (Op (Sub x y)) = evalBinary x y Sub
eval (Op (Mul x y)) = evalBinary x y Mul
eval (Op (Div x y)) = evalBinary x y Div