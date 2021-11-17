module HW2.T5 
  ( EvaluationError (..)
  , ExceptState (..)
  , eval
  , evalBinary
  , evalUnary
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , throwExceptState
  , wrapExceptState
  ) where

import Control.Monad ( ap )
import HW2.T1
    ( Except(..), Annotated(..), mapAnnotated, mapExcept )
import HW2.T4 ( Expr(Val, Op), Prim (Abs, Sgn, Add, Sub, Mul, Div), calculateValue )

data ExceptState e s a = ES { runES :: s -> Except e (Annotated s a) }

-- |Returns a new ExceptState with a runES function which works as follows:
-- 1) applies the given ExceptState's runES function
-- 2) returns Error if the result is Error
-- 3) applies the given functoin to the annotated value of the result otherwise.
mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState func ES {runES = run} = ES {runES = mapExcept (mapAnnotated func) . run}

-- |Wraps the given value in ExceptState with Success.
wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES {runES = \s -> Success (a :# s)}

-- |Extracts ExceptState that returns Error or 
-- ExceptState with function runES from inside Success
extractES :: Except e (Annotated s (ExceptState e s a)) -> ExceptState e s a
extractES (Error e) = ES {runES = \_ -> Error e}
extractES (Success (ES {runES = run} :# _)) = ES {runES = run}

-- |Flattens two ExceptStates.
joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState ES {runES = run} = ES {runES = \s -> runES (extractES (run s)) s}

-- |Returns ExceptState with runES function that applies 
-- the given function to its argument wrapped in Success.
modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState modify = ES {runES = \s -> Success (() :# modify s)}

-- |Returns ExceptState with runES function that
-- returns the given error argument wrapped in Error.
throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES {runES = \_ -> Error e}

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero 

-- |Returns True is given Except is Error,
-- False otherwise.
isError :: Except e a -> Bool
isError (Error _)   = True
isError (Success _) = False

-- |Returns True if given Prim is division,
-- False otherwise.
isDivision :: Prim a -> Bool
isDivision (Div _ _ ) = True
isDivision _          = False

-- |Evaluates expression with unary operation (Abs, Sgn) 
-- or returns ExceptState with error if division by zero occured in evaluation
-- It takes two arguments:
-- x - expression under unary operation
-- constructor - unary operation constructor
evalUnary :: Expr -> (Double -> Prim Double) -> ExceptState EvaluationError [Prim Double] Double
evalUnary x constructor = do
  let result = runES (eval x) []
  if isError result 
  then ES {runES = \_ -> Error DivideByZero}
  else do
    let (Success (value :# lst)) = result
    let newPrim                  = constructor value
    let newState                 = modifyExceptState (++ newPrim : lst)
    let calculatedValue          = calculateValue newPrim
    mapExceptState (const calculatedValue) newState

-- |Evaluates expression with binary operation (Add, Sub, Mul, Div) 
-- or returns ExceptState with error if division by zero occured in evaluation
-- It takes three arguments:
-- x, y - expressions under binary operation
-- constructor - binary operation constructor
evalBinary :: Expr -> Expr -> (Double -> Double -> Prim Double) -> ExceptState EvaluationError [Prim Double] Double
evalBinary x y constructor = do
  let resultX = runES (eval x) []
  if isError resultX
  then ES {runES = \_ -> Error DivideByZero}
  else do
    let Success (valueX :# lstX) = resultX
    let resultY                  = runES (eval y) lstX
    if isError resultY
    then ES {runES = \_ -> Error DivideByZero}
    else do
      let Success (valueY :# lstY) = runES (eval y) lstX
      if valueY == 0 && isDivision (constructor valueX valueY)
      then ES {runES = \_ -> Error DivideByZero}
      else do
        let newPrim         = constructor valueX valueY
        let newState        = modifyExceptState (++ newPrim : lstY)
        let calculatedValue = calculateValue newPrim
        mapExceptState (const calculatedValue) newState

-- |Evaluates expression and returns ExceptState with EvaluationError
-- or with sequence of sub-evaluations and evaluated value.
eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val value)    = pure value
eval (Op (Abs x))   = evalUnary x Abs
eval (Op (Sgn x))   = evalUnary x Sgn
eval (Op (Add x y)) = evalBinary x y Add
eval (Op (Sub x y)) = evalBinary x y Sub
eval (Op (Mul x y)) = evalBinary x y Mul
eval (Op (Div x y)) = evalBinary x y Div