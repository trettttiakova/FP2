module HW2.T1 
  ( Annotated (..)
  , Except (..)
  , Fun (..)
  , List (..)
  , Option (..)
  , Pair (..)
  , Prioritised (..)
  , Quad (..)
  , Stream (..)
  , Tree (..)
  , mapAnnotated
  , mapExcept
  , mapFun
  , mapList
  , mapOption
  , mapPair
  , mapPrioritised
  , mapQuad
  , mapStream
  , mapTree
  ) where

data Option a = None | Some a

-- |Applies the given function to the value inside 
-- the given Some if (Some a) is given as the second argument,
-- returns None otherwise.
mapOption :: (a -> b) -> (Option a -> Option b)
mapOption _ None        = None
mapOption func (Some a) = Some $ func a

data Pair a = P a a

-- |Applies the given function to the values inside the given Pair.
mapPair :: (a -> b) -> (Pair a -> Pair b)
mapPair func (P a1 a2) = P (func a1) (func a2)

data Quad a = Q a a a a

-- |Applies the given function to the values inside the given Quad.
mapQuad :: (a -> b) -> (Quad a -> Quad b)
mapQuad func (Q a1 a2 a3 a4) = Q (func a1) (func a2) (func a3) (func a4)

data Annotated e a = a :# e
infix 0 :#

-- |Applies the given function to the value inside 
-- the given Annotation.
mapAnnotated :: (a -> b) -> (Annotated e a -> Annotated e b)
mapAnnotated func (a :# e) = func a :# e

data Except e a = Error e | Success a

-- |Applies the given function to the value inside 
-- the given Success if (Success a) is given as the second argument,
-- returns Error otherwise.
mapExcept :: (a -> b) -> (Except e a -> Except e b)
mapExcept _ (Error e)      = Error e
mapExcept func (Success a) = Success $ func a

data Prioritised a = Low a | Medium a | High a

-- |Applies the given function to the values inside
-- the given Prioritised leaving the constructor the same.
mapPrioritised :: (a -> b) -> (Prioritised a -> Prioritised b)
mapPrioritised func (Low a)    = Low $ func a
mapPrioritised func (Medium a) = Medium $ func a
mapPrioritised func (High a)   = High $ func a

data Stream a = a :> Stream a
infixr 5 :>

-- |Applies the given function to the value inside Stream recursively.
mapStream :: (a -> b) -> (Stream a -> Stream b)
mapStream func (a :> stream) = func a :> mapStream func stream

data List a = Nil | a :. List a
infixr 5 :.

-- |Applies the given function to all elements of the List.
mapList :: (a -> b) -> (List a -> List b)
mapList _ Nil          = Nil
mapList func (a :. as) = func a :. mapList func as

data Fun i a = F (i -> a)

-- |Returns a new Fun with function that is a combination
-- of the given function and the function from the given Fun.
mapFun :: (a -> b) -> Fun i a -> Fun i b
mapFun func (F f) = F (func . f)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

-- |Applies the given function to all the elements 
-- of the given Tree recursively.
mapTree :: (a -> b) -> (Tree a -> Tree b)
mapTree _ Leaf                     = Leaf
mapTree func (Branch left a right) = Branch (mapTree func left) (func a) (mapTree func right)