module HW2.T2 
  ( concatTwoLists
  , distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

import HW2.T1
  ( Fun(..),
    List(..),
    Stream(..),
    Prioritised(..),
    Except(..),
    Annotated(..),
    Quad(..),
    Pair(..),
    Option(..) )

-- |Returns None if any of the arguments is None,
-- constructs Some with paired elements of the given Some's otherwise.
distOption :: (Option a, Option b) -> Option (a, b)
distOption (None, _)        = None
distOption (_, None)        = None
distOption (Some a, Some b) = Some (a, b)

-- | Wraps the given value in Some.
wrapOption :: a -> Option a
wrapOption = Some

-- |Returns a Pair with values constructed from two given Pairs as follows:
-- first value is a pair of first values of the given Pairs,
-- second value is a pair of second values of the given Pairs.
distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

-- |Returns a Pair with both values equal to the given argument.
wrapPair :: a -> Pair a
wrapPair a = P a a

-- |Returns a Quad with values constructed from four given Quads as follows:
-- n-th value is a pair of n-th values of the given Quads.
distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

-- |Returns a Quad with four values equal to the given argument.
wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

-- |Returns an Annotated constructed from two given Annotated values as follows:
-- first part is a pair of first parts of the given Annotated values,
-- second part is a combined value of two Semigroup instance values.
distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# (e1 <> e2)

-- |Wraps the given value in Annotated. It is annotated with mempty.
wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

-- |Return Error if any of the arguments is Error 
-- (taking the message from the earlier argument that is Error),
-- returns Success of a pair construncted from two values
-- of the given Success's otherwise.
distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e
distExcept (Success a, Success b) = Success (a, b)

-- |Wraps the given value in Success.
wrapExcept :: a -> Except e a
wrapExcept = Success

-- |Returns a Prioritised with a pair constructed from 
-- the two values of the given Prioritised's,
-- picking the higher priority construnctor.
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, High b)     = High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)

-- |Wraps the given value in Low.
wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

-- |Returns a Stream of pairs constructed from values 
-- in the given Streams.
distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> strA, b :> strB) = (a, b) :> distStream (strA, strB)

-- |Returns the stream of values equal to the argument.
wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

-- |Appends the given element to each element 
-- of the given list creating a list of pairs.
appendToEach :: a -> List b -> List (a, b)
appendToEach _ Nil       = Nil
appendToEach a (b :. bs) = (a, b) :. appendToEach a bs

-- |Takes two lists as arguments and returns a concatenated list.
concatTwoLists :: List a -> List a -> List a
concatTwoLists Nil Nil     = Nil
concatTwoLists a Nil       = a
concatTwoLists Nil a       = a
concatTwoLists (a :. as) b = a :. concatTwoLists as b

-- |Returns Nil if any of the values of the given pair is Nil,
-- returns a List of pairs constructed from the given Lists otherwise.
distList :: (List a, List b) -> List (a, b)
distList (Nil, _)        = Nil
distList (_, Nil)        = Nil
distList (a :. as, lstB) = concatTwoLists (appendToEach a lstB) (distList (as, lstB))

-- |Returns a List consisting of the argument.
wrapList :: a -> List a
wrapList a = a :. Nil

-- |Returns a Fun with a function that works as follows:
-- applies two functions from the given Funs to the arguments
-- and returns the pair of the results.
distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F fia, F fib) = F (\i -> (fia i, fib i))

-- |Returns a Fun with a function that returns the argument of wrapFun.
wrapFun :: a -> Fun i a
wrapFun a = F (const a)