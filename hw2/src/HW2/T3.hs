module HW2.T3 
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import HW2.T1
  ( Fun(..), List(..), Except(..), Annotated(..), Option(..) )
import HW2.T2 (concatTwoLists)

-- |Flattens the given Option:
-- returns None if outer or inner Option is None.
-- drops outer Some otherwise.
joinOption :: Option (Option a) -> Option a
joinOption None            = None
joinOption (Some None)     = None
joinOption (Some (Some a)) = Some a

-- |Flattens the given Except:
-- returns Error if outer or inner Except is Error.
-- drops outer Success otherwise.
joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Error e)             = Error e
joinExcept (Success (Error e))   = Error e
joinExcept (Success (Success a)) = Success a

-- |Flattens the given Annotated:
-- combines two annotations with an associative function.
joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e2) :# e1) = a :# (e1 <> e2)

-- |Returns Nil is passed as an argument,
-- flattens the List of Lists otherwise.
joinList :: List (List a) -> List a
joinList Nil = Nil
joinList (lst :. lsts) = concatTwoLists lst (joinList lsts)

-- |Takes Fun as an argument and returns its function.
getFunction :: Fun i a -> (i -> a)
getFunction (F func) = func

-- |Flattens the given Fun and returns a new Fun with 
-- a function that works as follows:
-- 1) applies the outer function to the argument
-- 2) extracts the function from the result
-- 3) applies the extratced function to the argument and returns the result.
joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F func) = F (\i -> getFunction (func i) i)   -- func :: i -> F (i -> a)