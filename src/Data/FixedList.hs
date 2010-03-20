{-#LANGUAGE MultiParamTypeClasses,
            FunctionalDependencies,
            FlexibleInstances,
            UndecidableInstances #-}
{- |
A fixed length list library.

The length of a list is encoded into it's type in a natural way. This allows
you to do things like specify that two list parameters have the same type,
which also forces them to have the same length. This can be a handy property.
It's not as flexible as the standard haskell list, but the added type safety is
sometimes worth it.

The entire library is Haskell98 except for the 'Append' typeclass. (which could
be easily removed if needed).

Most of your usual list functions ('foldr', 'fmap', 'sum', 'sequence', etc..)
are accessed via the 'Functor', 'Applicative', 'Foldable', and 'Traversable'
type classes.

The Equivalent of zipWith can be had via the Applicative instance:

> zipWith f xs ys = pure f <*> xs <*> ys

Also, 'sequenceA' transposes a FixedList of FixedLists.

The monad instance is also interesting.
return fills the list with the given element (remember that list size is
dependent on the type) You can think of bind as operating like this:

> m >>= k = diagonal $ fmap k m

This takes the FixedList m and maps k accross it, (which must return a FixedList)
which results in a FixedList of FixedLists the diagonal of which is returned.
The actually implementation is more elegant, but works essentialy the same.

This also means that 'join' gets the diagonal of a FixedList of FixedLists.

You can construct FixedLists like so:

> t1 :: Cons (Cons (Cons Nil)) Integer -- this is the same as FixedList3 Integer
> t1 = 1 :. 3 :. 5 :. Nil
>
> t2 :: FixedList3 Integer  -- type signature needed! and must be correct!
> t2 = fromFoldable' [4, 1, 0]
>
> t3 :: FixedList3 Integer -- type signature needed!
> t3 :: fromFoldable' [1..]
>
> t4 :: FixedList3 (FixedList3 Integer)
> t4 = t1 :. t2 :. t3 :. Nil
>
> -- get the sum of the diagonal of the transpose of t4
> test :: FixedList3 Integer
> test = sum $ join $ sequenceA $ t4

If you want to restrict a type to be a 'FixedList', but don't want to specify the
size of the list, use the 'FixedList' typeclass:

> myFunction :: (FixedList f) => f a -> Float

On a side note...
I think that if Haskell supported infinite types my 'Append' typeclass would
only have one parameter and I wouldn't need all those nasty extensions.

I think I could also implement direct, typesafe, versions of 'last', 'init', 'reverse'
and 'length' that don't depend on 'Foldable'. *sigh*  Maybe Haskell will one day
support such things.

This library is hosted on github (click on the /Contents/ (if you are viewing this
on hackage) link above and you should see the Homepage link) so it should be very
easy to forked it, patch it, and send patches back to me.

-}
module Data.FixedList (
                      -- * Types and Classes
                        Cons(..)
                      , Nil(..)
                      , FixedList
                      , Append (..)
                      -- * Baisc Functions that are not found in 'Traversable' or 'Foldable'
                      , reverse
                      , length
                      , last
                      , init
                      , unit
                      , subLists
                      , fromFoldable
                      , fromFoldable'
                      -- * Type synonyms for larger lists
                      , FixedList0, FixedList1, FixedList2, FixedList3, FixedList4
                      , FixedList5, FixedList6, FixedList7, FixedList8, FixedList9
                      , FixedList10, FixedList11, FixedList12, FixedList13, FixedList14
                      , FixedList15, FixedList16, FixedList17, FixedList18, FixedList19
                      , FixedList20, FixedList21, FixedList22, FixedList23, FixedList24
                      , FixedList25, FixedList26, FixedList27, FixedList28, FixedList29
                      , FixedList30, FixedList31, FixedList32
                      ) where

import Control.Applicative
import Control.Monad hiding (sequence) --hiding for haddock
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Maybe

import qualified Data.List
import Prelude hiding (head, tail, foldr, sum, sequence, reverse, length, last, init)

data FixedList f =>
  Cons f a = (:.) {
    head :: a,
    tail :: (f a)
  }  deriving (Eq, Ord)

data Nil a = Nil
  deriving (Eq, Ord)

infixr 5 :.

instance (FixedList f, Show a) => Show (Cons f a) where
  show x = "|" ++ show (toList x) ++ "|"
instance Show (Nil a) where
  show Nil = "|[]|"

-- | Just a restrictive typeclass. It makes sure ':.' only takes FixedLists as it's second parameter
--  and makes sure the use of fromFoldable's in reverse, and init is safe.
class (Applicative f, Traversable f, Monad f) => FixedList f
instance FixedList f => FixedList (Cons f)
instance FixedList Nil

-- The only very bad ugly, everything else is haskell98
class Append f g h | f g -> h, f h -> g where
  append :: f a -> g a -> h a
instance (FixedList f, FixedList c, Append f b c) => Append (Cons f) b (Cons c) where
  append (x :. xs) ys = x :. (xs `append` ys)
instance Append Nil a a where
  append Nil ys = ys


reverse :: FixedList t => t a -> t a
reverse xs = fromFoldable' $ Data.List.reverse $ toList xs

length :: Foldable t => t a -> Int
length xs = Data.List.length $ toList xs

-- | Returns the last element of the list
last :: Foldable t => t a -> a
last xs = Data.List.last $ toList xs

-- | Returns all but the last element of the list
init :: FixedList f => Cons f a -> f a
init xs = fromFoldable' $ Data.List.init $ toList xs

-- | Constructs a FixedList containing a single element.
--   Normally I would just use pure or return for this,
--   but you'd have to specify a type signature in that case.
unit :: a -> Cons Nil a
unit a = a :. Nil

-- | Given a list, returns a list of copies of that list but each with an element removed.
--   for example:
--  > subLists (1:. 2:. 3:. Nil)
--  gives
--  > |[|[2,3]|,|[1,3]|,|[1,2]|]|
--
subLists :: FixedList f => Cons f a -> Cons f (f a)
subLists xs = fromFoldable' $ fmap fromFoldable' $ subLists' $ toList xs
  where
    subLists' a = zipWith (++) (Data.List.inits a) ( Data.List.tail $ Data.List.tails a)

-- | Converts any Foldable to any Applicative Traversable.
--   However, this will only do what you want if 'pure' gives you the
--   shape of structure you are expecting.
fromFoldable :: (Foldable f, Applicative g, Traversable g) => f a -> Maybe (g a)
fromFoldable t = sequenceA $ snd $ mapAccumL f (toList t) (pure ())
  where
    f [] _ = ([], Nothing)
    f (x:xs) _ = (xs, Just x)

-- | This can crash if the foldable is smaller than the new structure.
fromFoldable' :: (Foldable f, Applicative g, Traversable g) => f a -> g a
fromFoldable' a = fromJust $ fromFoldable a

instance FixedList f => Functor (Cons f) where
  fmap f (a :. b) = f a :. fmap f b

instance FixedList f => Foldable (Cons f) where
  foldMap f (a :. b) = f a `mappend` foldMap f b

instance FixedList f => Traversable (Cons f) where
  traverse f (a :. b) = (:.) <$> f a <*> traverse f b

instance FixedList f => Monad (Cons f) where
  return x = x :. return x
  (a :. b) >>= k = head (k a) :. (b >>= (tail . k))

instance FixedList f => Applicative (Cons f) where
  pure = return
  (<*>) = ap


instance Functor Nil where
  fmap _ Nil = Nil

instance Foldable Nil where
  foldMap _ Nil = mempty

instance Traversable Nil where
  traverse _ Nil = pure Nil

instance Monad Nil where
  return _  = Nil
  Nil >>= _ = Nil -- should I define this as _ >>= _ = Nil ? would this
                  -- have the right behavior with bottom?

instance Applicative Nil where
  pure = return
  (<*>) = ap

instance (Num a, FixedList f, Eq (f a), Show (f a)) => Num (Cons f a) where
  a + b = pure (+) <*> a <*> b
  a - b = pure (-) <*> a <*> b
  a * b = pure (*) <*> a <*> b
  negate a = pure negate <*> a
  abs a = pure abs <*> a
  signum = fmap signum
  fromInteger = pure . fromInteger

instance (Fractional a, FixedList f, Eq (f a), Show (f a)) => Fractional (Cons f a) where
  a / b = pure (/) <*> a <*> b
  recip a = pure 1 / a
  fromRational = pure . fromRational

type FixedList0 = Nil
type FixedList1 = Cons FixedList0
type FixedList2 = Cons FixedList1
type FixedList3 = Cons FixedList2
type FixedList4 = Cons FixedList3
type FixedList5 = Cons FixedList4
type FixedList6 = Cons FixedList5
type FixedList7 = Cons FixedList6
type FixedList8 = Cons FixedList7
type FixedList9 = Cons FixedList8
type FixedList10 = Cons FixedList9
type FixedList11 = Cons FixedList10
type FixedList12 = Cons FixedList11
type FixedList13 = Cons FixedList12
type FixedList14 = Cons FixedList13
type FixedList15 = Cons FixedList14
type FixedList16 = Cons FixedList15
type FixedList17 = Cons FixedList16
type FixedList18 = Cons FixedList17
type FixedList19 = Cons FixedList18
type FixedList20 = Cons FixedList19
type FixedList21 = Cons FixedList20
type FixedList22 = Cons FixedList21
type FixedList23 = Cons FixedList22
type FixedList24 = Cons FixedList23
type FixedList25 = Cons FixedList24
type FixedList26 = Cons FixedList25
type FixedList27 = Cons FixedList26
type FixedList28 = Cons FixedList27
type FixedList29 = Cons FixedList28
type FixedList30 = Cons FixedList29
type FixedList31 = Cons FixedList30
type FixedList32 = Cons FixedList31

