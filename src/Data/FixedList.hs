{-#LANGUAGE MultiParamTypeClasses,
            FunctionalDependencies,
            FlexibleInstances,
            UndecidableInstances #-}
{- |
A fixed length list library.

The length of a list is encoded into it's type in a natural way. This allows you to do things like specify that two list parameters have the same type, which also forces them to have the same length. This can be a handy property.
It's not as flexible as the standard haskell list, but the added type safety is sometimes worth it.

The entire library is Haskell98 except for the 'Append' typeclass. (which could be easily removed if needed).

Most of your usual list functions ('foldr', 'fmap', 'sum', 'sequence', etc..) are accessed via the 'Functor', 'Applicative', 'Foldable', and 'Traversable' instances.

The Equivalent of zipWith can be had via the Applicative instance:

> zipWith f xs ys = pure f <*> xs <*> ys

Also, 'sequenceA' transposes a FixedList of FixedLists.

The monad instance is also interesting. 
return fills the list with the given element (remember that list size is dependent on the type)
You can think of bind as operating like this:

> m >>= k = diagonal $ fmap k m 

This takes the FixedList m and maps k accross it, (which must return a FixedList) which results in a FixedList of FixedLists the diagonal of which is returned.
The actually implementation is more elegant, but works essentialy the same.

This also means that 'join' gets the diagonal of a FixedList of FixedLists.

You can construct FixedLists like so:

> t1 :: Cons (Cons (Cons Nil)) Integer
> t1 = 1 :. 3 :. 5 :. Nil
> 
> t2 :: Cons (Cons (Cons Nil)) Integer  -- type signature needed! and must be correct!
> t2 = fromFoldable' [4, 1, 0]
> 
> t3 :: Cons (Cons (Cons Nil)) (Cons (Cons (Cons Nil)) Integer)
> t3 = t1 :. t1 :. t2 :. Nil
> 
> -- get the sum of the diagonal of the transpose of t3
> test :: Cons (Cons (Cons Nil)) Integer
> test = sum $ join $ sequenceA $ t3


On a side note...
I think that if Haskell supported infinite types my 'Append' typeclass would only have one parameter and I wouldn't need all those nasty extensions:

I think I could also implement direct, typesafe, versions of 'last', 'init', 'reverse' and 'length' that don't depend on 'Foldable'. *sigh*  Maybe Haskell will one day support such things.

-}
module Data.FixedList ( Cons(..)
                      , Nil(..)
                      , FixedList
                      , Append (..)
                      , reverse
                      , length
                      , last
                      , init
                      , unit
                      , fromFoldable
                      , fromFoldable'
                      ) where

import Control.Applicative
import Control.Monad hiding (sequence) --hidden for haddock
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Data.Maybe

import Prelude hiding (head, tail, (++), foldr, sum, sequence, reverse, length, last, init)
import qualified Prelude

data Cons f a = (:.) { head :: a, 
                       tail :: (f a)
         }  deriving (Eq, Ord)
data Nil a = Nil
  deriving (Eq, Ord)

infixr 5 :.
infixr 5  ++

instance (Foldable f, Show a) => Show (Cons f a) where
  show x = "|" Prelude.++ show (toList x) Prelude.++ "|"
instance Show (Nil a) where
  show Nil = "|[]|"

-- | just a restriction to make the fromFoldable's in reverse, and init safe
class (Applicative f, Traversable f, Foldable f, Monad f) => FixedList f
instance (FixedList f) => FixedList (Cons f)
instance FixedList Nil

-- The only very bad ugly, everything else is haskell98
class Append f g h | f g -> h, f h -> g where
  (++) :: f a -> g a -> h a
instance (Append f b c) => Append (Cons f) b (Cons c) where
  (x :. xs) ++ ys = x :. (xs ++ ys)
instance Append Nil a a where
  Nil ++ ys = ys


reverse :: (FixedList t) => t a -> t a
reverse xs = fromFoldable' $ Prelude.reverse $ toList xs

length :: (Foldable t) => t a -> Int
length xs = Prelude.length $ toList xs

-- | Returns the last element of the list
last :: (Foldable t) => t a -> a
last xs = Prelude.last $ toList xs

-- | Returns all but the last element of the list
init :: (FixedList f) => Cons f a -> f a
init xs = fromFoldable' $ Prelude.init $ toList xs

-- | Constructs a FixedList containing a single element
unit :: a -> Cons Nil a
unit a = a :. Nil

-- | Converts any Foldable to any Applicative Traversable.
--   However, this will only do what you want if 'pure' gives you the shape of structure you are expecting.
fromFoldable :: (Foldable f, Applicative g, Traversable g) => f a -> Maybe (g a)
fromFoldable t = sequenceA $ snd $ mapAccumL f (toList t) (pure undefined)
  where
    f [] _ = ([], Nothing)
    f (x:xs) _ = (xs, Just x)

-- | This can crash if the foldable is smaller than the new structure
fromFoldable' :: (Foldable f, Applicative g, Traversable g) => f a -> g a
fromFoldable' a = fromJust $ fromFoldable a

instance (Functor f) => Functor (Cons f) where
  fmap f (a :. b) = (f a) :. (fmap f b)

instance (Foldable f) => Foldable (Cons f) where
  foldMap f (a :. b) = f a `mappend` foldMap f b

instance (Traversable f) => Traversable (Cons f) where
  traverse f (a :. b) = (:.) <$> f a <*> traverse f b

instance (Monad f) => Monad (Cons f) where
  return x = x :. (return x)
  (a :. b) >>= k = (head $ k a) :. (b >>= (tail . k))

instance (Monad f, Applicative f) => Applicative (Cons f) where
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
  Nil >>= _ = Nil -- should I define this as _ >>= _ = Nil ? wouldn't have the right behavior with bottom

instance Applicative Nil where
  pure = return
  (<*>) = ap


