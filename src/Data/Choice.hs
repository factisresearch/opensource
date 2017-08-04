{-# LANGUAGE DeriveDataTypeable #-}
module Data.Choice where

import Test.QuickCheck

import Control.DeepSeq (NFData(..))
import Data.Bifunctor
import Data.Data
import Data.Hashable

data Choice a b
    = This !a
    | That !b
    deriving (Eq, Ord, Read, Show, Typeable, Data)

choice :: (a -> c) -> (b -> c) -> Choice a b -> c
choice fa fb = mergeChoice . bimap fa fb

this :: Monad m => Choice a b -> m a
this (This a) = return a
this _ = fail "This is a that"

that :: Monad m => Choice a b -> m b
that (That a) = return a
that _ = fail "That is a this"

these :: [Choice a b] -> [a]
these = concatMap this

those :: [Choice a b] -> [b]
those = concatMap that

eitherToChoice :: Either a b -> Choice a b
eitherToChoice = either This That

mergeChoice :: Choice a a -> a
mergeChoice x =
    case x of
      This y -> y
      That y -> y

instance Bifunctor Choice where
    bimap f g x =
        case x of
          This a -> This (f a)
          That b -> That (g b)

instance (Hashable a, Hashable b) => Hashable (Choice a b) where
    hashWithSalt s (This x) = s `hashWithSalt` (0 :: Int) `hashWithSalt` x
    hashWithSalt s (That x) = s `hashWithSalt` (1 :: Int) `hashWithSalt` x

instance Applicative (Choice e) where
    pure         = That
    This e <*> _ = This e
    That f <*> r = fmap f r

instance Functor (Choice a) where
    fmap = second

instance Monad (Choice e) where
    return = That
    This l >>= _ = This l
    That r >>= k = k r

instance (Arbitrary a, Arbitrary b) => Arbitrary (Choice a b) where
    arbitrary =
        do bool <- arbitrary
           if bool
             then fmap This arbitrary
             else fmap That arbitrary

    shrink (This a) = map This $ shrink a
    shrink (That b) = map That $ shrink b

instance (NFData a, NFData b) => NFData (Choice a b) where
    rnf (This x)  = rnf x
    rnf (That y) = rnf y

