{-# LANGUAGE DeriveDataTypeable #-}
module Data.Choice where

import Control.DeepSeq (NFData(..))
import Data.Bifunctor
import Data.Data
import Data.Hashable
import Test.QuickCheck

-- | 'Choice' is a version of 'Either' that is strict on both the 'Left' side (called 'This')
-- and the 'Right' side (called 'That').
--
-- Note: 'Choice' is not used as an error monad. Use 'Data.Fail.Fail' for that.
data Choice a b
    = This !a
    | That !b
    deriving (Eq, Ord, Read, Show, Typeable, Data)

-- | 'Choice''s version of 'either'
choice :: (a -> c) -> (b -> c) -> Choice a b -> c
choice fa fb = mergeChoice . bimap fa fb

-- |
-- >>> this (This "foo") :: Maybe String
-- Just "foo"
--
-- >>> this (That "bar") :: Maybe String
-- Nothing
this :: Monad m => Choice a b -> m a
this (This a) = return a
this _ = fail "This is a that"

-- |
-- >>> that (This "foo") :: Maybe String
-- Nothing
--
-- >>> that (That "bar") :: Maybe String
-- Just "bar"
that :: Monad m => Choice a b -> m b
that (That a) = return a
that _ = fail "That is a this"

-- |
-- >>> these [This "foo", This "bar", That "baz", This "quux"]
-- ["foo","bar","quux"]
these :: [Choice a b] -> [a]
these = concatMap this

-- |
-- >>> those [This "foo", This "bar", That "baz", This "quux"]
-- ["baz"]
those :: [Choice a b] -> [b]
those = concatMap that

-- |
-- >>> eitherToChoice (Left 1)
-- This 1
--
-- >>> eitherToChoice (Right 5)
-- That 5
eitherToChoice :: Either a b -> Choice a b
eitherToChoice = either This That

-- |
-- >>> mergeChoice (This 5 :: Choice Int Int)
-- 5
--
-- >>> mergeChoice (That 'c' :: Choice Char Char)
-- 'c'
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

