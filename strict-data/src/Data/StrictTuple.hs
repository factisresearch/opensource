{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Data.StrictTuple
    ( module Data.Strict.Tuple
    , toLazyTuple
    , fromLazyTuple
    , fst', snd'
    , uncurry'
    , first, second
    , swap, swap'
    , fst3, snd3, thr3
    , fst3', snd3', thr3'
    )
where

import Control.DeepSeq (NFData(..))
import Data.Aeson
import Data.Data
import Data.Hashable
import Data.Strict.Tuple hiding (fst, snd)
import Data.Tuple
import Test.QuickCheck
import qualified Data.Strict.Tuple

deriving instance Typeable Pair
deriving instance (Data a, Data b) => Data (Pair a b)

instance (Hashable a, Hashable b) => Hashable (Pair a b) where
    hashWithSalt s (a :!: b) = hashWithSalt s a `hashWithSalt` b

instance (NFData a, NFData b) => NFData (Pair a b) where
    rnf (a :!: b) = rnf a `seq` rnf b

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
    mempty = mempty :!: mempty
    (a1 :!: b1) `mappend` (a2 :!: b2) = a1 `mappend` a2 :!: b1 `mappend` b2

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = (:!:) <$> arbitrary <*> arbitrary

instance (ToJSON a, ToJSON b) => ToJSON (Pair a b) where
    toJSON = toJSON . toLazyTuple

instance (FromJSON a, FromJSON b) => FromJSON (Pair a b) where
    parseJSON = fmap fromLazyTuple . parseJSON

toLazyTuple :: a :!: b -> (a, b)
toLazyTuple (x :!: y) = (x, y)

fromLazyTuple :: (a, b) -> a :!: b
fromLazyTuple (x, y) = x :!: y

fst' :: Pair a b -> a
fst' = Data.Strict.Tuple.fst

snd' :: Pair a b -> b
snd' = Data.Strict.Tuple.snd

uncurry' :: (a -> b -> c) -> Pair a b -> c
uncurry' = Data.Strict.Tuple.uncurry

first :: (a -> b) -> (a :!: c) -> (b :!: c)
first f (a :!: c) = f a :!: c

second :: (b -> c) -> (a :!: b) -> (a :!: c)
second f (a :!: b) = a :!: f b

swap' :: (a :!: b) -> (b :!: a)
swap' (x :!: y) = y :!: x

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thr3 :: (a, b, c) -> c
thr3 (_, _, x) = x

fst3' :: (a :!: b :!: c) -> a
fst3' (x :!: _ :!: _) = x

snd3' :: (a :!: b :!: c) -> b
snd3' (_ :!: x :!: _) = x

thr3' :: (a :!: b :!: c) -> c
thr3' (_ :!: _ :!: x) = x
