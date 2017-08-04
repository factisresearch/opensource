{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Data.StrictList.Types where

import Data.Aeson
import Test.QuickCheck

import Control.Applicative
import Control.DeepSeq
import Control.Monad hiding (forM_, mapM, mapM_)
import Data.Data
import Data.Hashable (Hashable)
import Data.Monoid
import Data.Traversable hiding (mapM)
import GHC.Exts
import GHC.Generics (Generic)
import Prelude (Eq(..), Ord(..), Show(..), (.), (+), Bool)
import Text.Read
import qualified Control.Monad.Fail
import qualified Data.Foldable as F

type SL = StrictList

data StrictList a
    = Nil
    | !a :! !(StrictList a)
    deriving (Eq,Ord,Functor,F.Foldable,Traversable,Typeable,Generic,Data)

instance Read a => Read (StrictList a) where
    readPrec = fromLazyList <$> readPrec

instance Show a => Show (StrictList a) where
    showsPrec n xs = showsPrec n (toLazyList xs)

infixr 5  +!+
infixr 5  :!

(+!+) :: StrictList a -> StrictList a -> StrictList a
(+!+) Nil ys = ys
(+!+) (x :! xs) ys = x :! (xs +!+ ys)

instance Applicative StrictList where
    pure = return
    (<*>) = ap

instance Alternative StrictList where
    empty = Nil
    (<|>) = (+!+)

instance Control.Monad.Fail.MonadFail StrictList where
    fail _ = Nil

instance Monad StrictList where
    return = (:! Nil)
    (>>=) xs f = F.asum (fmap f xs)
    fail = Control.Monad.Fail.fail

instance Arbitrary a => Arbitrary (StrictList a) where
    arbitrary =
        do v <- arbitrary
           return (fromLazyList v)

instance Monoid (StrictList a) where
    mempty = Nil
    mappend = (+!+)

instance Hashable a => Hashable (StrictList a)

instance ToJSON a => ToJSON (StrictList a) where
    toJSON = toJSON . toLazyList

instance FromJSON a => FromJSON (StrictList a) where
    parseJSON = fmap fromLazyList . parseJSON

instance NFData a => NFData (StrictList a)

instance IsList (StrictList a) where
    type Item (StrictList a) = a
    fromList = fromLazyList
    toList = toLazyList

length :: StrictList a -> Int
length xxs =
    case xxs of
      _ :! xs -> 1 + length xs
      Nil -> 0

fromLazyList :: [a] -> StrictList a
fromLazyList [] = Nil
fromLazyList (x : xs) = x :! fromLazyList xs

toLazyList :: StrictList a -> [a]
toLazyList Nil = []
toLazyList (x :! xs) = x : toLazyList xs

prop_StrictListOrd :: [Int] -> [Int] -> Bool
prop_StrictListOrd l1 l2 =
    let l1' = fromLazyList l1
        l2' = fromLazyList l2
    in compare l1 l2 == compare l1' l2'
