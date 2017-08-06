{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Data.StrictVector
    ( module Data.Vector.Generic
    , Vector
    , null, length, (!), (!?), head, last
    , fromList, fromListN, toList, empty, singleton
    , generate, generateM
    , catMaybes, mapMaybe, lastMay, toHashSet
    , lookAround
    , dropWhileEnd
    , dropWhileLookingAround
    , fromSL
    , imapM, binarySearchL, binarySearchR
    , sort
    , sortBy
    , sortOn
    , groupBy
    , groupOn
    , toSL
    , uncons
    , updateVector, updateVectorWith
    , unfoldrM, unfoldrNM
    , theOnlyOne
    ) where

import Data.Option
import Data.StrictList (SL, toLazyList, fromLazyList)
-- import qualified Cpm.Util.List as L
import qualified Data.StrictVector.Mutable as VM

import Control.DeepSeq (NFData)
import Control.Monad
import Data.Aeson (ToJSON, FromJSON(..))
import Data.Bits (shiftR)
import Data.Data
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(..))
import Data.Vector.Generic hiding
    ( Vector, fromList, fromListN, toList, empty, singleton, null, length ,(!), (!?), head, last
    , imapM, generate, generateM, unfoldrNM, unfoldrM, mapMaybe)
import Prelude hiding
    ( map, drop, dropWhile, concatMap, length, zip3, mapM, null, (++), replicate, head, last)
import Safe.Plus
import Test.QuickCheck (Arbitrary(..))
import Text.Read
import qualified Control.Applicative as A
import qualified Control.Monad.Fail
import qualified Data.HashSet as HashSet
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA (sortBy, sort)
import qualified Data.Vector.Fusion.Bundle.Monadic as VFM
import qualified Data.Vector.Generic as VG
import qualified GHC.Exts as Exts

newtype Vector a = Vector (V.Vector a)
    deriving (Eq, Ord, NFData, ToJSON, Monoid, Foldable, Data, Typeable)

type instance VG.Mutable Vector = VM.MVector

instance VG.Vector Vector a where
    basicUnsafeFreeze (VM.MVector v) = fmap Vector (basicUnsafeFreeze v)
    basicUnsafeThaw (Vector v) = fmap VM.MVector (basicUnsafeThaw v)
    basicLength (Vector v) = basicLength v
    basicUnsafeSlice n m (Vector v) = Vector (basicUnsafeSlice n m v)
    basicUnsafeIndexM (Vector v) = basicUnsafeIndexM v
    basicUnsafeCopy (VM.MVector v1) (Vector v2) = basicUnsafeCopy v1 v2
    elemseq _ = seq

instance Show a => Show (Vector a) where
    showsPrec = VG.showsPrec

instance Read a => Read (Vector a) where
    readPrec = VG.readPrec
    readListPrec = readListPrecDefault

instance (Hashable a) => Hashable (Vector a) where
    hashWithSalt = hashVectorWithSalt

instance Arbitrary a => Arbitrary (Vector a) where
    arbitrary = VG.fromList <$> arbitrary

instance Functor Vector where
    {-# INLINE fmap #-}
    fmap = VG.map

instance Control.Monad.Fail.MonadFail Vector where
    {-# INLINE fail #-}
    fail _ = VG.empty

instance Monad Vector where
    {-# INLINE return #-}
    return = VG.singleton
    {-# INLINE (>>=) #-}
    (>>=) = flip VG.concatMap
    {-# INLINE fail #-}
    fail = safeFail

instance MonadPlus Vector where
    {-# INLINE mzero #-}
    mzero = VG.empty
    {-# INLINE mplus #-}
    mplus = (VG.++)

instance Applicative Vector where
    {-# INLINE pure #-}
    pure = VG.singleton
    {-# INLINE (<*>) #-}
    (<*>) = ap

instance A.Alternative Vector where
    {-# INLINE empty #-}
    empty = VG.empty
    {-# INLINE (<|>) #-}
    (<|>) = (VG.++)

instance Traversable Vector where
    {-# INLINE traverse #-}
    traverse f xs = fromList <$> traverse f (toList xs)
    {-# INLINE mapM #-}
    mapM = VG.mapM
    {-# INLINE sequence #-}
    sequence = VG.sequence

instance Exts.IsList (Vector a) where
    type Item (Vector a) = a
    fromList = fromList
    fromListN = fromListN
    toList = toList

instance FromJSON a => FromJSON (Vector a) where
    parseJSON x = (convert :: V.Vector a -> Vector a) <$> parseJSON x

-- | /O(1)/ Yield the length of the vector.
length :: Vector a -> Int
{-# INLINE length #-}
length = VG.length

-- | /O(1)/ Test whether a vector if empty
null :: Vector a -> Bool
{-# INLINE null #-}
null = VG.null

-- | O(1) Indexing
(!) :: Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (VG.!)

-- | O(1) Safe indexing
(!?) :: Vector a -> Int -> Maybe a
{-# INLINE (!?) #-}
(!?) = (VG.!?)

-- | /O(1)/ First element
head :: Vector a -> a
{-# INLINE head #-}
head = VG.head

-- | /O(1)/ Last element
last :: Vector a -> a
{-# INLINE last #-}
last = VG.last

fromList :: [a] -> Vector a
{-# INLINE fromList #-}
fromList = VG.fromList

fromListN :: Int -> [a] -> Vector a
{-# INLINE fromListN #-}
fromListN = VG.fromListN

toList :: Vector a -> [a]
{-# INLINE toList #-}
toList = VG.toList

singleton :: a -> Vector a
{-# INLINE singleton #-}
singleton = VG.singleton

empty :: Vector a
{-# INLINE empty #-}
empty = VG.empty

generate :: Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate = VG.generate

generateM :: Monad m => Int -> (Int -> m a) -> m (Vector a)
{-# INLINE generateM #-}
generateM = VG.generateM

fromSL :: SL a -> Vector a
{-# INLINE fromSL #-}
fromSL = VG.fromList . toLazyList

toSL :: Vector a -> SL a
{-# INLINE toSL #-}
toSL = fromLazyList . toList

{-# INLINABLE hashVectorWithSalt #-}
hashVectorWithSalt :: Hashable a => Int -> Vector a -> Int
hashVectorWithSalt salt v = foldl' hashWithSalt salt v

{-# INLINABLE mapMaybe #-}
mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
mapMaybe f = catMaybes . map f

{-# INLINABLE catMaybes #-}
catMaybes :: Vector (Maybe a) -> Vector a
catMaybes = concatMap maybeToVector

{-# INLINABLE maybeToVector #-}
maybeToVector :: Maybe a -> Vector a
maybeToVector Nothing = VG.empty
maybeToVector (Just x) = VG.singleton x

{-# INLINABLE lastMay #-}
lastMay :: Vector a -> Maybe a
lastMay vec =
    vec !? ((length vec) - 1)

uncons :: Vector a -> Option (a, Vector a)
uncons v | null v = None
         | otherwise = Some (unsafeHead v, drop 1 v)

-- | Returns `Just` the only element of the vector if there is exactly
-- one element or `Nothing` otherwise.
theOnlyOne :: Vector a -> Maybe a
theOnlyOne xs
    | length xs /= 1 = Nothing
    | otherwise = xs !? 0

{-# INLINABLE lookAround #-}
lookAround :: Vector a -> Vector (Maybe a, a, Maybe a)
lookAround v = zip3 lookBehind v lookAhead
    where
      lookBehind = Nothing `cons` map Just v
      lookAhead = drop 1 (map Just v) `snoc` Nothing

{-# INLINABLE toHashSet #-}
toHashSet :: (Eq a, Hashable a) => Vector a -> HashSet a
toHashSet = foldl' (\set elem -> HashSet.insert elem set) HashSet.empty

dropWhileLookingAround :: (Maybe a -> a -> Maybe a -> Bool) -> Vector a -> Vector a
dropWhileLookingAround f = map (\(_, v, _) -> v) . dropWhile (\(x,y,z) -> f x y z) . lookAround

dropWhileEnd :: (a -> Bool) -> Vector a -> Vector a
dropWhileEnd pred v =
    case pred `fmap` (v !? (length v - 1)) of
      Nothing -> v
      Just False -> v
      Just True ->
          let toDelete count =
                  case pred `fmap` (v !? (length v - count - 1)) of
                    Nothing -> count
                    Just False -> count
                    Just True -> toDelete (count + 1)
          in VG.take (length v - toDelete 1) v

imapM :: Monad m => (Int -> a -> m b) -> Vector a -> m (Vector b)
{-# INLINE imapM #-}
imapM = VG.imapM

binarySearchL :: (e -> Ordering) -> Vector e -> Int
binarySearchL cmp vec = loop 0 (length vec)
 where
   loop !l !u
       | u <= l = l
       | otherwise =
           let k = (u + l) `shiftR` 1
           in case cmp (vec ! k) of
                LT -> loop (k+1) u
                _  -> loop l     k

binarySearchR :: (e -> Ordering) -> Vector e -> Int
binarySearchR cmp vec = loop 0 (length vec)
    where
      loop !l !u
          | u <= l    = l
          | otherwise =
              let k = (u + l) `shiftR` 1
              in case cmp (vec ! k) of
                   GT -> loop l     k
                   _  -> loop (k+1) u

sortOn :: Ord b => (a -> b) -> Vector a -> Vector a
sortOn f = sortBy (\x y -> compare (f x) (f y))

sortBy :: (a -> a -> Ordering) -> Vector a -> Vector a
sortBy comp v =
    modify (VA.sortBy comp) v

sort :: Ord a => Vector a -> Vector a
sort v = modify VA.sort v

groupBy :: (a -> a -> Bool) -> Vector a -> Vector (Vector a)
groupBy eq xs = unfoldrN (VG.length xs) next xs
    where
      next ys
        | VG.null ys = Nothing
        | otherwise =
            let y = VG.unsafeHead ys
                ys' = VG.unsafeTail ys
                (l1,l2) = VG.span (eq y) ys'
            in Just (y `VG.cons` l1, l2)

groupOn :: Eq b => (a -> b) -> Vector a -> Vector (b, (Vector a))
groupOn proj xs = unfoldrN (VG.length xs) next xs
    where
      next ys
        | VG.null ys = Nothing
        | otherwise =
            let y = VG.unsafeHead ys
                z = proj y
                ys' = VG.unsafeTail ys
                (l1,l2) = VG.span (\x -> proj x == z) ys'
            in Just ((z , y `VG.cons` l1), l2)

unfoldrM :: Monad m => (s -> m (Maybe (a, s))) -> s -> m (Vector a)
unfoldrM f s = unstreamM (VFM.unfoldrM f s)

unfoldrNM :: Monad m => Int -> (s -> m (Maybe (a, s))) -> s -> m (Vector a)
unfoldrNM n f s = unstreamM (VFM.unfoldrNM n f s)

-- | Copied from Data.Vector.Generic as it isn't exported there
unstreamM :: (Monad m) => VFM.Bundle m u a -> m (Vector a)
{-# INLINE unstreamM #-}
unstreamM s =
    do xs <- VFM.toList s
       return $ unstream $ VFM.unsafeFromList (VFM.size s) xs

updateVector :: Int -> a -> a -> Vector a -> Vector a
updateVector comp def val vect = updateVectorWith comp def (const val) vect

updateVectorWith :: Int -> a -> (a -> a) -> Vector a -> Vector a
updateVectorWith comp def val vect =
    let vect' =
            if comp >= length vect
            then vect ++ replicate (comp - (length vect) + 1) def
            else vect
    in vect' // [(comp, val (vect' ! comp))]
