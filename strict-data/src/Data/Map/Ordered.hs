{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE BangPatterns #-}
-- | An ordered, strict map.
--
-- One might think that `Data.Map.Strict` already provides such a data type. This is not correct.
-- `Data.Map.Lazy` and `Data.Map.Strict` use the same, non-strict `Map` datatype.
-- `Data.Map.Strict` just provides functions that evaluate the value argument before inserting it
-- in the Map. The problem is that the typeclass instances of the shared `Map` datatype use the
-- non-strict functions.
module Data.Map.Ordered
    ( OSMap, Map, empty, lookup, insert, delete, fromList, fromListWith, toList, map, mapMaybe
    , lookupLT, lookupGT, lookupLE, lookupGE, lookupM, elemAt
    , singleton, insertWith
    , member, elems, unionWith, difference, union, findWithDefault, size, null, isSubmapOf, unions
    , intersection, foldrWithKey, foldlWithKey, filter, filterWithKey
    , keys, toDescList, updateLookupWithKey
    , deleteLookup, insertLookupWithKey, adjust, assocs, insertWith'
    , alter, differenceWith, updateWithKey, update, mapKeys, insertWithKey, insertWithKey'
    , keysSet
    , maxView, maxViewWithKey, minView, minViewWithKey
    , intersectionWith, fromDistinctAscList
    , toDataMap, fromDataMap, hasKey, hasValue
    )
where

import Control.Arrow (second)
import Control.DeepSeq (NFData(..))
import Data.Coerce
import Data.Data
import Data.Hashable
import Data.List (foldl')
import Data.Maybe (isJust)
import Prelude hiding (map, lookup, null, filter)
import Test.QuickCheck
import qualified Data.Map.Strict as DM
import qualified Data.Set as Set

type Map = OSMap

newtype OSMap k v = OSMap { unOSMap :: DM.Map k v }
    deriving (Eq, Ord, Read, Show, Foldable, NFData, Data)

instance (Hashable k, Hashable v) => Hashable (OSMap k v) where
    hashWithSalt = foldlWithKey updateHash
        where
          updateHash salt k v = hashWithSalt salt k `hashWithSalt` v

instance Functor (OSMap k) where
    {-# INLINE fmap #-}
    fmap = map

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (OSMap k v) where
    arbitrary = OSMap <$> arbitrary
    shrink = coerce . shrink . unOSMap

instance Traversable (OSMap k) where
    {-# INLINE traverse #-}
    traverse f (OSMap m) =
        fromDataMap <$> DM.traverseWithKey (\_ x -> let y = f x in y) m

instance (Ord k) => Monoid (OSMap k v) where
    mempty = empty
    mconcat = unions
    mappend = union

{-# INLINE fromDataMap #-}
fromDataMap :: DM.Map k v -> OSMap k v
fromDataMap dm = DM.foldr (\a b -> a `seq` b) () dm `seq` OSMap dm

{-# INLINE toDataMap #-}
toDataMap :: OSMap k v -> DM.Map k v
toDataMap = unOSMap

{-# INLINE empty #-}
empty :: OSMap k v
empty = OSMap DM.empty

{-# INLINE member #-}
member :: (Ord k) => k -> OSMap k v -> Bool
member k (OSMap m) = DM.member k m

{-# INLINE lookup #-}
lookup :: (Ord k) => k -> OSMap k v -> Maybe v
lookup k (OSMap hm) = DM.lookup k hm

{-# INLINE lookupM #-}
lookupM :: (Show k, Ord k, Monad m) => k -> OSMap k v -> m v
lookupM k (OSMap hm) =
    case DM.lookup k hm of
      Nothing -> fail ("Could not find " ++ show k ++ " in Map.")
      Just x -> pure x

{-# INLINE lookupLT #-}
lookupLT :: (Ord k) => k -> OSMap k v -> Maybe (k, v)
lookupLT k (OSMap hm) = DM.lookupLT k hm

{-# INLINE lookupGT #-}
lookupGT :: (Ord k) => k -> OSMap k v -> Maybe (k, v)
lookupGT k (OSMap hm) = DM.lookupGT k hm

{-# INLINE lookupLE #-}
lookupLE :: (Ord k) => k -> OSMap k v -> Maybe (k, v)
lookupLE k (OSMap hm) = DM.lookupLE k hm

{-# INLINE lookupGE #-}
lookupGE :: (Ord k) => k -> OSMap k v -> Maybe (k, v)
lookupGE k (OSMap hm) = DM.lookupGE k hm

{-# INLINE insert #-}
insert :: (Ord k) => k -> v -> OSMap k v -> OSMap k v
insert k v (OSMap hm) = OSMap (DM.insert k v hm)

{-# INLINE delete #-}
delete :: (Ord k) => k -> OSMap k v -> OSMap k v
delete k (OSMap hm) = OSMap (DM.delete k hm)

{-# INLINE fromList #-}
fromList :: (Ord k) => [(k,v)] -> OSMap k v
fromList = OSMap . DM.fromList

{-# INLINE fromListWith #-}
fromListWith :: (Ord k) => (v -> v -> v) -> [(k,v)] -> OSMap k v
fromListWith f kvs = OSMap $ DM.fromListWith f kvs

{-# INLINE toList #-}
toList :: OSMap k v -> [(k, v)]
toList (OSMap hm) = DM.toList hm

{-# INLINE toDescList #-}
toDescList :: OSMap k v -> [(k, v)]
toDescList (OSMap hm) = DM.toDescList hm

{-# INLINE map #-}
map :: (v -> v') -> OSMap k v -> OSMap k v'
map f (OSMap m) = OSMap (DM.map f m)

{-# INLINE mapMaybe #-}
mapMaybe :: (v -> Maybe v') -> OSMap k v -> OSMap k v'
mapMaybe f (OSMap m) = OSMap (DM.mapMaybe f m)

{-# INLINE singleton #-}
singleton :: k -> v -> OSMap k v
singleton k v = OSMap (DM.singleton k v)

{-# INLINE insertWith #-}
insertWith :: (Ord k) => (v -> v -> v) -> k -> v -> OSMap k v -> OSMap k v
insertWith f k !v (OSMap hm) = OSMap (DM.insertWith f k v hm)

{-# INLINE elems #-}
elems :: OSMap k v -> [v]
elems = DM.elems . unOSMap

{-# INLINE keys #-}
keys :: OSMap k v -> [k]
keys = DM.keys . unOSMap

{-# INLINE keysSet #-}
keysSet :: OSMap k v -> Set.Set k
keysSet = DM.keysSet . unOSMap

{-# INLINE union #-}
union :: Ord k => OSMap k v -> OSMap k v -> OSMap k v
union (OSMap m1) (OSMap m2) = OSMap (DM.union m1 m2)

{-# INLINABLE unions #-}
unions :: Ord k => [OSMap k v] -> OSMap k v
unions ts = foldl' union empty ts

{-# INLINE unionWith #-}
unionWith :: Ord k => (v -> v -> v) -> OSMap k v -> OSMap k v -> OSMap k v
unionWith f (OSMap m1) (OSMap m2) = OSMap (DM.unionWith f m1 m2)

{-# INLINE difference #-}
difference :: (Ord k) => OSMap k v -> OSMap k w -> OSMap k v
difference (OSMap m1) (OSMap m2) = OSMap (DM.difference m1 m2)

{-# INLINE intersection #-}
intersection :: (Ord k) => OSMap k v -> OSMap k w -> OSMap k v
intersection (OSMap m1) (OSMap m2) = OSMap (DM.intersection m1 m2)

{-# INLINE findWithDefault #-}
findWithDefault :: (Ord k) => a -> k -> OSMap k a -> a
findWithDefault def k (OSMap m) = DM.findWithDefault def k m

{-# INLINE elemAt #-}
elemAt :: Int -> OSMap k a -> (k, a)
elemAt n (OSMap m) = DM.elemAt n m

{-# INLINE size #-}
size :: OSMap k v -> Int
size = DM.size . unOSMap

{-# INLINE null #-}
null :: OSMap k v -> Bool
null = DM.null . unOSMap

{-# INLINE isSubmapOf #-}
isSubmapOf :: (Ord k, Eq a) => OSMap k a -> OSMap k a -> Bool
isSubmapOf (OSMap a) (OSMap b) = DM.isSubmapOf a b

{-# INLINE foldrWithKey #-}
foldrWithKey :: (k -> v -> a -> a) -> a -> OSMap k v -> a
foldrWithKey f a (OSMap hm) = DM.foldrWithKey f a hm

{-# INLINE foldlWithKey #-}
foldlWithKey :: (a -> k -> v -> a) -> a -> OSMap k v -> a
foldlWithKey f a (OSMap hm) = DM.foldlWithKey f a hm

{-# INLINE filter #-}
filter :: (v -> Bool) -> OSMap k v -> OSMap k v
filter f (OSMap m) = OSMap (DM.filter f m)

{-# INLINE filterWithKey #-}
filterWithKey :: (k -> v -> Bool) -> OSMap k v -> OSMap k v
filterWithKey f (OSMap m) = OSMap (DM.filterWithKey f m)

{-# INLINE updateLookupWithKey #-}
updateLookupWithKey :: Ord k => (k -> a -> Maybe a) -> k -> OSMap k a -> (Maybe a, OSMap k a)
updateLookupWithKey f k (OSMap curMap) =
    let (mv, newMap) = DM.updateLookupWithKey f k curMap
    in (mv, OSMap newMap)

{-# INLINE deleteLookup #-}
deleteLookup :: Ord k => k -> OSMap k v -> (Maybe v, OSMap k v)
deleteLookup = updateLookupWithKey (\_k _v -> Nothing)

{-# INLINE insertLookupWithKey #-}
insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> OSMap k a -> (Maybe a, OSMap k a)
insertLookupWithKey f k !newV (OSMap curM) =
    let (mOldV, newM) = DM.insertLookupWithKey f k newV curM
    in (mOldV, OSMap newM)

{-# INLINE adjust #-}
adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f k = OSMap . DM.adjust f k . unOSMap

{-# INLINE assocs #-}
assocs :: OSMap k a -> [(k, a)]
assocs = DM.assocs . unOSMap

{-# INLINE insertWith' #-}
insertWith' :: Ord k => (a -> a -> a) -> k -> a -> OSMap k a -> OSMap k a
insertWith' f k !v (OSMap dm) = OSMap (DM.insertWith f k v dm)

{-# INLINE alter #-}
alter :: Ord k => (Maybe a -> Maybe a) -> k -> OSMap k a -> OSMap k a
alter f k (OSMap dm) = OSMap (DM.alter f k dm)

{-# INLINE differenceWith #-}
differenceWith :: Ord k => (a -> b -> Maybe a) -> OSMap k a -> OSMap k b -> OSMap k a
differenceWith f (OSMap dmA) (OSMap dmB) = OSMap (DM.differenceWith f dmA dmB)

{-# INLINE updateWithKey #-}
updateWithKey :: Ord k => (k -> a -> Maybe a) -> k -> OSMap k a -> OSMap k a
updateWithKey f k (OSMap dm) = OSMap (DM.updateWithKey f k dm)

{-# INLINE update #-}
update :: Ord k => (a -> Maybe a) -> k -> OSMap k a -> OSMap k a
update f k (OSMap dm) = OSMap (DM.update f k dm)

{-# INLINE insertWithKey #-}
insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey f k !v (OSMap dm) = OSMap (DM.insertWithKey f k v dm)

{-# INLINE insertWithKey' #-}
insertWithKey' :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey' = insertWithKey

{-# INLINE mapKeys #-}
mapKeys :: Ord k2 => (k1 -> k2) -> OSMap k1 a -> OSMap k2 a
mapKeys f (OSMap dm) = OSMap (DM.mapKeys f dm)

{-# INLINE maxView #-}
maxView :: OSMap k a -> Maybe (a, OSMap k a)
maxView (OSMap m) = fmap (second OSMap) (DM.maxView m)

{-# INLINE maxViewWithKey #-}
maxViewWithKey :: OSMap k a -> Maybe ((k, a), OSMap k a)
maxViewWithKey (OSMap m) = fmap (second OSMap) (DM.maxViewWithKey m)

{-# INLINE minView #-}
minView :: OSMap k a -> Maybe (a, OSMap k a)
minView (OSMap m) = fmap (second OSMap) (DM.minView m)

{-# INLINE minViewWithKey #-}
minViewWithKey :: OSMap k a -> Maybe ((k, a), OSMap k a)
minViewWithKey = fmap (second OSMap) . DM.minViewWithKey . unOSMap

{-# INLINE intersectionWith #-}
intersectionWith :: Ord k => (a -> b -> c) -> OSMap k a -> OSMap k b -> OSMap k c
intersectionWith f (OSMap l) (OSMap r) =
    OSMap (DM.intersectionWith f l r)

{-# INLINE fromDistinctAscList #-}
fromDistinctAscList :: [(k,v)] -> OSMap k v
fromDistinctAscList = OSMap . DM.fromDistinctAscList

hasValue :: Int -> OSMap Int Int -> Bool
hasValue v m = any (\(_, x) -> x == v) (toList m)

hasKey :: Int -> OSMap Int Int -> Bool
hasKey k m = isJust (lookup k m)
