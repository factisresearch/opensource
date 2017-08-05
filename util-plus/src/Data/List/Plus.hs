{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.List.Plus
    ( module Data.List
    , catMaybes
    , chunksOf
    , extractLast
    , groupOn
    , groupOn'
    , groupOnSort
    , groupOnSort'
    , groupBySort
    , groupUnsortedOn
    , headM
    , lastElems
    , lastM
    , lookupM
    , makeMapping
    , mapMaybe
    , maximumM
    , merge
    , middle
    , minimumM
    , monotone
    , nubMerge
    , prefixesAndSuffixes
    , sconcatBy
    , spanTailRec
    , stripSuffix
    , tryStripPrefix
    , ungroupMay
    , withLast
    )
where

import Safe.Plus

import Control.Arrow (first, second)
import Data.Function
import Data.Hashable (Hashable)
import Data.List
import Data.Maybe (mapMaybe, catMaybes)
import qualified Data.Foldable as F
import qualified Data.HashSet as HashSet
import qualified Data.List as L
import qualified Data.List.NonEmpty as NL
import qualified Data.Semigroup as S

-- | Computes the element in the middle of the list
-- If the list has an even number of elements, you will get the element after the middle of the
-- list.
middle :: [a] -> Maybe a
middle xs =
    case drop (length xs `div` 2) xs of
      [] -> Nothing
      x:_ -> Just x

-- O(n) requires a list sorted by the group key
groupOn :: Eq b => (a -> b) -> [a] -> [(b,[a])]
groupOn _  [] =  []
groupOn proj (x:xs) = (x', (x:ys)) : groupOn proj zs
    where
      x' = proj x
      (ys,zs) = span ((==x') . proj) xs

-- First sort and then group the list
groupOnSort :: Ord b => (a -> b) -> [a] -> [(b,[a])]
groupOnSort proj = groupOn proj . L.sortOn proj

-- First sort and then group the list
groupBySort :: Ord b => (b -> b -> Ordering) -> (a -> b) -> [a] -> [(b,[a])]
groupBySort cmp proj = groupOn proj . L.sortBy (\x y -> cmp (proj x) (proj y))

-- O(n^2)
-- Group the list, but don't sort it
groupUnsortedOn :: forall a b. Eq b => (a -> b) -> [a] -> [(b, [a])]
groupUnsortedOn proj =
    L.foldl' (addToGroups) []
    where
      addToGroups :: [(b, [a])] -> a -> [(b, [a])]
      addToGroups m val =
          let key = proj val
          in case break ((== key) . fst) m of
               (_, []) -> m ++ [(key, [val])]
               (p, f:r) -> p ++ [(fst f, snd f ++ [val])] ++ r

-- O(n) requires a list sorted by the group key
groupOn' :: Eq b => (a -> (b,c)) -> [a] -> [(b,[c])]
groupOn' proj = map (second (map (snd . proj))) . groupOn (fst . proj)

groupOnSort' :: Ord b => (a -> (b,c)) -> [a] -> [(b,[c])]
groupOnSort' proj = groupOn' proj . L.sortOn (fst . proj)

sconcatBy :: (Ord b, Foldable f, S.Semigroup s) => (a -> b) -> (a -> s) -> f a -> [(b,s)]
sconcatBy p1 p2 =
    fmap proj
    . NL.groupBy ((==) `on` p1)
    . L.sortOn p1
    . F.toList
    where
      proj gr = (p1 $ NL.head gr, S.sconcat $ NL.map p2 gr)

extractLast :: a -> [a] -> ([a], a)
extractLast x xs =
    case reverse xs of
      [] -> ([], x)
      y:ys -> (x : reverse ys, y)

lastElems :: Int -> [a] -> [a]
lastElems n =
    reverse . take n . reverse

headM :: Monad m => [a] -> m a
headM xs =
    case xs of
      [] -> safeFail "Cannot compute head of empty list"
      x:_ -> return x

lastM :: Monad m => [a] -> m a
lastM xs =
    case xs of
      [] -> safeFail "Cannot compute last of empty list"
      x:[] -> return x
      _:xs -> lastM xs

withLast :: (a -> a) -> [a] -> [a]
withLast _ [] = []
withLast f [x] = [f x]
withLast f (x:xs) = x : withLast f xs

minimumM :: (Monad m, Ord a) => [a] -> m a
minimumM xs =
    case xs of
      [] -> safeFail "Cannot compute minimum of empty list"
      y:ys -> return $ L.foldl' min y ys

maximumM :: (Monad m, Ord a) => [a] -> m a
maximumM xs =
    case xs of
      [] -> safeFail "Connot compute maximum of empty list"
      y:ys -> return $ L.foldl' max y ys

lookupM :: (Eq a, Monad m) => (a -> String) -> a -> [(a,b)] -> m b
lookupM str x xs =
    case lookup x xs of
      Nothing ->
          safeFail ("Lookup of " ++ str x ++ " failed.  Valid values are: "
                    ++ show (map (str . fst) xs))
      Just a ->
          return a

ungroupMay :: [(a,[b])] -> Maybe [(a,b)]
ungroupMay [] = Just []
ungroupMay ((_,[]):_) = Nothing
ungroupMay ((a,bs):rest) =
    do r <- ungroupMay rest
       return (map ((,) a) bs ++ r)

-- Returns false if and only if there are elements in decreasing order in the list.
monotone :: (Ord a) => [a] -> Bool
monotone (x0:x1:xs)
    | x0 <= x1 = monotone (x1:xs)
    | otherwise = False
monotone _ = True

-- makeMapping takes a list of pairs and create a list of key-value pairs
-- such that each key appears only once in the result list. Moreover,
-- the result list contains the pairs in the same order as the input list.
-- Example: [(k1, v1), (k2, v2), (k1, v3)] --> [(k2, v2), (k1, v3)]
makeMapping :: (Eq a, Hashable a) => [(a, b)] -> [(a, b)]
makeMapping l =
    go (reverse l) HashSet.empty []
    where
      go [] _ acc = acc
      go (x@(k, _) : xs) done acc =
          if k `HashSet.member` done
          then go xs done acc
          else go xs (HashSet.insert k done) (x:acc)

-- | Merge two sorted list so that the resulting list is sorted as well.
--   and contains all elements from one of the lists.
--   The length of the resulting list is the sum of the lengths of the given lists.
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x == y = x:y:(merge xs ys)
    | x < y = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)

-- | Merge the two sorted lists and remove all duplicates.
nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge xs ys = nubSorted $ merge xs ys
    where
      nubSorted :: Ord a => [a] -> [a]
      nubSorted = foldr consUniqSorted []

      consUniqSorted :: Ord a => a -> [a] -> [a]
      consUniqSorted x [] = [x]
      consUniqSorted x ys@(y:_) | x == y = ys
                                | otherwise = x:ys

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
    case splitAt n xs of
      ([], _) -> []
      (first, rest) -> first : chunksOf n rest

stripSuffix :: (Eq a) =>  [a] -> [a] -> Maybe [a]
stripSuffix s = (fmap reverse) . L.stripPrefix (reverse s) . reverse

prefixesAndSuffixes :: [a] -> [([a],[a])]
prefixesAndSuffixes a =
    case a of
      [] -> [([], [])]
      (a : r) -> ([], a : r) : map (first (a:)) (prefixesAndSuffixes r)

-- | Strips as elements of a given prefix list as possible.  Stops stripping
-- if the prefix doesn't match anymore or is exhausted and returns the remaining
-- string.
tryStripPrefix :: Eq a => [a] -> [a] -> [a]
tryStripPrefix [] xs = xs
tryStripPrefix _ [] = []
tryStripPrefix (x:xs) yys@(y:ys)
    | x == y = tryStripPrefix xs ys
    | otherwise = yys

spanTailRec :: (a -> Bool) -> [a] -> ([a], [a])
spanTailRec p xs = go ([], xs)
    where go (xs,[]) = (reverse xs, [])
          go (xs,(y:ys))
               | p y = go (y : xs, ys)
               | otherwise = (reverse xs, y:ys)
