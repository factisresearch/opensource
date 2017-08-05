{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Data.StrictList
    ( StrictList(..)
    , SL
    , (+!+)
    , (\!\)
    , all
    , any
    , atIdx
    , break
    , catMaybes
    , catOptions
    , catOptionsL
    , concat
    , concatSL
    , concatMap
    , concatMapSL
    , concatMapM
    , concatText
    , delete
    , deleteBy
    , deleteIdx
    , drop
    , dropWhile
    , dropWhileEnd
    , elem
    , filter
    , find
    , findIndex
    , fromLazyList, toLazyList
    , groupBy
    , headM
    , headOpt
    , insert
    , insertBy
    , intercalateString
    , intercalateText
    , intersperse
    , lastM
    , lastOpt
    , length
    , ll
    , lookup
    , lookupM
    , lookupM'
    , lookupM''
    , map
    , mapM
    , mapM_
    , mapMaybe
    , mapOption
    , maximumM
    , maybeToStrictList
    , mconcatSL
    , notElem
    , nub
    , null
    , optionToStrictList
    , partition
    , replicate
    , reverse
    , singleton
    , sl
    , snoc
    , merge
    , mergeBy
    , sort
    , sortBy
    , sortOn
    , span
    , stripPrefix
    , stripSuffix
    , tailOpt
    , take
    , takeWhile
    , transpose
    , unzip
    , unzipL
    , unzipLL
    , zip
    , zipLL
    , zipLS
    , zipSL
    , zipWith
    , zipWithLS
    , zipWithSL
    )
where

import Data.Option hiding (catOptions, mapOption)
import Data.StrictList.Types
import Data.StrictTuple

import Data.Hashable
import Data.Ord (comparing)
import Prelude hiding
    ( (!!)
    , all
    , any
    , break
    , concat
    , concatMap
    , drop
    , dropWhile
    , elem
    , filter
    , length
    , lookup
    , map
    , mapM
    , mapM_
    , notElem
    , null
    , replicate
    , reverse
    , span
    , take
    , takeWhile
    , unzip
    , zip
    , zipWith
    )
import qualified Data.Foldable as F
import qualified Data.HashSet as HashSet
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Traversable as Tr
import qualified Prelude as P

sl :: [a] -> SL a
sl = fromLazyList

ll :: SL a -> [a]
ll = toLazyList

null :: StrictList a -> Bool
null Nil = True
null _ = False

headOpt :: StrictList a -> Option a
headOpt Nil = None
headOpt (x :! _) = Some x

headM :: Monad m => StrictList a -> m a
headM xxs =
    case xxs of
      Nil -> fail "headM of empty strict list."
      (x :! _) -> return x

tailOpt :: StrictList a -> Option (StrictList a)
tailOpt Nil = None
tailOpt (_ :! xs) = Some xs

lastOpt :: StrictList a -> Option a
lastOpt = lastM

lastM :: Monad m => StrictList a -> m a
lastM xxs =
    case xxs of
      Nil -> fail "No last element in strict list."
      (x :! Nil) -> return x
      (_ :! xs) -> lastM xs

optionToStrictList :: Option a -> StrictList a
optionToStrictList None = Nil
optionToStrictList (Some x) = x :! Nil

maybeToStrictList :: Maybe a -> StrictList a
maybeToStrictList Nothing = Nil
maybeToStrictList (Just x) = x :! Nil

takeWhile :: (a -> Bool) -> StrictList a -> StrictList a
takeWhile _ Nil = Nil
takeWhile p (x :! xs)
    | p x = x :! takeWhile p xs
    | otherwise = Nil

drop :: Int -> StrictList a -> StrictList a
drop _ Nil = Nil
drop n xss@(_ :! xs)
    | n <= 0 = xss
    | otherwise = drop (n - 1) xs

deleteIdx :: Int -> StrictList a -> StrictList a
deleteIdx _ Nil = Nil
deleteIdx idx lst@(x :! xs) =
    case idx of
      0 ->
          case xs of
            Nil -> Nil
            l -> l
      i ->
          if i < 0
          then lst
          else x :! deleteIdx (i-1) xs

-- | 'delete' @x@ removes the first occurrence of @x@ from its list argument.
-- NOTE: Implementation copied from Data.List.
delete :: (Eq a) => a -> SL a -> SL a
delete = deleteBy (==)

-- | The 'deleteBy' function behaves like 'delete', but takes a
-- user-supplied equality predicate.
-- NOTE: Implementation copied from Data.List.
deleteBy :: (a -> a -> Bool) -> a -> SL a -> SL a
deleteBy eq x yys =
    case yys of
      Nil -> Nil
      (y:!ys) -> if x `eq` y then ys else y :! deleteBy eq x ys

atIdx :: Int -> StrictList a -> Option a
atIdx _ Nil = None
atIdx idx (p :! ps) =
    case idx of
      0 -> Some p
      i ->
          if i < 0
          then None
          else atIdx (i-1) ps

dropWhile :: (a -> Bool) -> StrictList a -> StrictList a
dropWhile _ Nil = Nil
dropWhile p (x :! xs)
    | p x = dropWhile p xs
    | otherwise = x :! xs

findIndex :: (a -> Bool) -> StrictList a -> Option Int
findIndex _ Nil = None
findIndex p (x :! xs)
    | p x = Some 0
    | otherwise = (+1) <$> findIndex p xs

map :: (a -> b) -> StrictList a -> StrictList b
map = fmap

mapM :: Monad m => (a -> m b) -> StrictList a -> m (StrictList b)
mapM = Tr.mapM

mapM_ :: Monad m => (a -> m b) -> StrictList a -> m ()
mapM_ = F.mapM_

filter :: (a -> Bool) -> StrictList a -> StrictList a
filter _ Nil = Nil
filter pred (x :! xs)
    | pred x = x :! filter pred xs
    | otherwise = filter pred xs

catMaybes :: StrictList (Maybe a) -> StrictList a
catMaybes xs =
    case xs of
      Nil -> Nil
      (Nothing :! xs) -> catMaybes xs
      (Just x :! xs ) -> x :! catMaybes xs

mapMaybe :: (a -> Maybe b) -> StrictList a -> StrictList b
mapMaybe f = catMaybes . map f

mapOption :: (a -> Option b) -> StrictList a -> StrictList b
mapOption f = catOptions . map f

catOptions :: StrictList (Option a) -> StrictList a
catOptions xs =
    case xs of
      Nil -> Nil
      (None :! xs) -> catOptions xs
      (Some x :! xs) -> x :! catOptions xs

catOptionsL :: [Option a] -> StrictList a
catOptionsL xs =
    case xs of
      [] -> Nil
      (None : xs) -> catOptionsL xs
      (Some x : xs) -> x :! catOptionsL xs

-- |
-- >>> take 3 (sl [1, 2, 3, 4, 5, 6, 7])
-- [1,2,3]
take :: Int -> StrictList a -> StrictList a
take _ Nil = Nil
take n _ | n <= 0 = Nil
take n (x :! xs) = x :! take (n-1) xs

sort :: (Ord a) => StrictList a -> StrictList a
sort = sortBy compare

-- |
-- >>> sortOn snd (sl [("foo", 10), ("bar", 1), ("baz", 100)])
-- [("bar",1),("foo",10),("baz",100)]
sortOn :: (Ord b) => (a -> b) -> StrictList a -> StrictList a
sortOn f =
    map snd
    . sortBy (comparing fst)
    . map (\x -> let y = f x
                 in y `seq` (y,x))

replicate :: Integral i => i -> a -> StrictList a
replicate i a =
    case i of
      0 -> Nil
      n -> a :! replicate (n-1) a

-- |
-- prop> reverse (reverse xs) == xs
reverse :: StrictList a -> StrictList a
reverse l =  rev l Nil
  where
    rev xxs !a =
        case xxs of
          Nil -> a
          (x :! xs) -> rev xs (x :! a)

merge :: Ord a => StrictList a -> StrictList a -> StrictList a
merge = mergeBy compare

mergeBy :: (a -> a -> Ordering) -> StrictList a -> StrictList a -> StrictList a
mergeBy cmp = go
    where
      go as@(a :! as') bs@(b :! bs') =
          case cmp a b of
            LT -> a :! go as' bs
            GT -> b :! go as bs'
            EQ -> a :! go as' bs'
      go Nil bs = bs
      go as Nil = as

sortBy :: (a -> a -> Ordering) -> StrictList a -> StrictList a
sortBy cmp = mergeAll . sequences
  where
    sequences (a :! (b :! xs))
      | a `cmp` b == GT = descending b (a :! Nil) xs
      | otherwise       = ascending  b (a :!) xs
    sequences xs = xs :! Nil
    descending a as (b :! bs)
      | a `cmp` b == GT = descending b (a :! as) bs
    descending a as bs  = (a :! as) :! sequences bs
    ascending a as (b:!bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a :! ys)) bs
    ascending a as bs   = as (a :! Nil) :! sequences bs
    mergeAll (x :! Nil) = x
    mergeAll xs  = mergeAll (mergePairs xs)
    mergePairs (a :! (b :! xs)) = (merge a b) :! mergePairs xs
    mergePairs xs       = xs
    merge as@(a :! as') bs@(b :! bs')
      | a `cmp` b == GT = b :! merge as  bs'
      | otherwise       = a :! merge as' bs
    merge Nil bs         = bs
    merge as Nil         = as

span :: (a -> Bool) -> StrictList a -> (StrictList a, StrictList a)
span _ Nil =  (Nil, Nil)
span p xs@(x :! xs')
    | p x = let (ys, zs) = span p xs' in (x :! ys, zs)
    | otherwise = (Nil, xs)

break :: (a -> Bool) -> StrictList a -> (StrictList a, StrictList a)
break p =  span (not . p)

concat :: F.Foldable t => t (StrictList a) -> StrictList a
concat = F.fold

concatSL :: SL (SL a) -> SL a
concatSL = concat

concatMap :: F.Foldable t => (a -> StrictList b) -> t a -> StrictList b
concatMap = F.foldMap

concatMapSL :: (a -> StrictList b) -> SL a -> StrictList b
concatMapSL = concatMap

concatMapM  :: (Monad m) => (a -> m (SL b)) -> SL a -> m (SL b)
concatMapM f xs = concat <$> mapM f xs

any :: (a -> Bool) -> StrictList a -> Bool
any = F.any

all :: (a -> Bool) -> StrictList a -> Bool
all = F.all

elem :: Eq a => a -> StrictList a -> Bool
elem = F.elem

notElem :: Eq a => a -> StrictList a -> Bool
notElem = F.notElem

find :: (a -> Bool) -> StrictList a -> Maybe a
find = F.find

zip :: StrictList a -> StrictList b -> StrictList (a :!: b)
zip Nil _ = Nil
zip _ Nil = Nil
zip (x :! xs) (y :! ys) = (x :!: y) :! (zip xs ys)

zipSL :: StrictList a -> [b] -> StrictList (a :!: b)
zipSL Nil _ = Nil
zipSL _ [] = Nil
zipSL (x :! xs) (y : ys) = (x :!: y) :! (zipSL xs ys)

zipLS :: [a] -> StrictList b -> StrictList (a :!: b)
zipLS [] _ = Nil
zipLS _ Nil = Nil
zipLS (x : xs) (y :! ys) = (x :!: y) :! (zipLS xs ys)

zipLL :: [a] -> [b] -> StrictList (a :!: b)
zipLL [] _ = Nil
zipLL _ [] = Nil
zipLL (x : xs) (y : ys) = (x :!: y) :! (zipLL xs ys)

zipWith :: (a->b->c) -> SL a-> SL b -> SL c
zipWith f (a:!as) (b:!bs) = f a b :! zipWith f as bs
zipWith _ _ _ = Nil

-- zipWith - left list is lazy, right list is strict
zipWithLS :: (a->b->c) -> [a]-> SL b -> SL c
zipWithLS f (a:as) (b:!bs) = f a b :! zipWithLS f as bs
zipWithLS _ _ _ = Nil

-- zipWith - left list is strict, right list is lazy
zipWithSL :: (a->b->c) -> SL a-> [b] -> SL c
zipWithSL f (a:!as) (b:bs) = f a b :! zipWithSL f as bs
zipWithSL _ _ _ = Nil

concatText :: StrictList T.Text -> T.Text
concatText = T.concat . toLazyList

concatString :: StrictList String -> String
concatString = P.concat . toLazyList

groupBy :: (a -> a -> Bool) -> StrictList a -> StrictList (StrictList a)
groupBy _ Nil =  Nil
groupBy eq (x:!xs) =  (x:!ys) :! groupBy eq zs
    where (ys,zs) = span (eq x) xs

intersperse :: a -> StrictList a -> StrictList a
intersperse y =
    F.foldr' prepend Nil
    where
      prepend x xs =
          case xs of
            Nil -> x :! Nil
            _ -> x :! y :! xs

intercalateText :: T.Text -> StrictList T.Text -> T.Text
intercalateText t =
    concatText . intersperse t

intercalateString :: String -> SL String -> String
intercalateString s =
    concatString . intersperse s

singleton :: a -> StrictList a
singleton x =
    x :! Nil

lookupM' :: (Monad m, Eq a) => (a -> String) -> a -> StrictList (a :!: b) -> m b
lookupM' showA x = fmap snd' . lookupM'' showA (Just . fst') x

-- | @lookupM'' showKey getKey getValue key list@ searches for @key@ in
-- @list@ using @getKey@ as the key extraction function and @showKey@ to print
-- all available keys when no match is found.
lookupM'' :: (Monad m, Eq k) => (k -> String) -> (a -> Maybe k) -> k -> StrictList a -> m a
lookupM'' showKey getKey wantedK list = loop list
    where
      loop xxs =
          case xxs of
            Nil ->
                let keys = ll $ mapMaybe getKey list
                    keyCount = P.length keys
                    count = P.length list
                in fail $
                   "Didn't find " ++ showKey wantedK ++ " in the list with these keys ["
                   ++ L.intercalate ", " (fmap showKey keys) ++ "]. " ++
                   if keyCount == count
                      then ""
                      else ("Only " ++ show keyCount ++ "/" ++ show count ++ " entries had a key.")
            (x@(getKey -> Just curK) :! xs)
                | wantedK == curK -> return x
                | otherwise -> loop xs
            _ :! xs -> loop xs

lookupM :: (Monad m, Show a, Eq a) => a -> StrictList (a :!: b) -> m b
lookupM = lookupM' show

lookup :: Eq a => a -> StrictList (a :!: b) -> Option b
lookup = lookupM' (const "fail in Option is None")

insert :: Ord a => a -> SL a -> SL a
insert = insertBy compare

insertBy :: (a -> a -> Ordering) -> a -> SL a -> SL a
insertBy cmp x yss =
    case yss of
      Nil -> x :! Nil
      y:!ys ->
          case cmp x y of
            GT -> y :! insertBy cmp x ys
            _ -> x :! yss

partition :: (a -> Bool) -> SL a -> (SL a, SL a)
partition p =
    F.foldr (select p) (Nil, Nil)
    where
        select :: (a -> Bool) -> a -> (SL a, SL a) -> (SL a, SL a)
        select p x (ts, fs)
            | p x       = (x :! ts, fs)
            | otherwise = (ts, x :! fs)

dropWhileEnd :: (a -> Bool) -> SL a -> SL a
dropWhileEnd p =
    F.foldr (\x xs -> if p x && null xs then Nil else x :! xs) Nil

maximumM :: (Ord a, Monad m) => SL a -> m a
maximumM xxs =
    case xxs of
      Nil -> fail "Empty list doesn't have a maximum."
      (x :! xs) -> return $! loop x xs
    where
      loop x yys =
          case yys of
            Nil -> x
            (y :! ys) -> loop (max x y) ys

mconcatSL :: Monoid a => SL a -> a
mconcatSL = F.foldr mappend mempty

stripPrefix :: Eq a => SL a -> SL a -> Maybe (SL a)
stripPrefix Nil ys = Just ys
stripPrefix (x :! xs) (y :! ys) | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

stripSuffix :: Eq a => SL a -> SL a -> Maybe (SL a)
stripSuffix suffix xs = fmap reverse (stripPrefix (reverse suffix) (reverse xs))

-- unzip strict list of strict tuples to strict lists of strict tuples
unzip :: SL (a :!: b) -> (SL a :!: SL b)
unzip =  F.foldr (\(a :!: b) (as :!: bs) -> (a:!as :!: b:!bs)) (Nil :!: Nil)

-- unzip lazy list of lazy tuples to strict lists of strict tuples
unzipLL :: [(a,b)] -> (SL a :!: SL b)
unzipLL =  F.foldr (\(a,b) (as :!: bs) -> (a:!as :!: b:!bs)) (Nil :!: Nil)

-- unzip lazy list of strict tuples to strict lists of strict tuples
unzipL :: [(a:!:b)] -> (SL a :!: SL b)
unzipL =  F.foldr (\(a:!:b) (as :!: bs) -> (a:!as :!: b:!bs)) (Nil :!: Nil)

-- | Appends an element to the end of this list.  This is really inefficient because the
-- whole list needs to be copied.  Use at your own risk.
snoc :: SL a -> a -> SL a
snoc xxs y =
    case xxs of
      Nil -> y :! Nil
      x :! xs -> x :! snoc xs y

-- NOTE: copied from Data.List
transpose :: SL (SL a) -> SL (SL a)
transpose xxs =
    case xxs of
      Nil -> Nil
      (Nil :! ys) -> transpose ys
      ((x:!xs) :! xss) -> (x :! [h | (h:!_) <- xss]) :! transpose (xs :! [ t | (_:!t) <- xss])

(\!\) :: (Eq a) => SL a -> SL a -> SL a
(\!\) = F.foldl (flip delete)

nub :: (Eq a, Hashable a) => SL a -> SL a
nub = nub' HashSet.empty
    where
      nub' acc xxs =
          case xxs of
            Nil -> Nil
            x :! xs
                | x `HashSet.member` acc -> nub' acc xs
                | otherwise -> x :! nub' (HashSet.insert x acc) xs
