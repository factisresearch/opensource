{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Data.Map.OrderedSpec
    ( htf_thisModulesTests
    )
where

import Data.Map.Ordered

import Data.Traversable
import Prelude hiding (map, lookup, null, filter)
import System.IO.Unsafe (unsafePerformIO)
import Test.Framework
import qualified Control.Exception as E

newtype OSMapInt = OSMapInt (OSMap Int Int)
    deriving (Eq, Show)

bottom :: a
bottom = undefined

bottomInt :: Int
bottomInt = bottom

-- | A modified variant of 'isBottomTimeOut' that lives in the 'IO' monad.
-- (Taken from ChasingBottoms)
isBottom :: a -> Bool
isBottom f =
    unsafePerformIO $!
    E.evaluate (f `seq` False) `E.catches`
    [ E.Handler (\(_ :: E.ArrayException)   -> return True)
    , E.Handler (\(_ :: E.ErrorCall)        -> return True)
    , E.Handler (\(_ :: E.NoMethodError)    -> return True)
    , E.Handler (\(_ :: E.NonTermination)   -> return True)
    , E.Handler (\(_ :: E.PatternMatchFail) -> return True)
    , E.Handler (\(_ :: E.RecConError)      -> return True)
    , E.Handler (\(_ :: E.RecSelError)      -> return True)
    , E.Handler (\(_ :: E.RecUpdError)      -> return True)
    ]

keyValueByIndex :: Int -> OSMap Int Int -> (Int, Int)
keyValueByIndex i m =
    let n = size m
    in toList m !! (i `mod` n)

valueByIndex :: Int -> OSMap Int Int -> Int
valueByIndex i m = snd (keyValueByIndex i m)

keyByIndex :: Int -> OSMap Int Int -> Int
keyByIndex i m = fst (keyValueByIndex i m)

instance Arbitrary OSMapInt where
    arbitrary =
        do l <- arbitrary
           return $ OSMapInt $ fromList l

prop_insertStrictKey :: OSMapInt -> Int -> Bool
prop_insertStrictKey (OSMapInt m) v =
    isBottom (insert bottom v m)

prop_insertStrictValue :: OSMapInt -> Int -> Bool
prop_insertStrictValue (OSMapInt m) k =
    isBottom (insert k bottom m)

prop_deleteStrict :: OSMapInt -> Bool
prop_deleteStrict (OSMapInt m) = isBottom (delete bottom m)

prop_mapStrict :: OSMapInt -> Int -> Property
prop_mapStrict (OSMapInt m) i =
    not (null m) ==>
    isBottom $ map (\x -> if x == value then bottom else x) m
    where
      value = valueByIndex i m

prop_singletonStrictKey :: Int -> Bool
prop_singletonStrictKey v =
    isBottom $ singleton bottomInt v

prop_singletonStrictValue :: Int -> Bool
prop_singletonStrictValue k =
    isBottom $ singleton k bottom

prop_insertWithStrictKey :: OSMapInt -> Int -> Bool
prop_insertWithStrictKey (OSMapInt m) v =
    isBottom $ insertWith (\_ _ -> 0) bottom v m

prop_insertWithStrictValue1 :: OSMapInt -> Int -> Bool
prop_insertWithStrictValue1 (OSMapInt m) k =
    isBottom $ insertWith (\_ _ -> 0) k bottom m

prop_insertWithStrictValue2 :: OSMapInt -> Int -> Int -> Property
prop_insertWithStrictValue2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ insertWith (\_ old -> if old == value then bottom else v) key v m
    where
      (key, value) = keyValueByIndex i m

prop_unionStrictLeft :: OSMapInt -> Bool
prop_unionStrictLeft (OSMapInt m) =
    isBottom $ union bottom m

prop_unionStrictRight :: OSMapInt -> Bool
prop_unionStrictRight (OSMapInt m) =
    isBottom $ union m bottom

prop_differenceStrictLeft :: OSMapInt -> Bool
prop_differenceStrictLeft (OSMapInt m) =
    isBottom $ difference bottom m

prop_differenceStrictRight :: OSMapInt -> Property
prop_differenceStrictRight (OSMapInt m) =
    not (null m) ==>
    isBottom $ difference m bottom

prop_intersectionStrictLeft :: OSMapInt -> Bool
prop_intersectionStrictLeft (OSMapInt m) =
    isBottom $ intersection bottom m

prop_intersectionStrictRight :: OSMapInt -> Property
prop_intersectionStrictRight (OSMapInt m) =
    not (null m) ==>
    isBottom $ intersection m bottom

prop_insertLookupWithKeyStrictKey :: OSMapInt -> Int -> Bool
prop_insertLookupWithKeyStrictKey (OSMapInt m) v =
    isBottom $ snd $ insertLookupWithKey (\_ _ _ -> 0) bottom v m

prop_insertLookupWithKeyStrictValue1 :: OSMapInt -> Int -> Bool
prop_insertLookupWithKeyStrictValue1 (OSMapInt m) k =
    isBottom $ snd $ insertLookupWithKey (\_ _ _ -> 0) k bottom m

prop_insertLookupWithKeyStrictValue2 :: OSMapInt -> Int -> Int -> Property
prop_insertLookupWithKeyStrictValue2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ snd $ insertLookupWithKey (\_ _ old -> if old == value then bottom else v) key v m
    where
      (key, value) = keyValueByIndex i m

prop_updateLookupWithKeyStrictKey :: OSMapInt -> Maybe Int -> Bool
prop_updateLookupWithKeyStrictKey (OSMapInt m) v =
    isBottom $ snd $ updateLookupWithKey (\_ _ -> v) bottom m

prop_updateLookupWithKeyStrictValue1 :: OSMapInt -> Maybe Int -> Int -> Property
prop_updateLookupWithKeyStrictValue1 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ snd $ updateLookupWithKey (\_ old -> if old == value then bottom else v) key m
    where
      (key, value) = keyValueByIndex i m

prop_updateLookupWithKeyStrictValue2 :: OSMapInt -> Maybe Int -> Int -> Property
prop_updateLookupWithKeyStrictValue2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ snd $ updateLookupWithKey (\_ old -> if old == value then Just bottom else v) key m
    where
      (key, value) = keyValueByIndex i m

prop_deleteLookupStrict :: OSMapInt -> Property
prop_deleteLookupStrict (OSMapInt m) =
    not (null m) ==>
    isBottom $ snd $ deleteLookup bottom m

prop_alterStrictKey :: OSMapInt -> Bool
prop_alterStrictKey (OSMapInt m) =
    isBottom $ alter id bottom m

prop_alterStrictFun1 :: OSMapInt -> Int -> Property
prop_alterStrictFun1 (OSMapInt m) i =
    not (null m) ==>
    isBottom $ alter (\_ -> Just bottomInt) key m
    where
      key = keyByIndex i m

prop_alterStrictFun2 :: OSMapInt -> Int -> Property
prop_alterStrictFun2 (OSMapInt m) i =
    not (null m) ==>
    isBottom $ alter (\_ -> bottom) key m
    where
      key = keyByIndex i m

prop_differenceWithStrictLeft :: OSMapInt -> Maybe Int -> Bool
prop_differenceWithStrictLeft (OSMapInt m) v =
    isBottom $ differenceWith (\_ _ -> v) bottom m

prop_differenceWithStrictRight :: OSMapInt -> Maybe Int -> Property
prop_differenceWithStrictRight (OSMapInt m) v =
    not (null m) ==>
    isBottom $ differenceWith (\_ _ -> v) m bottom

prop_differenceWithStrictFun1 :: OSMapInt -> Int -> Int -> Property
prop_differenceWithStrictFun1 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ differenceWith (\_ _ -> Just bottom) m (insert key v m)
    where
      key = keyByIndex i m

prop_differenceWithStrictFun2 :: OSMapInt -> Int -> Int -> Property
prop_differenceWithStrictFun2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ differenceWith (\_ _ -> bottom) m (insert key v m)
    where
      key = keyByIndex i m

prop_intersectionWithStrictFun2 :: OSMapInt -> Int -> Int -> Property
prop_intersectionWithStrictFun2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ intersectionWith (\_ _ -> bottom) m (insert key v m)
    where
      key = keyByIndex i m

prop_updateWithKeyStrictKey :: OSMapInt -> Maybe Int -> Bool
prop_updateWithKeyStrictKey (OSMapInt m) v =
    isBottom $ updateWithKey (\_ _ -> v) bottom m

prop_updateWithKeyStrictValue1 :: OSMapInt -> Maybe Int -> Int -> Property
prop_updateWithKeyStrictValue1 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ updateWithKey (\_ old -> if old == value then bottom else v) key m
    where
      (key, value) = keyValueByIndex i m

prop_updateWithKeyStrictValue2 :: OSMapInt -> Maybe Int -> Int -> Property
prop_updateWithKeyStrictValue2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ updateWithKey (\_ old -> if old == value then Just bottom else v) key m
    where
      (key, value) = keyValueByIndex i m

prop_insertWithKeyStrictKey :: OSMapInt -> Int -> Bool
prop_insertWithKeyStrictKey (OSMapInt m) v =
    isBottom $ insertWithKey (\_ _ _ -> 0) bottom v m

prop_insertWithKeyStrictValue1 :: OSMapInt -> Int -> Bool
prop_insertWithKeyStrictValue1 (OSMapInt m) k =
    isBottom $ insertWithKey (\_ _ _ -> 0) k bottom m

prop_insertWithKeyStrictValue2 :: OSMapInt -> Int -> Int -> Property
prop_insertWithKeyStrictValue2 (OSMapInt m) v i =
    not (null m) ==>
    isBottom $ insertWithKey (\_ _ old -> if old == value then bottom else v) key v m
    where
      (key, value) = keyValueByIndex i m

prop_mapKeysStrict :: OSMapInt -> Int -> Property
prop_mapKeysStrict (OSMapInt m) i =
    not (null m) ==>
    isBottom $ mapKeys (\k -> if k == key then bottom else k) m
    where
      key = keyByIndex i m

prop_fmapStrict :: OSMapInt -> Int -> Property
prop_fmapStrict (OSMapInt m) i =
    not (null m) ==>
    isBottom $ fmap (\x -> if x == value then bottom else x) m
    where
      value = valueByIndex i m

prop_traverseStrict :: OSMapInt -> Int -> Property
prop_traverseStrict (OSMapInt m) i =
    not (null m) ==>
    isBottom $ fmapDefault (\x -> if x == value then bottom else x) m
    where
      value = valueByIndex i m
