{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Data.List.PlusSpec
    ( htf_thisModulesTests
    )
where

import Data.List.Plus
import Test.Framework
import Test.QuickCheck.Function
import qualified Data.List.Plus as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

prop_spanTailRec :: Fun Int Bool -> [Int] -> Bool
prop_spanTailRec (Fun _ f) lst = L.span f lst == spanTailRec f lst

test_middle :: IO ()
test_middle =
    do assertNothing (middle ([] :: [Int]))
       assertEqual (Just 1) (middle [1])
       assertEqual (Just 2) (middle [1,2])
       assertEqual (Just 2) (middle [1,2,3])
       assertEqual (Just 3) (middle [1,2,3,4])

test_tryStripPrefix :: IO ()
test_tryStripPrefix =
    do assertEqual "foobar" (tryStripPrefix "" "foobar")
       assertEqual "" (tryStripPrefix "foobar" "")
       assertEqual "z" (tryStripPrefix "foobar" "foobaz")
       assertEqual "baz" (tryStripPrefix "foobar" "foobarbaz")
       assertEqual "" (tryStripPrefix "foobarbaz" "foobar")

test_groupUnsortedOn :: IO ()
test_groupUnsortedOn =
    do assertEqual [] $ groupUnsortedOn id ([] :: [()])
       assertEqual [((), [()])] $ groupUnsortedOn id [()]
       assertEqual [((), [(), ()])] $ groupUnsortedOn id [(), ()]
       assertEqual [(1, [1]), (0, [2])] $ groupUnsortedOn (`mod` 2) [1, 2]
       assertEqual [(0, [2]), (1, [1])] $ groupUnsortedOn (`mod` 2) [2, 1]
       assertEqual [(1, [1, 3]), (0, [2, 4])] $ groupUnsortedOn (`mod` 2) [1, 2, 3, 4]
       assertEqual [(1, [1, 3]), (0, [2, 4])] $ groupUnsortedOn (`mod` 2) [1, 2, 4, 3]
       assertEqual [(0, [2, 4]), (1, [1, 3])] $ groupUnsortedOn (`mod` 2) [2, 1, 4, 3]

test_groupOnSort :: IO ()
test_groupOnSort =
    do assertEqual [] $ groupOnSort id ([] :: [()])
       assertEqual [((), [()])] $ groupOnSort id [()]
       assertEqual [((), [(), ()])] $ groupOnSort id [(), ()]
       assertEqual [(0, [2]), (1, [1])] $ groupOnSort (`mod` 2) [1, 2]
       assertEqual [(0, [2]), (1, [1])] $ groupOnSort (`mod` 2) [2, 1]
       assertEqual [(0, [2, 4]), (1, [1, 3])] $ groupOnSort (`mod` 2) [1, 2, 3, 4]
       assertEqual [(0, [2, 4]), (1, [1, 3])] $ groupOnSort (`mod` 2) [1, 2, 4, 3]
       assertEqual [(0, [2, 4]), (1, [1, 3])] $ groupOnSort (`mod` 2) [2, 1, 4, 3]

test_headM :: IO ()
test_headM =
    do assertEqual Nothing $ headM ([] :: [()])
       assertEqual (Just ()) $ headM [()]
       assertEqual (Just 1) $ headM [1,2]

test_lastM :: IO ()
test_lastM =
    do assertEqual Nothing $ lastM ([] :: [()])
       assertEqual (Just ()) $ lastM [()]
       assertEqual (Just 2) $ lastM [1,2]

test_minimumM :: IO ()
test_minimumM =
    do assertEqual Nothing $ minimumM ([] :: [Int])
       assertEqual (Just 1) $ minimumM [1]
       assertEqual (Just 1) $ minimumM [1,2]
       assertEqual (Just 1) $ minimumM [2,1]
       assertEqual (Just 1) $ minimumM [2,1,2]

test_maximumM :: IO ()
test_maximumM =
    do assertEqual Nothing $ maximumM ([] :: [Int])
       assertEqual (Just 2) $ maximumM [2]
       assertEqual (Just 2) $ maximumM [2,1]
       assertEqual (Just 2) $ maximumM [1,2]
       assertEqual (Just 2) $ maximumM [1,2,1]

test_merge :: IO ()
test_merge =
    do assertEqual [1, 2, 3, 4, 5] $ nubMerge as bs
       assertEqual [] $ nubMerge empty empty
       assertEqual [1] $ nubMerge one empty
       assertEqual [1] $ nubMerge empty one
       assertEqual [1, 2, 3] $ nubMerge cs cs
       assertEqual [1, 2, 3, 4, 5, 6] $ nubMerge cs ds
       assertEqual [1, 2, 3, 4, 5, 6] $ nubMerge es fs
       assertEqual [1] $ nubMerge two one
       assertEqual [1] $ nubMerge one two
       assertEqual [1] $ nubMerge three empty
       assertEqual [1] $ nubMerge empty three

       assertEqual [1, 2, 2, 3, 4, 4, 5] $ merge as bs
       assertEqual [] $ merge empty empty
       assertEqual [1] $ merge one empty
       assertEqual [1] $ merge empty one
       assertEqual [1, 1, 2, 2, 3, 3] $ merge cs cs
       assertEqual [1, 2, 3, 4, 5, 6] $ merge cs ds
       assertEqual [1, 2, 3, 4, 5, 6] $ merge es fs
       assertEqual [1, 1, 1] $ merge two one
       assertEqual [1, 1, 1] $ merge one two
       assertEqual [1, 1, 1] $ merge three empty
       assertEqual [1, 1, 1] $ merge empty three
    where
      empty, one, two, three, as, bs, cs, ds, es, fs :: [Int]
      as = [1, 2, 3, 4]
      bs = [2, 4, 5]
      cs = [1, 2, 3]
      ds = [4, 5, 6]
      es = [1, 3, 5]
      fs = [2, 4, 6]
      empty = []
      one = [1]
      two = [1, 1]
      three = [1, 1, 1]


test_ungroup :: IO ()
test_ungroup =
    assertEqual (Just [("a","1"),("a","2"),("a","3"),("b","4")])
              $ ungroupMay [("a",["1","2","3"]),("b",["4"])]

test_ungroupGroup :: IO ()
test_ungroupGroup =
    do let list = [("x","1"),("y","1"),("y","3"),("y","1")]
       assertEqual (Just list) (ungroupMay $ groupOn' id list)

test_stripSuffix :: IO ()
test_stripSuffix =
    do assertEqual (Just "foo") $ stripSuffix "bar" "foobar"
       assertEqual (Just "") $ stripSuffix "bar" "bar"
       assertEqual Nothing $ stripSuffix "bar" "foobars"

test_monotone :: IO ()
test_monotone =
    do assertEqual True $ monotone [1,2,3]
       assertEqual False $ monotone [-1,0,3,2]
       assertEqual True $ monotone [1]
       assertEqual True $ monotone ([] :: [Int])

test_lastElems :: IO ()
test_lastElems =
    do assertEqual ([]::[Int]) (lastElems 100 [])
       assertEqual [1,2,3] (lastElems 5 [1,2,3])
       assertEqual [1,2,3] (lastElems 3 [1,2,3])
       assertEqual [2,3] (lastElems 2 [1,2,3])


prop_lastElems :: [Int] -> Int -> Bool
prop_lastElems l n =
    lastElems n l `L.isSuffixOf` l

test_makeMapping :: IO ()
test_makeMapping =
    do assertEqual [] (makeMapping ([]::[(Int, String)]))
       let l = [(1::Int, "one"), (2, "two")] in assertEqual l (makeMapping l)
       assertEqual [(2::Int, "two"), (1, "three")]
                   (makeMapping [(1, "one"), (2, "two"), (1, "three")])
       assertEqual [(1::Int, "x")] (makeMapping [(1,"x"),(1,"x")])
       let l2 = [(-2,""),(-2,"a"),(-2,"")]
       assertBool $ checkOrder (makeMapping l2) l2

test_chunksOf :: IO ()
test_chunksOf =
    do assertEqual [] (chunksOf 1 ([] :: [Int]))
       assertEqual [] (chunksOf 0 [1])
       assertEqual [[1], [2], [3], [4]] (chunksOf 1 [1, 2, 3, 4])
       assertEqual [[1, 2], [3, 4]] (chunksOf 2 [1, 2, 3, 4])
       assertEqual [[1, 2], [3]] (chunksOf 2 [1, 2, 3])
       assertEqual [[1, 2, 3]] (chunksOf 3 [1, 2, 3])

test_prefixesAndSuffixes :: IO ()
test_prefixesAndSuffixes =
    do assertEqual (prefixesAndSuffixes "") [("","")]
       assertEqual (prefixesAndSuffixes "Hallo")
           [("","Hallo"), ("H","allo"),("Ha","llo"),("Hal","lo"),("Hall","o"),("Hallo","")]

prop_makeMappingConcat :: [(Int, String)] -> Bool
prop_makeMappingConcat l =
    makeMapping l == makeMapping (l ++ l)

prop_makeMappingKeysUnique :: [(Int, String)] -> Bool
prop_makeMappingKeysUnique l =
    length (map fst (makeMapping l)) == Set.size (Set.fromList (map fst l))

prop_makeMappingKeyValsOk :: [(Int, String)] -> Bool
prop_makeMappingKeyValsOk l =
    Map.fromList (makeMapping l) == Map.fromList l

prop_makeMappingOrderingOk :: [(Int, String)] -> Bool
prop_makeMappingOrderingOk l =
    checkOrder (makeMapping l) l

checkOrder :: [(Int, String)] -> [(Int, String)] -> Bool
checkOrder [] [] = True
checkOrder (x:xs) (y:ys)
    | x == y = checkOrder xs (dropWhile ((fst x ==) . fst) ys)
    | otherwise = checkOrder (x:xs) ys
checkOrder _ _ = False

test_withLast :: IO ()
test_withLast =
    do assertEqual [] (withLast not [])
       assertEqual [False] (withLast not [True])
       assertEqual [True,False] (withLast not [True,True])
