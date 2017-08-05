{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-type-defaults #-}
module StrictList (htf_thisModulesTests) where

import Data.Option
import Data.StrictList
import Data.StrictTuple

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
import Test.Framework
import qualified Prelude as P

test_nub :: IO ()
test_nub =
    do assertEqual (sl [1, 5, 2] :: SL Int) $ nub (sl [1, 5, 1, 2, 5, 2])

test_unzip :: IO ()
test_unzip =
    do assertEqual ((1:!2:!Nil) :!: ('a':!'b':!Nil)) (unzip ((1 :!: 'a') :! (2 :!: 'b') :! Nil))
       assertEqual ((1:!2:!Nil) :!: ('a':!'b':!Nil)) (unzipL [(1:!:'a'),(2:!:'b')])
       assertEqual ((1:!2:!Nil) :!: ('a':!'b':!Nil)) (unzipLL [(1,'a'),(2,'b')])

test_dropWhileEnd :: IO ()
test_dropWhileEnd =
    do assertEqual Nil $ dropWhileEnd (<= 1) $ Nil
       assertEqual Nil $ dropWhileEnd (<= 1) $ 1 :! Nil
       assertEqual (1 :! 2 :! Nil) $ dropWhileEnd (<= 1) $ 1 :! 2 :! 1 :! Nil


prop_partition :: [Int] -> Bool
prop_partition l =
    let me :: ([Int], [Int])
        me = (\(xs, ys) -> (toLazyList xs, toLazyList ys)) $ partition even $ fromLazyList l
    in (P.filter even l, P.filter odd l) == me

test_insert :: IO ()
test_insert =
    do assertEqual (1 :! Nil) (insert 1 Nil)
       assertEqual (1 :! 2 :! Nil) (insert 1 (2 :! Nil))
       assertEqual (1 :! 2 :! Nil) (insert 2 (1 :! Nil))
       assertEqual (1 :! 2 :! 3 :! Nil) (insert 2 (1 :! 3 :! Nil))
       assertEqual (1 :! 2 :! 3 :! Nil) (insert 3 (1 :! 2 :! Nil))
       assertEqual (1 :! 2 :! 2 :! Nil) (insert 2 (1 :! 2 :! Nil))
       assertEqual (2 :! 3 :! 1 :! Nil) (insert 2 (3 :! 1 :! Nil))

test_lookup :: IO ()
test_lookup =
    do assertEqual None (lookup True (mk []))
       assertEqual (Some 'a') (lookup True (mk [(True, 'a')]))
       assertEqual (Some 'a') (lookup True (mk [(False, 'b'),(True, 'a')]))
       assertEqual (Some 'a') (lookup True (mk [(False, 'b'),(True, 'a'),(False, 'c')]))
       assertEqual (Some 'a') (lookup True (mk [(False, 'b'),(False, 'c'),(True, 'a')]))
       assertEqual None (lookup True (mk [(False, 'b')]))
       assertEqual None (lookup True (mk [(False, 'a'), (False, 'b')]))
    where
      mk :: [(Bool,Char)] -> StrictList (Bool :!: Char)
      mk = fromLazyList . fmap fromLazyTuple

prop_take :: Int -> [Int] -> Bool
prop_take l lst =
    let me :: [Int]
        me = toLazyList $ take l (fromLazyList lst)
    in P.take l lst == me

-- test_sort :: IO ()
-- test_sort =
--     do let list = fromLazyList [1..133]
--        list' <- Cpm.Util.Random.shuffle (toLazyList list)
--        assertEqual list (sort (fromLazyList list'))

test_headOpt :: IO ()
test_headOpt =
    do assertEqual (Some "B") $ headOpt $ fromLazyList ["B","C"]
       assertEqual None $ headOpt (Nil :: StrictList ())

test_lastOpt :: IO ()
test_lastOpt =
    do assertEqual (Some 5) $ lastOpt $ fromLazyList [2,4,5]
       assertEqual (Some 5) $ lastOpt $ fromLazyList [5]
       assertEqual None $ lastOpt $ (Nil :: StrictList ())

test_findIndex :: IO ()
test_findIndex =
    do assertEqual None $ findIndex (== "A") $ fromLazyList ["B","C"]
       assertEqual None $ findIndex (== "A") Nil
       assertEqual (Some 1) $ findIndex (== "C") $ fromLazyList ["B","C","D"]
       assertEqual (Some 0) $ findIndex (/= "C") $ fromLazyList ["B","C","D"]

test_reverse :: IO ()
test_reverse =
    do assertEqual (fromLazyList ["D","C","B"]) (reverse $ fromLazyList ["B","C","D"])
       assertEqual (Nil :: StrictList ()) $ reverse Nil

test_replicate :: IO ()
test_replicate =
  do assertEqual (replicate 3 'a') (fromLazyList (P.replicate 3 'a'))
     assertEqual (replicate 3 'b') ('b' :! 'b' :! 'b' :! Nil)
     assertEqual (replicate 0 'c') Nil
     assertEqual (replicate 0 'd') (fromLazyList (P.replicate 0 'e'))

test_dropWhile :: IO ()
test_dropWhile =
    do assertEqual (fromLazyList [5]) $ dropWhile even $ fromLazyList [2,4,5]
       assertEqual (fromLazyList [2,4,5]) $ dropWhile odd $ fromLazyList [2,4,5]
       assertEqual Nil $ dropWhile (>=1) $ fromLazyList [2,4,5]

test_stripPrefix :: IO ()
test_stripPrefix =
    do assertEqual (Just Nil) $ stripPrefix Nil (Nil :: SL Int)
       assertEqual (Just $ 1 :! Nil) $ stripPrefix Nil (1 :! Nil)
       assertEqual (Just Nil) $ stripPrefix (1 :! Nil) (1 :! Nil)
       assertEqual (Just $ 3 :! Nil) $ stripPrefix (1 :! 2 :! Nil) (1 :! 2 :! 3 :! Nil)
       assertEqual Nothing $ stripPrefix (1 :! Nil) Nil

test_stripSuffix :: IO ()
test_stripSuffix =
    do assertEqual (Just Nil) $ stripSuffix Nil (Nil :: SL Int)
       assertEqual (Just $ 1 :! Nil) $ stripSuffix Nil (1 :! Nil)
       assertEqual (Just Nil) $ stripSuffix (1 :! Nil) (1 :! Nil)
       assertEqual (Just $ 1 :! Nil) $ stripSuffix (2 :! 3 :! Nil) (1 :! 2 :! 3 :! Nil)
       assertEqual Nothing $ stripSuffix (1 :! Nil) Nil

test_deleteIdx :: IO ()
test_deleteIdx =
    do assertEqual (1 :! Nil) $ deleteIdx (-5) (1 :! Nil)
       assertEqual (1 :! 3 :! Nil) $ deleteIdx 1 (1 :! 2 :! 3 :! Nil)
       assertEqual Nil $ deleteIdx 0 ("B" :! Nil)
       assertEqual (fromLazyList ["a","B","C","D","E"]) $
                   deleteIdx 5 (fromLazyList ["a","B","C","D","E","Q"])
       assertEqual (fromLazyList [1,3,2,4]) $
                    deleteIdx 4 (fromLazyList [1,3,2,4])

test_atIdx :: IO ()
test_atIdx =
    do assertEqual (Some 1) $ atIdx 0 (1 :! Nil)
       assertEqual None $ atIdx 6 (1 :! 2 :! 3 :! Nil)
       assertEqual None $ atIdx (-3) ("B" :! Nil)
       assertEqual (Some "a") $ atIdx 5 (fromLazyList ["g","q","s","u","xc","a"])

test_snoc :: IO ()
test_snoc =
    do assertEqual (True :! Nil) (snoc Nil True)
       assertEqual (False :! True :! Nil) (snoc (False :! Nil) True)

test_transpose :: IO ()
test_transpose =
    do assertEqual (f [[1,4],[2,5],[3,6]]) (transpose (f [[1,2,3],[4,5,6]]))
       assertEqual (f [[1,2,3],[4,5],[6]]) (transpose (f [[1,4],[2],[],[3,5,6]]))
    where
      f = fmap sl . sl

prop_difference :: SL Int -> SL Int -> Bool
prop_difference xs ys = (xs +!+ ys) \!\ xs == ys

test_delete :: IO ()
test_delete =
    do assertEqual (sl "bnana") (delete 'a' (sl "banana"))
       assertEqual Nil (delete 'a' Nil)

test_merge :: IO ()
test_merge =
    assertEqual (sl "abcdef" :: SL Char) (merge (sl "acde") (sl "abdf"))
