{-# OPTIONS_GHC -fno-warn-type-defaults -fno-warn-name-shadowing -F -pgmF htfpp #-}
module StrictVector where

import Data.Option
import Data.StrictVector

import Control.Exception
import Test.Framework
import qualified Data.List as L
import qualified Data.Vector.Generic as VG

prop_groupBy :: [(Int,Int)] -> Bool
prop_groupBy l =
    let eq (_,x) (_,y) = x == y
        res1 = groupBy eq (fromList l)
        res2 = fmap fromList (fromList (L.groupBy eq l))
    in res2 == res1

-- prop_groupOn :: [(Int,Int)] -> Bool
-- prop_groupOn l =
--     let proj = snd
--         res1 = groupOn proj (fromList l)
--         res2 = fmap (second fromList) (fromList (L.groupOn proj l))
--     in res2 == res1

test_lookingAround :: IO ()
test_lookingAround =
    do assertEqual expected1 (f input1)
       assertEqual [] (f [])
       assertEqual [(Nothing,1,Nothing)] (f [1])
       assertEqual [(Nothing,1,Just 2),(Just 1,2,Nothing)] (f [1,2])
    where
      f :: [Int] -> [(Maybe Int, Int, Maybe Int)]
      f list = toList (lookAround (fromList list))
      input1 = [1,2,3]
      expected1 = [ (Nothing, 1, Just 2)
                  , (Just 1, 2, Just 3)
                  , (Just 2, 3, Nothing)
                  ]

test_uncons :: IO ()
test_uncons =
    let atoe :: Int
        atoe = 42
    in do assertEqual None (uncons $ fromList $ L.drop 1 [atoe])
          assertEqual (Some (atoe, fromList [5,2,3]))
                      (uncons $ fromList [atoe,5,2,3])

test_binarySearchL :: IO ()
test_binarySearchL =
    do assertEqual 0 (binarySearchL (flip compare 0) (fromList [0,1,2]))
       assertEqual 1 (binarySearchL (flip compare 1) (fromList [0,1,2]))
       assertEqual 1 (binarySearchL (flip compare 1) (fromList [0,1,1,2]))
       assertEqual 2 (binarySearchL (flip compare 2) (fromList [0,1,2]))
       assertEqual 3 (binarySearchL (flip compare 3) (fromList [0,1,2]))

test_binarySearchR :: IO ()
test_binarySearchR =
    do assertEqual 0 (binarySearchR (flip compare 0) (fromList [1,2,3]))
       assertEqual 1 (binarySearchR (flip compare 0) (fromList [0,1,2]))
       assertEqual 2 (binarySearchR (flip compare 1) (fromList [0,1,2]))
       assertEqual 3 (binarySearchR (flip compare 1) (fromList [0,1,1,2]))
       assertEqual 3 (binarySearchR (flip compare 2) (fromList [0,1,2]))
       assertEqual 3 (binarySearchR (flip compare 3) (fromList [0,1,2]))

test_fromListStrict :: IO ()
test_fromListStrict =
    do let err = ErrorCall "..."
       res <- try $ (VG.fromList [1,2,throw err] :: Vector Int) `seq` return ()
       assertEqual (Left err) res
