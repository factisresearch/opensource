{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Option where

import Data.Option

import Control.Monad (forM_)
import Data.List
import Test.Framework

test_ord :: IO ()
test_ord =
    let list = [None, None, Some "x", Some "x", Some "y"]
    in forM_ (permutations list) $ \perm ->
           assertEqual list (sort perm)

