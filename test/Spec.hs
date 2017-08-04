{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Control.Monad
import Data.Option
import Data.List
import Test.Framework

main = htfMain htf_thisModulesTests

test_ord :: IO ()
test_ord =
    let list = [None, None, Some "x", Some "x", Some "y"]
    in forM_ (permutations list) $ \perm ->
           assertEqual list (sort perm)
