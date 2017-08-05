{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Fail where

import Data.Fail

import Test.Framework

test_partitionFails :: IO ()
test_partitionFails =
    do assertEqual ([]::[Int], []) (partitionFails [])
       assertEqual ([1::Int], []) (partitionFails [Ok 1])
       assertEqual ([]::[Int], ["bad"]) (partitionFails [Fail "bad"])
       assertEqual ([1,2,3::Int], ["bad1", "bad2"])
           (partitionFails [Ok 1, Fail "bad1", Ok 2, Ok 3, Fail "bad2"])
