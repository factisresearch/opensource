{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework
import {-@ HTF_TESTS @-} Test.Core
import {-@ HTF_TESTS @-} Test.NodeManager
import {-@ HTF_TESTS @-} Test.Persistence

main :: IO ()
main = htfMain htf_importedTests
