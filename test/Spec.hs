{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} Fail
import {-@ HTF_TESTS @-} Option
import {-@ HTF_TESTS @-} StrictList

import Test.Framework

main :: IO ()
main = htfMain htf_importedTests
