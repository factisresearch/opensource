{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} Data.List.PlusSpec
import {-@ HTF_TESTS @-} GHC.Stack.PlusSpec
import {-@ HTF_TESTS @-} Safe.PlusSpec

import Test.Framework

main :: IO ()
main = htfMain htf_importedTests
