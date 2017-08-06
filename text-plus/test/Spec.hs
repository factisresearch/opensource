{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} Data.Text.PlusSpec

import Test.DocTest
import Test.Framework

main :: IO ()
main =
    do doctest ["src"]
       htfMain htf_importedTests
