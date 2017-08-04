{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} Option
import {-@ HTF_TESTS @-} StrictList

import Test.Framework

main = htfMain htf_importedTests
