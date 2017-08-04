{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import {-@ HTF_TESTS @-} Option
import {-@ HTF_TESTS @-} StrictList

import Data.Option

import Control.Monad
import Test.Framework
import Data.List

main = htfMain htf_importedTests
