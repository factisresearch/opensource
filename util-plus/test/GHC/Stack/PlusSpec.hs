{-# OPTIONS_GHC -F -pgmF htfpp #-}
module GHC.Stack.PlusSpec
    ( htf_thisModulesTests
    )
where

import GHC.Stack.Plus
import Test.Framework

test_callerSrcLoc :: IO ()
test_callerSrcLoc =
    do assertEqual __FILE__ callerFile
       assertEqual __LINE__ callerLine
