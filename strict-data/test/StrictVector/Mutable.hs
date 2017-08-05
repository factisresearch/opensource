{-# OPTIONS_GHC -F -pgmF htfpp #-}
module StrictVector.Mutable where

import Data.StrictVector.Mutable

import Control.Exception
import Test.Framework
import qualified Data.Vector.Generic.Mutable as VGM

test_replicateStrict :: IO ()
test_replicateStrict =
    do let err = ErrorCall "..."
       res <- try $ (VGM.replicate 10 (throw err :: ()) :: IO (IOVector ()))
       assertEqual (Left err) (res >> Right ())

test_writeStrict :: IO ()
test_writeStrict =
    do let err = ErrorCall "..."
       vec <- VGM.replicate 1 ()
       res <- try $ VGM.write (vec :: IOVector ()) 0 (throw err)
       assertEqual (Left err) res

test_setStrict :: IO ()
test_setStrict =
    do let err = ErrorCall "..."
       vec <- VGM.replicate 1 ()
       res <- try $ VGM.set (vec :: IOVector ()) (throw err)
       assertEqual (Left err) res
