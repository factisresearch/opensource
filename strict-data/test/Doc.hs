module Main where

import System.FilePath.Find
import Test.DocTest

_SRC_DIR_ :: FilePath
_SRC_DIR_ = "src"

main :: IO ()
main =
    do tree <- find always (fileName ~~? "*.hs") _SRC_DIR_
       doctest $ ("-i" ++ _SRC_DIR_) : tree

