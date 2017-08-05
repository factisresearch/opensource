-- Run this using
-- @stack exec -- runhaskell helper.hs@
{-# LANGUAGE OverloadedStrings #-}

import System.Directory
import System.Environment
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main =
    do args <- getArgs
       case args of
         ["opensource", file, package, newName] ->
             do ct <- T.readFile file
                let fileName =
                        package ++ "/src/"
                        ++ T.unpack (T.replace "." "/" (T.pack newName))
                        ++ ".hs"
                    baseDir = takeDirectory fileName
                createDirectoryIfMissing True baseDir
                T.writeFile fileName ct
         _ ->
             putStrLn "Usage: helper.hs opensource [src] [pkg] [tgt]"
