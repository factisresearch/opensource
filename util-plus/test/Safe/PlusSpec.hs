{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Safe.PlusSpec where

import GHC.Exception
import Safe.Plus
import Test.Framework

test_safeError :: IO ()
test_safeError =
    assertThrows (safeError msg) (matches (format __FILE__ __LINE__))
    where
      matches expected (ErrorCallWithLocation actual _) = expected == actual
      format :: String -> Int -> String
      format file line = file ++ ":" ++ show line ++ ": " ++ msg
      msg = "error message"
