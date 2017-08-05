{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Fail.Types
  ( Fail(..)
  , pattern Fail
  , FailT(..)
  , FIO
  ) where

import qualified Data.Text as T

data Fail a
    = Err T.Text
    | Ok !a
    deriving (Show, Ord, Eq, Functor, Foldable, Traversable)

pattern Fail :: String -> Fail a
pattern Fail x <- Err (T.unpack -> x) where
    Fail x = Err (T.pack x)

newtype FailT m a = FailT { unFailT :: m (Fail a) }
    deriving (Functor)

type FIO a = FailT IO a
