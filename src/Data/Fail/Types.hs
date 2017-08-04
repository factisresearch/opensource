{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Fail.Types where

data Fail a
    = Fail String
    | Ok !a
    deriving (Show, Ord, Eq, Functor, Foldable, Traversable)

newtype FailT m a = FailT { unFailT :: m (Fail a) }
    deriving (Functor)

type FIO a = FailT IO a
