module Control.Applicative.Plus
    ( withOptional
    , optional'
    , module Control.Applicative
    )
where

import Control.Applicative
import Control.Monad
import qualified Data.Foldable as F

{-# SPECIALIZE
       withOptional :: (a -> (b -> m c) -> m c) -> Maybe a -> (Maybe b -> m c) -> m c
 #-}
withOptional ::
    (Foldable t, Alternative t)
    => (a -> (b -> m c) -> m c)
    -> t a
    -> (t b -> m c)
    -> m c
withOptional withReq wrappedVal go =
    do let runAction =
               flip fmap wrappedVal $ \val ->
               withReq val $ \inner -> go (pure inner)
           emptyCase = go empty
           actionOrEmptyCase =
               F.foldl' (\_ a -> a) emptyCase runAction
       actionOrEmptyCase

-- | A generalized version of 'optional' that works with 'Option' for example
optional' :: (MonadPlus t, Alternative f) => f a -> f (t a)
optional' x =
    flip fmap (optional x) $ \val ->
    case val of
      Just ok -> pure ok
      Nothing -> empty
