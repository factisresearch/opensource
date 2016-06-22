{-# LANGUAGE DeriveGeneric #-}
module Data.Option where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Aeson
import Data.Store
import GHC.Generics

data Option a
   = None
   | Some !a
   deriving (Show, Eq, Ord, Generic)

instance NFData a => NFData (Option a)
instance Store a => Store (Option a)

instance Functor Option where
    fmap f x =
        case x of
          None -> None
          Some v -> Some (f v)
    {-# INLINE fmap #-}

instance Applicative Option where
    pure = Some
    {-# INLINE pure #-}

    f <*> x =
        case f of
          Some g -> fmap g x
          None -> None
    {-# INLINE (<*>) #-}

instance Monad Option where
    (Some x) >>= k = k x
    None  >>= _ = None
    (>>) = (*>)
    fail _ = None

instance Monoid a => Monoid (Option a) where
    mempty = None
    None `mappend` m = m
    m `mappend` None = m
    Some m1 `mappend` Some m2 = Some (m1 `mappend` m2)

instance Alternative Option where
    empty = None
    None <|> r = r
    l <|> _ = l

instance MonadPlus Option

instance ToJSON a => ToJSON (Option a) where
    toJSON = toJSON . optionToMaybe
    {-# INLINE toJSON #-}

instance FromJSON a => FromJSON (Option a) where
    parseJSON x = maybeToOption <$> parseJSON x
    {-# INLINE parseJSON #-}

optionToMaybe :: Option a -> Maybe a
optionToMaybe x =
    case x of
      Some v -> Just v
      None -> Nothing
{-# INLINE optionToMaybe #-}

maybeToOption :: Maybe a -> Option a
maybeToOption x =
    case x of
      Just v -> Some v
      Nothing -> None
{-# INLINE maybeToOption #-}
