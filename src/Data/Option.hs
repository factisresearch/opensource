{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
module Data.Option where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Hashable
import Data.Typeable
import GHC.Generics (Generic)
import Test.QuickCheck
import qualified Control.Monad.Fail as Fail

data Option a
   = None
   | Some !a
   deriving (Show, Read, Eq, Generic, Typeable, Functor, Foldable, Traversable)

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

newtype OptionT m a
    = OptionT
    { runOptionT :: m (Option a)
    }

runOptionTDef :: Functor m => a -> OptionT m a -> m a
runOptionTDef x = fmap (fromOption x) . runOptionT

class ToOptionT t where
    optionT :: Monad m => m (t a) -> OptionT m a

instance ToOptionT Maybe where
    optionT = OptionT . liftM maybeToOption

instance ToOptionT Option where
    optionT = OptionT

instance Functor m => Functor (OptionT m) where
  fmap f = OptionT . fmap (fmap f) . runOptionT

instance (Functor m, Monad m) => Applicative (OptionT m) where
    pure = return
    (<*>) = ap

instance Monad m => Fail.MonadFail (OptionT m) where
    fail _ = OptionT (return None)

instance Monad m => Monad (OptionT m) where
    fail = Fail.fail
    return = lift . return
    x >>= f = OptionT (runOptionT x >>= option (return None) (runOptionT . f))

instance Ord a => Ord (Option a) where
    compare x y =
        case x of
          Some a ->
              case y of
                Some b -> compare a b
                None -> GT
          None ->
              case y of
                None -> EQ
                Some _ -> LT

instance NFData a => NFData (Option a) where
    rnf None = ()
    rnf (Some b) = rnf b

instance MonadTrans OptionT where
    lift x = OptionT (liftM Some x)

instance (MonadIO m) => MonadIO (OptionT m) where
    liftIO = lift . liftIO

instance Fail.MonadFail Option where
    fail _ = None

instance Arbitrary a => Arbitrary (Option a) where
    arbitrary = frequency [(1, return None), (3, liftM Some arbitrary)]

    shrink (Some x) = None : [ Some x' | x' <- shrink x ]
    shrink _        = []

noneIf :: (a -> Bool) -> a -> Option a
noneIf p x
    | p x = None
    | otherwise = Some x

fromOption :: a -> Option a -> a
fromOption def opt =
    case opt of
      Some x -> x
      None -> def

isSome :: Option a -> Bool
isSome (Some _) = True
isSome _ = False

isNone :: Option a -> Bool
isNone None = True
isNone _ = False

optionToMaybe :: Option a -> Maybe a
optionToMaybe (Some a) = Just a
optionToMaybe None = Nothing
{-# INLINE optionToMaybe #-}

maybeToOption :: Maybe a -> Option a
maybeToOption (Just a) = Some a
maybeToOption Nothing = None
{-# INLINE maybeToOption #-}

optionToList :: Option a -> [a]
optionToList (Some a) = [a]
optionToList None = []

-- optionToSL :: Option a -> StrictList a
-- optionToSL (Some a) = a :! Nil
-- optionToSL None = Nil

listToOption :: [a] -> Option a
listToOption [] = None
listToOption (x:_) = Some x

getSomeNote :: Monad m => String -> Option a -> m a
getSomeNote str = option (fail str) return

option :: b -> (a -> b) -> Option a -> b
option def f opt =
    case opt of
      Some a -> f $! a
      None -> def

catOptions :: [Option a] -> [a]
catOptions ls = [x | Some x <- ls]

mapOption :: (a -> Option b) -> [a] -> [b]
mapOption _ [] = []
mapOption f (x:xs) =
    let rs = mapOption f xs in
    case f x of
      None -> rs
      Some r  -> r : rs

instance Hashable a => Hashable (Option a)

forOptionM :: Monad m => [a] -> (a -> OptionT m b) -> m [b]
forOptionM xs f = liftM catOptions (forM xs (runOptionT . f))

mapOptionM :: Monad m => (a -> OptionT m b) -> [a] -> m [b]
mapOptionM = flip forOptionM

-- safeFromSome :: (HasCallStack) => Option a -> a
-- safeFromSome = fromOption (safeError "fromSome None")

-- optionToFail :: String -> Option a -> Fail a
-- optionToFail _ (Some x) = Ok x
-- optionToFail err None = Fail err

-- optionToFailT :: Monad m => String -> Option a -> FailT m a
-- optionToFailT _ (Some x) = return x
-- optionToFailT err None = safeFail err

