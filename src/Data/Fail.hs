{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-orphans #-}
module Data.Fail
    ( Fail(..), isFail, isOk
    , FailT(FailT), runFailT, FIO
    , failEitherStr, failEitherShow, failEitherText
    , runExceptTFail, failInM, failInM', failInM''
    , failToEither, failMaybe, failToMaybe, mapFail
    , failSwitch, fromFail
    , MonadFailure(..)
    , failForIOException, catFails
    , eitherToError, errorToEither, liftError, errorToDefault, errorToMaybe, maybeToError, runError
    , runExceptTorFail, maybeToFail, eitherToFail
    , fromFailString, partitionFails
    , Control.Monad.Fail.MonadFail
    , pattern Fail
) where


import Data.Fail.Types

import Control.Monad.Base (MonadBase (..), liftBaseDefault)
import Control.Monad.Catch (MonadThrow (..))
import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans.Control
    ( MonadBaseControl (..), defaultLiftBaseWith, defaultRestoreM, ComposeSt
    , MonadTransControl (..) )
import Control.Applicative (Alternative(..))
import Control.Exception (ErrorCall(..), IOException, catch)
import Control.Monad (MonadPlus(..))
import Control.Monad.Except (ExceptT, runExceptT, MonadError(..))
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Resource (MonadResource (..))
import Control.Monad.Writer (MonadWriter(..))
import qualified Control.Monad.Fail
import qualified Data.Text as T

instance MonadThrow m => MonadThrow (FailT m) where
    throwM = FailT . throwM

instance MonadBase b m => MonadBase b (FailT m) where
    liftBase = liftBaseDefault

instance MonadBaseControl b m => MonadBaseControl b (FailT m) where
    type StM (FailT m) a = ComposeSt FailT m a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM

instance MonadTransControl FailT where
    type StT FailT a = Fail a
    liftWith f = FailT $ fmap return $ f $ runFailT
    restoreT = FailT

instance Monad m => MonadError String (FailT m) where
    throwError = throwFailT
    catchError = catchFailT

instance MonadTrans FailT where
    lift m =
        FailT $
        do a <- m
           return (Ok a)

instance MonadIO m => MonadIO (FailT m) where
    liftIO io = FailT (liftIO io >>= (return . Ok))

instance MonadState s m => MonadState s (FailT m) where
    get = lift get
    put = lift . put

instance MonadResource m => MonadResource (FailT m) where
    liftResourceT = FailT . fmap Ok . liftResourceT

instance MonadWriter w m => MonadWriter w (FailT m) where
    tell = lift . tell
    listen =
        mapFailT $ \m ->
        do (a, w) <- listen m
           return $! fmap (\r -> (r, w)) a
    pass =
        mapFailT $ \m ->
        pass $
        do a <- m
           return $!
               case a of
                 Fail l -> (Fail l, id)
                 Ok (r, f) -> (Ok r, f)

mapFailT :: (m (Fail a) -> n (Fail b)) -> FailT m a -> FailT n b
mapFailT f = FailT . f . runFailT

throwFailT :: Monad m => String -> FailT m a
throwFailT l = FailT $ return (Fail l)

catchFailT :: Monad m => FailT m a -> (String -> FailT m a) -> FailT m a
m `catchFailT` h =
    FailT $
    do a <- runFailT m
       case a of
         Fail  l -> runFailT (h l)
         Ok r -> return (Ok r)

isFail :: Fail a -> Bool
isFail (Fail _) = True
isFail (Ok _) = False

isOk :: Fail a -> Bool
isOk = not . isFail

instance Monad Fail where
    return = Ok
    {-# INLINE return #-}
    fail = Control.Monad.Fail.fail
    {-# INLINE fail #-}
    (>>=) = failBind
    {-# INLINE (>>=) #-}

instance Control.Monad.Fail.MonadFail Fail where
    fail = Fail
    {-# INLINE fail #-}

instance MonadPlus Fail where
    mzero = failZero
    mplus = failPlus

instance Applicative Fail where
    pure = Ok
    (<*>) = failAp

instance Alternative Fail where
    empty = failZero
    (<|>) = failPlus

instance MonadFix Fail where
    mfix f = let a = f (unOk a) in a
        where
          unOk (Ok x) = x
          unOk (Fail msg) = error ("mfix failed: " ++ msg)

instance MonadFix m => MonadFix (FailT m) where
    mfix f =
        FailT $ mfix $ \a -> runFailT $ f $
        case a of
          Ok r -> r
          Fail msg -> error ("FailT.mfix failed: " ++ msg)

instance Monad m => Monad (FailT m) where
    return = returnFailT
    fail = Control.Monad.Fail.fail
    (>>=) = bindFailT

instance Monad m => Control.Monad.Fail.MonadFail (FailT m) where
    fail = FailT . return . Fail

instance (Functor m, Monad m) => Applicative (FailT m) where
    pure = FailT . return . Ok
    FailT f <*> FailT v =
        FailT $
            do mf <- f
               case mf of
                 Fail msg -> return (Fail msg)
                 Ok k ->
                     do mv <- v
                        case mv of
                          Fail msg -> return (Fail msg)
                          Ok x -> return (Ok (k x))

instance Monad m => Alternative (FailT m) where
    empty = FailT $ return failZero
    FailT f <|> FailT g =
        FailT $
            do mf <- f
               mg <- g
               return $ mf `failPlus` mg

instance Monad m => MonadPlus (FailT m) where
    mzero = empty
    mplus = (<|>)

failBind :: Fail a -> (a -> Fail b) -> Fail b
failBind ma f =
    case ma of
      Ok x -> {-# SCC "Fail/>>=/f" #-} (f x)
      -- is there a better way to avoid allocations?
      Fail x -> {-# SCC "Fail/>>=/Fail" #-} (Fail x)
{-# INLINE failBind #-}

failAp :: Fail (a -> b) -> Fail a -> Fail b
failAp (Ok f) (Ok a) = Ok (f a)
failAp (Fail msg) _ = Fail msg
failAp _ (Fail msg) = Fail msg
{-# INLINE failAp #-}

failZero :: Fail a
failZero = Fail "mzero"
{-# INLINE failZero #-}

failPlus :: Fail a -> Fail a -> Fail a
failPlus x@(Ok _) _ = x
failPlus _ x = x
{-# INLINE failPlus #-}

failSwitch :: (String -> c) -> (a -> c) -> Fail a -> c
failSwitch _ g (Ok x) = g x
failSwitch f _ (Fail x) = f x
{-# INLINE failSwitch #-}

{-# INLINE runFailT #-}
runFailT :: FailT m a -> m (Fail a)
runFailT = unFailT

{-# INLINE returnFailT #-}
returnFailT :: Monad m => a -> FailT m a
returnFailT = FailT . return . Ok

{-# INLINE bindFailT #-}
bindFailT :: Monad m => FailT m a -> (a -> FailT m b) -> FailT m b
bindFailT (FailT action) f =
    FailT $
    do mx <- action
       case mx of
         Ok x -> unFailT (f x)
         Fail m -> return (Fail m)

instance MonadError String Fail where
    throwError             = Fail
    Fail  l `catchError` h = h l
    Ok r `catchError` _    = Ok r

failMaybe :: String -> Maybe a -> Fail a
failMaybe _ (Just x) = Ok x
failMaybe msg Nothing = Fail msg

failEitherStr :: Either String a -> Fail a
failEitherStr = either Fail Ok

failEitherText :: Either T.Text a -> Fail a
failEitherText = either (Fail . T.unpack) Ok

failEitherShow :: Show a => Either a b -> Fail b
failEitherShow e =
    case e of
      Left err -> Fail $ show err
      Right val -> Ok val

runExceptTFail :: Monad m => ExceptT String m a -> m (Fail a)
runExceptTFail err =
    do eith <- runExceptT err
       case eith of
         Left err -> return $ Fail err
         Right x -> return $ Ok x

class Control.Monad.Fail.MonadFail m => MonadFailure m where
    catchFailure :: m a -> (String -> m a) -> m a

instance MonadFailure Maybe where
    Nothing `catchFailure` hdl = hdl "Failed in Maybe."
    ok `catchFailure` _ = ok

instance MonadFailure IO where
    catchFailure action hdl = action `catch` \(ErrorCall s) -> hdl s

instance MonadFailure Fail where
    ok@(Ok _) `catchFailure` _ =  ok
    Fail msg `catchFailure` hdl = hdl msg

instance Monad m => MonadFailure (FailT m) where
    FailT action `catchFailure` hdl =
        FailT $
        do result <- action
           case result of
             Fail msg -> unFailT (hdl msg)
             Ok _ -> return result

instance (MonadFail (ReaderT r m), MonadFailure m) => MonadFailure (ReaderT r m) where
    action `catchFailure` handler =
        ReaderT $ \r ->
        runReaderT action r `catchFailure` \msg -> runReaderT (handler msg) r

failInM :: Monad m => Fail a -> m a
failInM f = failInM' f id

failInM' :: Monad m => Fail a -> (String -> String) -> m a
failInM' f h =
    case f of
      Ok x -> return x
      Fail msg -> fail (h msg)

failInM'' :: Monad m => String -> Fail a -> m a
failInM'' what = flip failInM' (("Failed to " ++ what ++ ":")++)

mapFail :: (String -> String) -> Fail a -> Fail a
mapFail f x =
    case x of
      Ok _ -> x
      Fail msg -> Fail (f msg)

failToEither :: Fail a -> Either String a
failToEither (Ok x) = Right x
failToEither (Fail x) = Left x

failToMaybe :: Fail a -> Maybe a
failToMaybe (Ok x) = Just x
failToMaybe _ = Nothing

failForIOException :: IO a -> IO (Fail a)
failForIOException action =
    catch (Ok <$> action) (\(exc::IOException) -> return (Fail (show exc)))

catFails :: [Fail a] -> [a]
catFails [] = []
catFails ((Fail _):xs) = catFails xs
catFails ((Ok a):xs) = a:(catFails xs)

fromFail :: (String -> a) -> Fail a -> a
fromFail f = failSwitch f id

fromFailString :: Fail a -> Maybe String
fromFailString f =
    case f of
      Ok _ -> Nothing
      Fail str -> Just str

runError :: forall a. (forall m. Monad m => m a) -> Either String a
runError x = runIdentity (runExceptT x)

partitionFails :: [Fail a] -> ([a], [String])
partitionFails l = go l ([], [])
    where
      go l (good, bad) =
          case l of
            [] ->
                (reverse good, reverse bad)
            (Ok x : rest) ->
                go rest (x : good, bad)
            (Fail s : rest) ->
                go rest (good, s : bad)

eitherToError :: MonadError e m => Either e a -> m a
eitherToError = either throwError return

errorToEither :: MonadError e m => m a -> m (Either e a)
errorToEither m = catchError (Right <$> m) (return . Left)

errorToDefault :: MonadError e m => a -> m a -> m a
errorToDefault a ma = catchError ma (\_ -> return a)

liftError :: (MonadError e m, MonadError e m1) => (forall a. m a -> m1 a) -> m a -> m1 a
liftError liftBase action = liftBase (errorToEither action) >>= eitherToError

errorToMaybe :: MonadError e m => m a -> m (Maybe a)
errorToMaybe ma = catchError (Just <$> ma) (\_ -> return Nothing)

maybeToError :: MonadError e m => String -> Maybe a -> m a
maybeToError msg ma =
    case ma of
      Nothing -> fail msg
      Just a -> return a

maybeToFail :: Monad m => String -> Maybe a -> m a
maybeToFail msg ma =
    case ma of
         Nothing -> fail msg
         Just a -> return a

eitherToFail :: Monad m => Either String a -> m a
eitherToFail = either fail return

runExceptTorFail :: (Monad m, Show e) => ExceptT e m a -> m a
runExceptTorFail action =
    do result <- runExceptT action
       either (fail . show) return result
