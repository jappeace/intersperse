{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Lib
  ( IntersperseT
  , Intersperse
  , runIntersperseT
  )
where

import Data.Functor.Identity
import Control.Monad.Writer.Lazy


-- | this allows you to interlace bind calls with a before and after callback.
--   this is usefull to show/log progress on a computation at every side effect.
--
--   A good example is during the processing of some background task
data IntersperseT m a = MkIntersperse
  { iBefore ::  m ()
  , iUnderlying :: m a
  }

runIntersperseT :: m () -> IntersperseT m a -> m a
runIntersperseT before inter =
  iUnderlying $ inter { iBefore = before }


type Intersperse = IntersperseT Identity

instance (Monad m) => Monad (IntersperseT m) where
  (>>=) (MkIntersperse {iUnderlying, iBefore}) fun = do
    MkIntersperse iBefore $ do
      iBefore
      a <- iUnderlying
      let MkIntersperse _ y = fun a
      y

instance MonadTrans IntersperseT where
  lift = MkIntersperse (pure ())


instance Applicative m => Applicative (IntersperseT m) where
  (<*>) (MkIntersperse before abF) (MkIntersperse _ a) =
    MkIntersperse before
      (abF <*> a)
  pure x = MkIntersperse (pure ()) (pure x)

instance Functor f => Functor (IntersperseT f) where
  fmap fun (MkIntersperse before underlying) = MkIntersperse before $ fun <$> underlying

instance (MonadWriter w m) => MonadWriter w (IntersperseT m) where
  tell = lift . tell
  listen = lift . listen . iUnderlying
  pass = lift . pass . iUnderlying

instance MonadIO m => MonadIO (IntersperseT m) where
  liftIO = MkIntersperse (pure ()) . liftIO
