{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
module Lib
  ( IntersperseT (..)
  , Intersperse
  , BeforeCall(..)
  )
where

import Data.Functor.Identity
import Control.Monad.Writer.Lazy


-- | this allows you to interlace bind calls with a before and after callback.
--   this is usefull to show/log progress on a computation at every side effect.
--
--   A good example is during the processing of some background task
data IntersperseT m a = MkIntersperse {
  runIntersperse :: m a
  }

class Monad m => BeforeCall m where
  before ::  m ()

type Intersperse = IntersperseT Identity

instance (BeforeCall m, Monad m) => Monad (IntersperseT m) where
  (>>=) (MkIntersperse {runIntersperse}) fun = do
    MkIntersperse $ do
      before
      a <- runIntersperse
      let MkIntersperse y = fun a
      y

instance MonadTrans IntersperseT where
  lift = MkIntersperse


instance Applicative m => Applicative (IntersperseT m) where
  (<*>) (MkIntersperse abF) (MkIntersperse a) =
    MkIntersperse
      (abF <*> a)
  pure x = MkIntersperse (pure x)

instance Functor f => Functor (IntersperseT f) where
  fmap fun (MkIntersperse underlying) = MkIntersperse $ fun <$> underlying

instance (BeforeCall m, MonadWriter w m) => MonadWriter w (IntersperseT m) where
  tell = lift . tell
  listen = lift . listen . runIntersperse
  pass = lift . pass . runIntersperse
