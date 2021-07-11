{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Intersperse
  ( IntersperseT (..)
  , Intersperse
  , BeforeBindCall(..)
  )
where

import Data.Functor.Identity
import Control.Monad.Writer.Lazy
import Control.Monad.Except
import Control.Monad.State.Class
import Control.Monad.Reader


-- | this allows you to intersperse bind calls with a before callback.
--  this is usefull to show/log progress on a computation at every side effect.
--
--  A good example is during the processing of some background task,
--  you want to ensure it's doing stuff, simply interspering it with
--  a program counter and a log message will give you that feedback.
data IntersperseT m a = MkIntersperse {
  runIntersperse :: m a
  }

-- | to run intersperse this typeclass has to be defined for whatever monad stack is being run.
--   newtypes can be used to get different before calls for the same
--   monad stacks.
class Monad m => BeforeBindCall m where
  before ::  m ()

type Intersperse = IntersperseT Identity

instance BeforeBindCall m => Monad (IntersperseT m) where
  (>>=) (MkIntersperse {runIntersperse}) fun = do
    MkIntersperse $ do
      before
      a <- runIntersperse
      let MkIntersperse y = fun a
      y

instance MonadTrans IntersperseT where
  lift = MkIntersperse


instance BeforeBindCall m => Applicative (IntersperseT m) where
   -- this is a straight copy from the monad laws,
   -- we satisfy them like this (and get the right amount of before calls)
  (<*>) m1 m2  =
    m1 >>= (\ x1 -> m2 >>= (\ x2 -> return (x1 x2)))
  pure x = MkIntersperse (pure x)

instance BeforeBindCall f => Functor (IntersperseT f) where
  fmap fun xs = xs >>= pure . fun

instance (BeforeBindCall m, MonadWriter w m) => MonadWriter w (IntersperseT m) where
  tell = lift . tell
  listen = lift . listen . runIntersperse
  pass = lift . pass . runIntersperse

instance (BeforeBindCall m, MonadIO m) => MonadIO (IntersperseT m) where
  liftIO = MkIntersperse . liftIO

instance (BeforeBindCall m, MonadError e m) => MonadError e (IntersperseT m) where
  throwError = lift . throwError

instance (BeforeBindCall m, MonadReader e m) => MonadReader e (IntersperseT m) where
  ask = lift ask
  local r m = lift $ local r (runIntersperse m)

instance (BeforeBindCall m, MonadState e m) => MonadState e (IntersperseT m) where
  get = lift get
  put = lift . put
