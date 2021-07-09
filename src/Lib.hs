{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances #-}
module Lib
  ( IntersperseT (..)
  )
where

import Data.Functor.Identity
import Control.Monad.Writer.Lazy


-- | this allows you to interlace bind calls with a before and after callback.
--   this is usefull to show/log progress on a computation at every side effect.
--
--   A good example is during the processing of some background task
data IntersperseT m a = MkIntersperse {
    before :: m ()
  , runIntersperse :: m a
  }

type Intersperse = IntersperseT Identity

instance Monad m => Monad (IntersperseT m) where
  (>>=) (MkIntersperse {before, runIntersperse}) fun = do
    MkIntersperse before $ do
      before
      a <- runIntersperse
      let MkIntersperse _ y = fun a
      y

instance MonadTrans IntersperseT where
  lift m = MkIntersperse (pure ()) m

instance Applicative m => Applicative (IntersperseT m) where
  (<*>) (MkIntersperse before abF) (MkIntersperse before2 a) =
    MkIntersperse
      (before <* before2) -- is this unlawfull? I think this means it builds up before calls
      (abF <*> a)
  pure x = MkIntersperse (pure ()) (pure x)

instance Functor f => Functor (IntersperseT f) where
  fmap fun (MkIntersperse before underlying) = MkIntersperse before $ fun <$> underlying

instance MonadWriter w m => MonadWriter w (IntersperseT m) where
  tell x = lift $ tell x
