{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}
module Lib
  ( IntersperseT (..)
  )
where


-- | this allows you to interlace bind calls with a before and after callback.
--   this is usefull to show/log progress on a computation at every side effect.
--
--   A good example is during the processing of some background task
data IntersperseT m a = MkIntersperse {
    before :: m ()
  , underlying :: m a
  }

instance Monad m => Monad (IntersperseT m) where
  (>>=) (MkIntersperse {before, underlying}) fun = do
    MkIntersperse before $ do
      before
      a <- underlying
      let MkIntersperse _ y = fun a
      y

instance Applicative m => Applicative (IntersperseT m) where
  (<*>) (MkIntersperse before abF) (MkIntersperse before2 a) =
    MkIntersperse
      (before <* before2) -- is this unlawfull? I think this means it builds up before calls
      (abF <*> a)
  pure x = MkIntersperse (pure ()) (pure x)

instance Functor f => Functor (IntersperseT f) where
  fmap fun (MkIntersperse before underlying) = MkIntersperse before $ fun <$> underlying
