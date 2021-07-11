{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}
module Test.TestSpec
  ( spec
  )
where

import Lib
import Test.Hspec
import Control.Monad.Writer.Lazy
import Control.Monad.Reader
import Data.IORef

one :: Int
one = 1

two :: Int
two = 2

-- define some base stack
newtype WriterTestM m a =  MkWriterTestM { unListstack :: WriterT [Int] m a }
  deriving (Functor, Applicative, Monad, MonadWriter [Int])

newtype ProgramCounterTestM m a = MkProgramCounterTest { unIORef :: ReaderT (IORef Int) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IORef Int) )

spec :: Spec
spec =
  describe "The sanity of our test setup" $ do
  it "should satisfy equality" $          one `shouldBe` 1
  it "should intersperse a list" $ do

    (_char, w :: [Int]) <- runWriterT $ unListstack $ runIntersperse $  do -- run intersperce
              tell [two]
              tell [3]
              pure 'c'
    w `shouldBe` [1,2,1,3]

  it "can program count" $ do
    ref <- newIORef 0
    flip runReaderT ref $ unIORef $ runIntersperse $
      (liftIO $ putStrLn "hello ") >> (liftIO $ putStrLn "world") >> pure 'x'

    res <- readIORef ref
    res `shouldBe` 2

-- the instance decides what to interspserse
instance BeforeCall (WriterTestM IO) where
  before = tell [1]

instance BeforeCall (ProgramCounterTestM IO) where
  before = do
    ref <- ask
    liftIO $ modifyIORef ref (+1)
