{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}
module Test.TestSpec
  ( spec
  )
where


import Control.Monad.Intersperse
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

  it "can program count, (assign a number to each bind call)" $ do
    ref <- newIORef 0
    void $ flip runReaderT ref $ unIORef $ runIntersperse someProgram
    res <- readIORef ref
    res `shouldBe` 2

newtype ProgramCounterTestM m a = MkProgramCounterTest { unIORef :: ReaderT (IORef Int) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (IORef Int) )

-- the instance decides what to interspserse
instance BeforeBindCall (ProgramCounterTestM IO) where
  before = do
    ref <- ask
    liftIO $ modifyIORef ref (+1)

someProgram :: MonadIO m => m Char
someProgram = do
      liftIO $ putStrLn "hello " -- 0
      liftIO $ putStrLn "world"  -- 1
      pure 'x' -- 2

instance BeforeBindCall (WriterTestM IO) where
  before = tell [1]
