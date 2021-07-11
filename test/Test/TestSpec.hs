{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}
module Test.TestSpec
  ( spec
  )
where

import Lib
import Test.Hspec
import Control.Monad.Writer.Lazy
import Data.IORef

one :: Int
one = 1

two :: Int
two = 2

spec :: Spec
spec =
  describe "The sanity of our test setup" $ do
  it "should satisfy equality" $          one `shouldBe` 1
  it "should intersperse a list" $ do
    let (_b :: [Int] , (_char, w :: [Int])) = runWriterT $ runIntersperseT (tell [1]) $  do
              tell [two]
              tell [3]
              pure 'c'
    w `shouldBe` [1,2,1,3]

  it "can program count" $ do
    ref <- newIORef 0
    runIntersperseT (liftIO $ modifyIORef ref (+1)) $ do
      liftIO $ putStrLn "hello "
      liftIO $ putStrLn "world"

    res <- readIORef ref
    res `shouldBe` 2
