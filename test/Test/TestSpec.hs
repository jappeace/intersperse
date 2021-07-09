{-# LANGUAGE ScopedTypeVariables #-}
module Test.TestSpec
  ( spec
  )
where

import           Lib
import           Test.Hspec
import Control.Monad.Writer.Lazy
import Data.Functor.Identity

one :: Int
one = 1

two :: Int
two = 2

spec :: Spec
spec =
  describe "The sanity of our test setup" $ do
  it "should satisfy equality" $          one `shouldBe` 1
  it "should intersperse a list" $ do
    let (b :: [Int] , (char, w :: [Int])) = runIntersperse $ runWriterT $ do
              tell [two]
              tell [3]
              lift $ pure 'c'
    w `shouldBe` [1,2,1,3]
