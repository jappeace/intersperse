{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}
module Test.TestSpec
  ( spec
  )
where

import Lib
import Test.Hspec
import Control.Monad.Writer.Lazy

one :: Int
one = 1

two :: Int
two = 2

spec :: Spec
spec =
  describe "The sanity of our test setup" $ do
  it "should satisfy equality" $          one `shouldBe` 1
  it "should intersperse a list" $ do
    let (_b :: [Int] , (_char, w :: [Int])) = runWriterT $ runIntersperse $  do
              tell [two]
              tell [3]
              pure 'c'
    w `shouldBe` [1,2,1,3]


instance BeforeCall (WriterT [Int] ((,) [Int])) where
  before = tell [1]
