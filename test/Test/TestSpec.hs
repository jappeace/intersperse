module Test.TestSpec
  ( spec
  )
where

import           Lib
import           Test.Hspec
import Control.Monad.Writer.Lazy



one :: Int
one = 1

spec :: Spec
spec =
  describe "The sanity of our test setup" $ do
  it "should satisfy equality" $          one `shouldBe` 1
  it "should intersperse a list" $ do
    let res = runIntersperse $ runWriterT $ do
              lift $ tell [2]
              lift $ tell [3]
              pure 'c'
    res `shouldBe` [1,2,1,3]
