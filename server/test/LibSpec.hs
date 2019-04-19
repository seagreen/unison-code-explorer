module LibSpec where

import Prelude
import Test.Hspec

spec :: Spec
spec = do
  describe "Lib.one" $ do
    it "is 1" $
      True `shouldBe` True
