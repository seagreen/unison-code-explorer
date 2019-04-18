module LibSpec where

import qualified Lib
import Prelude
import Test.Hspec
import qualified Test.QuickCheck as QuickCheck

spec :: Spec
spec = do
  describe "Lib.one" $ do
    it "is 1" $
      Lib.one `shouldBe` 1
  describe "Int" $ do
    it "is equal to itself" $ do
      QuickCheck.property $ \(n :: Int) ->
        n `shouldBe` n
