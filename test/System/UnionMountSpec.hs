module System.UnionMountSpec where

import System.UnionMount
import Test.Hspec

-- TODO: Write tests: https://github.com/srid/unionmount/issues/5
spec :: Spec
spec = do
  describe "basics" $ do
    it "dummy" $ do
      head (one ()) `shouldBe` ()
      New `shouldBe` New
