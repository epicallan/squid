module Squid.SuccessSpec  (spec) where

import Test.Hspec


spec :: Spec
spec = describe "Squid.SuccessSpec" $ do
    successSpec

successSpec :: Spec
successSpec =
  describe "runs properly" $ do
   it "returns expected result" $ do
     True `shouldBe` True
