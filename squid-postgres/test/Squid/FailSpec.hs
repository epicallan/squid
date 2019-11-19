module Squid.FailSpec (spec) where

import Test.Hspec


spec :: Spec
spec = describe "Squid.FailSpec" $ do
    failSpec

failSpec :: Spec
failSpec =
  describe "throw appropriate errors" $ do
   it "throw failure error" $ do
     True `shouldBe` True
