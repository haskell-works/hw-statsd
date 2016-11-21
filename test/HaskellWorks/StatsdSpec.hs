{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.StatsdSpec (spec) where

import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.SearchSpec" $ do
  it "No tests" $ do
    1 `shouldBe` (1 :: Int)
