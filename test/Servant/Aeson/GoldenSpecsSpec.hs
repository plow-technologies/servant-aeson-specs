{-# LANGUAGE DataKinds #-}

module Servant.Aeson.GoldenSpecsSpec where

import           Data.Proxy
import           Servant.API
import           System.Directory
import           Test.Hspec
import           Test.Hspec.Core.Runner
import           Test.Mockery.Directory
import           Test.Utils

import           Servant.Aeson.GoldenSpecs

spec :: Spec
spec = do
  describe "servantGoldenSpecs" $ do
    it "writes files for used types" $ do
      inTempDirectory $ do
        _ <- hspecSilently $ servantGoldenSpecs (Proxy :: Proxy (Get '[JSON] Bool))
        doesFileExist "golden.json/Bool.json" `shouldReturn` True

    it "raises errors for non-matching golden files" $ do
      inTempDirectory $ do
        createDirectoryIfMissing True "golden.json"
        writeFile "golden.json/Bool.json" "foo"
        servantGoldenSpecs (Proxy :: Proxy (Get '[JSON] Bool)) `shouldTestAs`
          Summary 1 1
