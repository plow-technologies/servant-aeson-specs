{-# LANGUAGE DataKinds #-}

module Servant.Aeson.GoldenSpecsSpec where

import           Data.Proxy
import           Servant.Aeson.GenericSpecs
import           Servant.API
import           System.Directory
import           Test.Hspec
import           Test.Hspec.Core.Runner
import           Test.Mockery.Directory
import           Test.Utils


spec :: Spec
spec = do
  describe "apiGoldenSpecs" $ do
    it "writes files for used types" $ do
      inTempDirectory $ do
        _ <- hspecSilently $ apiGoldenSpecs defaultSettings (Proxy :: Proxy (Get '[JSON] Bool))
        doesFileExist "golden/Bool.json" `shouldReturn` True

    it "raises errors for non-matching golden files" $ do
      inTempDirectory $ do
        createDirectoryIfMissing True "golden"
        writeFile "golden/Bool.json" "foo"
        apiGoldenSpecs defaultSettings (Proxy :: Proxy (Get '[JSON] Bool)) `shouldTestAs`
          Summary 1 1
