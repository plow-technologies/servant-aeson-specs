{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Aeson.RoundtripSpecsSpec where

import           Servant.API
import           Test.Hspec
import           Test.Hspec.Core.Runner

import           Servant.Aeson.RoundtripSpecs
import           Test.Aeson.RoundtripSpecsSpec

spec :: Spec
spec = do
  describe "allRoundtripSpec" $ do
    it "detects failures in types from ReqBody" $ do
      allRoundtripSpec reqBodyFailApi `shouldTestAs`
        Summary 2 1

    it "detects failures in types from Get" $ do
      allRoundtripSpec getFailApi `shouldTestAs`
        Summary 1 1

  describe "usedTypes" $ do
    it "extracts types from ReqBody" $ do
      usedTypes reqBodyFailApi `shouldBe`
        (Proxy :: Proxy '[FaultyRoundtrip, Bool])

    it "extracts types from Get" $ do
      usedTypes getFailApi `shouldBe`
        (Proxy :: Proxy '[FaultyRoundtrip])

    it "traverses :>" $ do
      usedTypes (Proxy :: Proxy ("foo" :> Get '[JSON] FaultyRoundtrip)) `shouldBe`
        (Proxy :: Proxy '[FaultyRoundtrip])

    it "traverses :<|>" $ do
      let api :: Proxy (Get '[JSON] FaultyRoundtrip :<|> Get '[JSON] Bool)
          api = Proxy
      usedTypes api `shouldBe` (Proxy :: Proxy [FaultyRoundtrip, Bool])

type ReqBodyFailApi =
  ReqBody '[JSON] FaultyRoundtrip :> Get '[JSON] Bool

reqBodyFailApi :: Proxy ReqBodyFailApi
reqBodyFailApi = Proxy

type GetFailApi =
  Get '[JSON] FaultyRoundtrip

getFailApi :: Proxy GetFailApi
getFailApi = Proxy
