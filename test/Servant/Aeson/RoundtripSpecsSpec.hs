{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Aeson.RoundtripSpecsSpec where

import           Data.Typeable
import           Servant.API
import           Test.Hspec
import           Test.Hspec.Core.Runner

import           Servant.Aeson.RoundtripSpecs
import           Test.Aeson.RoundtripSpecsSpec

spec :: Spec
spec = do
  describe "roundtripSpecs" $ do
    it "detects failures in types from ReqBody" $ do
      roundtripSpecs reqBodyFailApi `shouldTestAs`
        Summary 2 1

    it "detects failures in types from Get" $ do
      roundtripSpecs getFailApi `shouldTestAs`
        Summary 1 1

  describe "usedTypes" $ do
    it "extracts types from ReqBody" $ do
      usedTypes reqBodyFailApi `shouldBe`
        [faultyRoundtripRep, boolRep]

    it "extracts types from Get" $ do
      usedTypes getFailApi `shouldBe`
        [faultyRoundtripRep]

    it "traverses :>" $ do
      usedTypes (Proxy :: Proxy ("foo" :> Get '[JSON] FaultyRoundtrip)) `shouldBe`
        [faultyRoundtripRep]

    it "traverses :<|>" $ do
      let api :: Proxy (Get '[JSON] FaultyRoundtrip :<|> Get '[JSON] Bool)
          api = Proxy
      usedTypes api `shouldBe` [faultyRoundtripRep, boolRep]

type ReqBodyFailApi =
  ReqBody '[JSON] FaultyRoundtrip :> Get '[JSON] Bool

reqBodyFailApi :: Proxy ReqBodyFailApi
reqBodyFailApi = Proxy

type GetFailApi =
  Get '[JSON] FaultyRoundtrip

getFailApi :: Proxy GetFailApi
getFailApi = Proxy

faultyRoundtripRep :: TypeRep
faultyRoundtripRep = typeRep (Proxy :: Proxy FaultyRoundtrip)

boolRep :: TypeRep
boolRep = typeRep (Proxy :: Proxy Bool)
