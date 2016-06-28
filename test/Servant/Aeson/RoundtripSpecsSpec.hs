{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Aeson.RoundtripSpecsSpec where

import           Data.Typeable
import           Servant.API
import           System.IO
import           System.IO.Temp
import           Test.Hspec
import           Test.Hspec.Core.Runner

import           Servant.Aeson.RoundtripSpecs
import           Test.Aeson.RoundtripSpecsSpec

-- ignores the Summary
hspecOutput :: Spec -> IO String
hspecOutput spec =
  withSystemTempFile "servant-aeson-specs" $ \ file handle -> do
    let config = defaultConfig{
          configOutputFile = Left handle
        }
    _ <- hspecWithResult config spec
    hClose handle
    readFile file

spec :: Spec
spec = do
  describe "roundtripSpecs" $ do
    it "detects failures in types from ReqBody" $ do
      roundtripSpecs reqBodyFailApi `shouldTestAs`
        Summary 2 1

    it "detects failures in types from Get" $ do
      roundtripSpecs getFailApi `shouldTestAs`
        Summary 1 1

    context "when it finds a list of something" $ do
      it "returns only the element type" $ do
        usedTypes getBoolList `shouldBe` [boolRep]

      it "mentions that the type was wrapped in a list" $ do
        output <- hspecOutput $ roundtripSpecs getBoolList
        output `shouldContain` "(as element-type in [])"

  describe "usedTypes" $ do
    it "extracts types from ReqBody" $ do
      usedTypes reqBodyFailApi `shouldMatchList`
        [faultyRoundtripRep, boolRep]

    it "extracts types from Get" $ do
      usedTypes getFailApi `shouldBe`
        [faultyRoundtripRep]

    it "traverses :>" $ do
      usedTypes (Proxy :: Proxy ("foo" :> Get '[JSON] FaultyRoundtrip)) `shouldBe`
        [faultyRoundtripRep]

    it "traverses :<|>" $ do
      usedTypes reqBodyFailApi `shouldMatchList` [faultyRoundtripRep, boolRep]

    it "returns types only ones (i.e. nubbed)" $ do
      usedTypes doubleTypesApi `shouldBe` [boolRep]

    it "returns types sorted by name" $ do
      usedTypes reqBodyFailApi `shouldBe` [boolRep, faultyRoundtripRep]

reqBodyFailApi :: Proxy (ReqBody '[JSON] FaultyRoundtrip :> Get '[JSON] Bool)
reqBodyFailApi = Proxy

getFailApi :: Proxy (Get '[JSON] FaultyRoundtrip)
getFailApi = Proxy

getBoolList :: Proxy (Get '[JSON] [Bool])
getBoolList = Proxy

doubleTypesApi :: Proxy (ReqBody '[JSON] Bool :> Get '[JSON] Bool)
doubleTypesApi = Proxy

faultyRoundtripRep :: TypeRep
faultyRoundtripRep = typeRep (Proxy :: Proxy FaultyRoundtrip)

boolRep :: TypeRep
boolRep = typeRep (Proxy :: Proxy Bool)
