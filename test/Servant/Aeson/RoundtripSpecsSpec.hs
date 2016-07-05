{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Servant.Aeson.RoundtripSpecsSpec where

import           Data.List
import           Data.Typeable
import           Servant.API
import           System.Directory
import           System.IO
import           System.IO.Temp
import           Test.Hspec
import           Test.Hspec.Core.Runner
import           Test.Mockery.Directory

import           Servant.Aeson.RoundtripSpecs
import           Test.Aeson.RoundtripSpecsSpec
import           Test.Utils

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
  describe "apiRoundtripSpecs" $ do
    it "detects failures in types from ReqBody" $ do
      apiRoundtripSpecs reqBodyFailApi `shouldTestAs`
        Summary 2 1

    it "detects failures in types from Get" $ do
      apiRoundtripSpecs getFailApi `shouldTestAs`
        Summary 1 1

    context "when it finds a list of something" $ do
      it "returns only the element type" $ do
        usedTypes getBoolList `shouldBe` [boolRep]

      it "mentions that the type was wrapped in a list" $ do
        output <- hspecOutput $ apiRoundtripSpecs getBoolList
        output `shouldContain` "(as element-type in [])"

    it "does not write any files" $ do
      inTempDirectory $ do
        _ <- hspecSilently $ apiRoundtripSpecs reqBodyFailApi
        sort <$> getDirectoryContents "." `shouldReturn` [".", ".."]

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
