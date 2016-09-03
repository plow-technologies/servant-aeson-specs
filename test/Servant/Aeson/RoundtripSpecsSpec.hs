{-# LANGUAGE CPP #-}
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

import           Servant.Aeson.GenericSpecs
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
        usedTypes getListOfBool `shouldBe` [boolRep]

      it "mentions that the type was wrapped in a list" $ do
        output <- hspecOutput $ apiRoundtripSpecs getListOfBool
        output `shouldContain` "(as element-type in [])"

    context "when it finds a Maybe" $ do
      it "returns only the element type" $ do
        usedTypes (Proxy :: Proxy (Get '[JSON] (Maybe Bool)))
          `shouldBe` [boolRep]

    context "when it finds ()" $ do
      it "does not return anything" $ do
        usedTypes (Proxy :: Proxy (Get '[JSON] ()))
          `shouldBe` []

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

    it "works for Post" $ do
      usedTypes postApi `shouldBe` [boolRep]

    it "ignores response headers" $ do
      usedTypes responseHeadersApi `shouldBe` [boolRep]

    matrixParamTest

    noContentTest

    authProtectTest

reqBodyFailApi :: Proxy (ReqBody '[JSON] FaultyRoundtrip :> Get '[JSON] Bool)
reqBodyFailApi = Proxy

getFailApi :: Proxy (Get '[JSON] FaultyRoundtrip)
getFailApi = Proxy

postApi :: Proxy (Post '[JSON] Bool)
postApi = Proxy

getListOfBool :: Proxy (Get '[JSON] [Bool])
getListOfBool = Proxy

doubleTypesApi :: Proxy (ReqBody '[JSON] Bool :> Get '[JSON] Bool)
doubleTypesApi = Proxy

faultyRoundtripRep :: TypeRep
faultyRoundtripRep = typeRep (Proxy :: Proxy FaultyRoundtrip)

boolRep :: TypeRep
boolRep = typeRep (Proxy :: Proxy Bool)

responseHeadersApi :: Proxy (Post '[JSON] (Headers '[Header "Cookie" Int] Bool))
responseHeadersApi= Proxy

matrixParamTest :: Spec
#if !MIN_VERSION_servant(0, 5, 0)
matrixParamTest = do
  it "supports MatrixParam" $ do
    usedTypes matrixParamApi `shouldBe` [boolRep]

matrixParamApi :: Proxy (MatrixParam "foo" String :> Get '[JSON] Bool)
matrixParamApi = Proxy
#else
matrixParamTest = return ()
#endif

noContentTest :: Spec
#if MIN_VERSION_servant(0, 5, 0)
noContentTest = do
  it "works for Apis containing NoContent" $ do
    usedTypes noContentApi `shouldBe` [boolRep]

noContentApi :: Proxy (ReqBody '[JSON] Bool :> GetNoContent '[JSON] NoContent)
noContentApi = Proxy
#else
noContentTest = return ()
#endif

authProtectTest :: Spec
#if MIN_VERSION_servant(0, 5, 0)
authProtectTest =  do
  it "traverses AuthProtect" $ do
    usedTypes authProtectApi `shouldBe` [boolRep]

authProtectApi :: Proxy (AuthProtect "scheme" :> Post '[JSON] Bool)
authProtectApi = Proxy
#else
authProtectTest = return ()
#endif
