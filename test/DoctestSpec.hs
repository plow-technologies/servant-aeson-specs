
module DoctestSpec where

import           System.Directory
import           System.FilePath
import           Test.DocTest
import           Test.Hspec
import           Test.Mockery.Directory

spec :: Spec
spec = do
  it "doctest" $ do
    dir <- getCurrentDirectory
    inTempDirectory $ do
      doctest [dir </> "src/Servant/Aeson/GenericSpecs.hs"]
