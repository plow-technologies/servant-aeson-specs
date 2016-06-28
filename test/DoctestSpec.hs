
module DoctestSpec where

import           Test.DocTest
import           Test.Hspec

spec :: Spec
spec = do
  it "doctest" $ do
    doctest ["src/Servant/Aeson/RoundtripSpecs.hs"]
