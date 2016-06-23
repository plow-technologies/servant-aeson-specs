{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.RoundtripSpecs (
  genericAesonRoundtrip,
  shouldBeIdentity,

  -- * re-exports
  Proxy(..),
) where

import           Control.Arrow
import qualified Data.Aeson as Aeson
import           Data.Aeson hiding (encode)
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck

import           Test.Aeson.RoundtripSpecs.Internal

genericAesonRoundtrip :: forall a .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Spec
genericAesonRoundtrip proxy = do
  describe ("JSON encoding of " ++ show (typeRep proxy)) $ do
    it "allows to encode values with aeson and read them back" $ do
      shouldBeIdentity proxy $
        Aeson.encode >>> aesonDecodeIO
