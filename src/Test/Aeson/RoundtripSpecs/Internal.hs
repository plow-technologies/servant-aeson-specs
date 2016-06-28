{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.RoundtripSpecs.Internal where

import           Control.Arrow
import           Control.Exception
import qualified Data.Aeson as Aeson
import           Data.Aeson as Aeson hiding (encode)
import           Data.ByteString.Lazy
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck

genericAesonRoundtrip :: forall a .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Spec
genericAesonRoundtrip proxy = genericAesonRoundtripWithNote proxy Nothing

genericAesonRoundtripWithNote :: forall a .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Maybe String -> Spec
genericAesonRoundtripWithNote proxy mNote = do
  let note = maybe "" (" " ++) mNote
  describe ("JSON encoding of " ++ show (typeRep proxy) ++ note) $ do
    it "allows to encode values with aeson and read them back" $ do
      shouldBeIdentity proxy $
        Aeson.encode >>> aesonDecodeIO

shouldBeIdentity :: (Eq a, Show a, Arbitrary a) =>
  Proxy a -> (a -> IO a) -> Property
shouldBeIdentity Proxy function =
  property $ \ (a :: a) -> do
    function a `shouldReturn` a

aesonDecodeIO :: FromJSON a => ByteString -> IO a
aesonDecodeIO bs = case eitherDecode bs of
  Right a -> return a
  Left msg -> throwIO $ ErrorCall
    ("aeson couldn't parse value: " ++ msg)
