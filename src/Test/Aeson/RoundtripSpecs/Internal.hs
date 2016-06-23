{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.RoundtripSpecs.Internal where

import           Control.Exception
import           Data.Aeson hiding (encode)
import           Data.ByteString.Lazy
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck

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
