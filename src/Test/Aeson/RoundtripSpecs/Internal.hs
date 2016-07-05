{-# LANGUAGE ScopedTypeVariables #-}

-- | Internal module, use at your own risk.
module Test.Aeson.RoundtripSpecs.Internal where

import           Control.Arrow
import           Control.Exception
import qualified Data.Aeson as Aeson
import           Data.Aeson as Aeson hiding (encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.Typeable
import           Test.Hspec
import           Test.QuickCheck

-- | Allows to obtain a roundtrip test to check whether values of the given type
-- can be successfully converted to JSON and back.
--
-- 'roundtripSpecs' will
--
-- - create random values (using 'Arbitrary'),
-- - convert them into JSON (using 'ToJSON'),
-- - read them back into Haskell (using 'FromJSON') and
-- - make sure that the result is the same as the value it started with
--   (using 'Eq').
roundtripSpecs :: forall a .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Spec
roundtripSpecs proxy = genericAesonRoundtripWithNote proxy Nothing

genericAesonRoundtripWithNote :: forall a .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Maybe String -> Spec
genericAesonRoundtripWithNote proxy mNote = do
  let note = maybe "" (" " ++) mNote
  describe ("JSON encoding of " ++ addBrackets (show (typeRep proxy)) ++ note) $ do
    it "allows to encode values with aeson and read them back" $ do
      shouldBeIdentity proxy $
        Aeson.encode >>> aesonDecodeIO

addBrackets :: String -> String
addBrackets s =
  if ' ' `elem` s
    then "(" ++ s ++ ")"
    else s

-- | [hspec](http://hspec.github.io/) style combinator to easily write tests
-- that check the a given operation returns the same value it was given, e.g.
-- roundtrip tests.
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
