
-- | If you're using [servant](http://haskell-servant.readthedocs.org/) with
-- either [servant-client](http://hackage.haskell.org/package/servant-client)
-- or [servant-server](http://hackage.haskell.org/package/servant-server) there
-- will be types included in your APIs that servant will convert to and from
-- JSON. (At least for most common APIs.) 'apiRoundtripSpecs' allows you to
-- generically obtain a test-suite, that makes sure for those types, that they
-- can be serialized to JSON and read back to Haskell successfully.
--
-- Here's an example:
--
-- >>> :set -XTypeOperators
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
--
-- >>> import Servant.API
-- >>> import Test.Hspec (hspec)
-- >>> import GHC.Generics (Generic)
-- >>> import Data.Aeson (ToJSON, FromJSON)
-- >>> import Test.QuickCheck (Arbitrary(..), oneof)
--
-- >>> data Foo = Foo { a :: String, b :: Int } deriving (Eq, Show, Generic)
-- >>> instance FromJSON Foo
-- >>> instance ToJSON Foo
-- >>> :{
--   instance Arbitrary Foo where
--     arbitrary = Foo <$> arbitrary <*> arbitrary
-- :}
--
-- >>> data Bar = BarA | BarB { bar :: Bool } deriving (Eq, Show, Generic)
-- >>> instance FromJSON Bar
-- >>> instance ToJSON Bar
-- >>> :{
--   instance Arbitrary Bar where
--     arbitrary = oneof $
--       pure BarA :
--       (BarB <$> arbitrary) :
--       []
-- :}
--
--
-- >>> type Api = "post" :> ReqBody '[JSON] Foo :> Get '[JSON] Bar
-- >>> let api = Proxy :: Proxy Api
-- >>> hspec $ apiRoundtripSpecs api
-- <BLANKLINE>
-- JSON encoding of Bar
--   allows to encode values with aeson and read them back
-- JSON encoding of Foo
--   allows to encode values with aeson and read them back
-- <BLANKLINE>
-- Finished in ... seconds
-- 2 examples, 0 failures

module Servant.Aeson.GenericSpecs (
  apiRoundtripSpecs,
  apiGoldenSpecs,
  apiSpecs,
  usedTypes,

  -- * re-exports
  Proxy(..),
) where

import           Data.Proxy

import           Servant.Aeson.Internal
