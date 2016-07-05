
module Test.Aeson.GenericSpecs (
  roundtripSpecs,
  goldenSpecs,

  shouldBeIdentity,

  -- * re-exports
  Proxy(..),
) where

import           Data.Proxy

import           Test.Aeson.Internal.GoldenSpecs
import           Test.Aeson.Internal.RoundtripSpecs
