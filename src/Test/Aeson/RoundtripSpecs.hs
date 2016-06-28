{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.RoundtripSpecs (
  genericAesonRoundtrip,
  shouldBeIdentity,

  -- * re-exports
  Proxy(..),
) where

import           Data.Proxy

import           Test.Aeson.RoundtripSpecs.Internal
