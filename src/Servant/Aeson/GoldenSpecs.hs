
module Servant.Aeson.GoldenSpecs where

import           Data.Proxy
import           Test.Hspec

import           Servant.Aeson.RoundtripSpecs.Internal

servantGoldenSpecs :: HasRoundtripSpecs api => Proxy api -> Spec
servantGoldenSpecs proxy = sequence_ $ map golden $ mkRoundtripSpecs proxy
