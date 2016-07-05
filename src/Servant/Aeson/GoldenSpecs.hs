
module Servant.Aeson.GoldenSpecs where

import           Data.Proxy
import           Test.Hspec

import           Servant.Aeson.RoundtripSpecs.Internal

apiGoldenSpecs :: HasRoundtripSpecs api => Proxy api -> Spec
apiGoldenSpecs proxy = sequence_ $ map golden $ mkRoundtripSpecs proxy
