{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module, use at your own risk.
module Servant.Aeson.RoundtripSpecs.Internal where

import           Data.Aeson
import           Data.Function
import           Data.List
import           Data.Proxy
import           Data.Typeable
import           GHC.TypeLits
import           Servant.API
import           Test.Hspec
import           Test.QuickCheck

import           Test.Aeson.GoldenSpecs.Internal
import           Test.Aeson.RoundtripSpecs.Internal

-- | Allows to obtain roundtrip tests for JSON serialization for all types used
-- in a [servant](http://haskell-servant.readthedocs.org/) api.
--
-- See also 'Test.Aeson.RoundtripSpecs.genericAesonRoundtrip'.
roundtripSpecs :: (HasRoundtripSpecs api) => Proxy api -> Spec
roundtripSpecs = sequence_ . map roundtrip . mkRoundtripSpecs

-- | Allows to retrieve a list of all used types in a
-- [servant](http://haskell-servant.readthedocs.org/) api as 'TypeRep's.
usedTypes :: (HasRoundtripSpecs api) => Proxy api -> [TypeRep]
usedTypes = map typ . mkRoundtripSpecs

mkRoundtripSpecs :: (HasRoundtripSpecs api) => Proxy api -> [TypeSpec]
mkRoundtripSpecs = normalize . collectRoundtripSpecs
  where
    normalize = nubBy ((==) `on` typ) . sortBy (compare `on` (show . typ))

class HasRoundtripSpecs api where
  collectRoundtripSpecs :: Proxy api -> [TypeSpec]

instance (HasRoundtripSpecs a, HasRoundtripSpecs b) => HasRoundtripSpecs (a :<|> b) where
  collectRoundtripSpecs Proxy =
    collectRoundtripSpecs (Proxy :: Proxy a) ++
    collectRoundtripSpecs (Proxy :: Proxy b)

instance (MkSpec response) =>
  HasRoundtripSpecs (Get contentTypes response) where

  collectRoundtripSpecs Proxy = do
    mkSpec (Proxy :: Proxy response)

instance (MkSpec response) =>
  HasRoundtripSpecs (Post contentTypes response) where

  collectRoundtripSpecs Proxy = mkSpec (Proxy :: Proxy response)

instance (MkSpec body, HasRoundtripSpecs api) =>
  HasRoundtripSpecs (ReqBody contentTypes body :> api) where

  collectRoundtripSpecs Proxy =
    mkSpec (Proxy :: Proxy body) ++
    collectRoundtripSpecs (Proxy :: Proxy api)

instance HasRoundtripSpecs api => HasRoundtripSpecs ((path :: Symbol) :> api) where
  collectRoundtripSpecs Proxy = collectRoundtripSpecs (Proxy :: Proxy api)

instance HasRoundtripSpecs api => HasRoundtripSpecs (MatrixParam name a :> api) where
  collectRoundtripSpecs Proxy = collectRoundtripSpecs (Proxy :: Proxy api)

data TypeSpec
  = TypeSpec {
    typ :: TypeRep,
    roundtrip :: Spec,
    golden :: Spec
  }

-- 'mkSpec' has to be implemented as a method of a separate class, because we
-- want to be able to have a specialized implementation for lists.
class MkSpec a where
  mkSpec :: Proxy a -> [TypeSpec]

instance (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) => MkSpec a where

  mkSpec proxy = pure $
    TypeSpec {
      typ = typeRep proxy,
      roundtrip = genericAesonRoundtrip proxy,
      golden = goldenSpecs proxy
    }

-- This will only test json serialization of the element type. As we trust aeson
-- to do the right thing for lists, we don't need to test that. (This speeds up
-- test suites immensely.)
instance {-# OVERLAPPING #-}
  (Eq a, Show a, Typeable a, Arbitrary a, ToJSON a, FromJSON a) =>
  MkSpec [a] where

  mkSpec Proxy = pure $
    TypeSpec {
      typ = typeRep proxy,
      roundtrip = genericAesonRoundtripWithNote proxy (Just note),
      golden = goldenSpecsWithNote proxy (Just note)
    }
    where
      proxy = Proxy :: Proxy a
      note = "(as element-type in [])"
