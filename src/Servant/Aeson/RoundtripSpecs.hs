{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Aeson.RoundtripSpecs (
  roundtripSpecs,
  HasRoundtripSpecs(..),
  usedTypes,

  -- * re-exports
  Proxy(..),
) where

import           Data.Aeson
import           Data.Proxy
import           Data.Typeable
import           GHC.TypeLits
import           Servant.API
import           Test.Hspec
import           Test.QuickCheck

import           Test.Aeson.RoundtripSpecs.Internal

roundtripSpecs :: (HasRoundtripSpecs api) => Proxy api -> Spec
roundtripSpecs = sequence_ . map snd . mkRoundtripSpecs

usedTypes :: (HasRoundtripSpecs api) => Proxy api -> [TypeRep]
usedTypes = map fst . mkRoundtripSpecs

class HasRoundtripSpecs api where
  mkRoundtripSpecs :: Proxy api -> [(TypeRep, Spec)]

instance (HasRoundtripSpecs a, HasRoundtripSpecs b) => HasRoundtripSpecs (a :<|> b) where
  mkRoundtripSpecs Proxy =
    mkRoundtripSpecs (Proxy :: Proxy a) ++
    mkRoundtripSpecs (Proxy :: Proxy b)

instance (MkSpec response) =>
  HasRoundtripSpecs (Get contentTypes response) where

  mkRoundtripSpecs Proxy = do
    mkSpec (Proxy :: Proxy response)

instance (MkSpec response) =>
  HasRoundtripSpecs (Post contentTypes response) where

  mkRoundtripSpecs Proxy = mkSpec (Proxy :: Proxy response)

instance (MkSpec body, HasRoundtripSpecs api) =>
  HasRoundtripSpecs (ReqBody contentTypes body :> api) where

  mkRoundtripSpecs Proxy =
    mkSpec (Proxy :: Proxy body) ++
    mkRoundtripSpecs (Proxy :: Proxy api)

instance HasRoundtripSpecs api => HasRoundtripSpecs ((path :: Symbol) :> api) where
  mkRoundtripSpecs Proxy = mkRoundtripSpecs (Proxy :: Proxy api)

instance HasRoundtripSpecs api => HasRoundtripSpecs (MatrixParam name a :> api) where
  mkRoundtripSpecs Proxy = mkRoundtripSpecs (Proxy :: Proxy api)

-- 'mkSpec' has to be implemented as a method of a separate class, because we
-- want to be able to have a specialized implementation for lists.
class MkSpec a where
  mkSpec :: Proxy a -> [(TypeRep, Spec)]

instance (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) => MkSpec a where

  mkSpec proxy = [(typeRep proxy, genericAesonRoundtrip proxy)]

-- This will only test json serialization of the element type. As we trust aeson
-- to do the right thing for lists, we don't need to test that. (This speeds up
-- test suites immensely.)
instance {-# OVERLAPPING #-}
  (Eq a, Show a, Typeable a, Arbitrary a, ToJSON a, FromJSON a) =>
  MkSpec [a] where

  mkSpec Proxy = [(typeRep proxy, genericAesonRoundtripWithNote proxy (Just note))]
    where
      proxy = Proxy :: Proxy a
      note = "(as element-type in [])"
