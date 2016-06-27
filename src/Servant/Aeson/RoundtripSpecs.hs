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

import           Test.Aeson.RoundtripSpecs

roundtripSpecs :: (HasRoundtripSpecs api) => Proxy api -> Spec
roundtripSpecs = sequence_ . map snd . mkRoundtripSpecs

usedTypes :: (HasRoundtripSpecs api) => Proxy api -> [TypeRep]
usedTypes = map fst . mkRoundtripSpecs

mkSpec :: (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> [(TypeRep, Spec)]
mkSpec proxy =
  [(typeRep proxy, genericAesonRoundtrip proxy)]

class HasRoundtripSpecs api where
  mkRoundtripSpecs :: Proxy api -> [(TypeRep, Spec)]

instance (HasRoundtripSpecs a, HasRoundtripSpecs b) => HasRoundtripSpecs (a :<|> b) where
  mkRoundtripSpecs p =
    mkRoundtripSpecs (Proxy :: Proxy a) ++
    mkRoundtripSpecs (Proxy :: Proxy b)

instance (Eq response, Show response, Typeable response, Arbitrary response, ToJSON response, FromJSON response) =>
  HasRoundtripSpecs (Get contentTypes response) where

  mkRoundtripSpecs Proxy = do
    mkSpec (Proxy :: Proxy response)

instance (Eq response, Show response, Typeable response, Arbitrary response, ToJSON response, FromJSON response) =>
  HasRoundtripSpecs (Post contentTypes response) where

  mkRoundtripSpecs Proxy = mkSpec (Proxy :: Proxy response)

instance (Eq body, Show body, Typeable body, Arbitrary body, ToJSON body, FromJSON body, HasRoundtripSpecs api) =>
  HasRoundtripSpecs (ReqBody contentTypes body :> api) where

  mkRoundtripSpecs Proxy =
    mkSpec (Proxy :: Proxy body) ++
    mkRoundtripSpecs (Proxy :: Proxy api)

instance HasRoundtripSpecs api => HasRoundtripSpecs ((path :: Symbol) :> api) where
  mkRoundtripSpecs Proxy = mkRoundtripSpecs (Proxy :: Proxy api)

instance HasRoundtripSpecs api => HasRoundtripSpecs (MatrixParam name a :> api) where
  mkRoundtripSpecs Proxy = mkRoundtripSpecs (Proxy :: Proxy api)
