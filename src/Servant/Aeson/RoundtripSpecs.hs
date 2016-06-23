{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Servant.Aeson.RoundtripSpecs (
  allRoundtripSpec,
  usedTypes,
  HasUsedTypes(..),

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

allRoundtripSpec :: (HasRoundtripSpecs (UsedTypes api), HasUsedTypes api) =>
  Proxy api -> Spec
allRoundtripSpec = roundtripSpecs . usedTypes

class HasRoundtripSpecs (types :: [*]) where
  roundtripSpecs :: Proxy types -> Spec

instance forall a r .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a, HasRoundtripSpecs r) =>
  HasRoundtripSpecs (a ': r) where

  roundtripSpecs Proxy = do
    genericAesonRoundtrip (Proxy :: Proxy a)
    roundtripSpecs (Proxy :: Proxy r)

instance HasRoundtripSpecs '[] where
  roundtripSpecs Proxy = return ()

-- * UsedTypes

usedTypes :: Proxy api -> Proxy (UsedTypes api)
usedTypes Proxy = Proxy

class HasUsedTypes api where
  type UsedTypes api :: [*]

instance HasUsedTypes (a :<|> b) where
  type UsedTypes (a :<|> b) = UsedTypes a ++ UsedTypes b

instance HasUsedTypes (Get contentTypes response) where
  type UsedTypes (Get contentTypes response) = '[response]

instance HasUsedTypes ((path :: Symbol) :> api) where
  type UsedTypes (path :> api) = UsedTypes api

instance HasUsedTypes (ReqBody contentTypes body :> api) where
  type UsedTypes (ReqBody contentTypes body :> api) = body ': UsedTypes api

-- * type level (++)

type family (a :: [*]) ++ (b :: [*]) where
  '[] ++ b = b
  (a ': r) ++ b = a ': (r ++ b)
