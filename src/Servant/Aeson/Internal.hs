{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internal module, use at your own risk.
module Servant.Aeson.Internal where

import           Data.Aeson
import           Data.Function
import           Data.List
import           Data.Proxy
import           Data.Typeable
import           GHC.TypeLits
import           Servant.API
import           Test.Hspec
import           Test.QuickCheck

import           Test.Aeson.Internal.GoldenSpecs
import           Test.Aeson.Internal.RoundtripSpecs

-- | Allows to obtain roundtrip tests for JSON serialization for all types used
-- in a [servant](http://haskell-servant.readthedocs.org/) api.
--
-- See also 'Test.Aeson.GenericSpecs.roundtripSpecs'.
apiRoundtripSpecs :: (HasGenericSpecs api) => Proxy api -> Spec
apiRoundtripSpecs = sequence_ . map roundtrip . mkRoundtripSpecs

-- | Allows to obtain golden tests for JSON serialization for all types used
-- in a [servant](http://haskell-servant.readthedocs.org/) api.
--
-- See also 'Test.Aeson.GenericSpecs.goldenSpecs'.
apiGoldenSpecs :: HasGenericSpecs api => Proxy api -> Spec
apiGoldenSpecs proxy = sequence_ $ map golden $ mkRoundtripSpecs proxy

-- | Combination of 'apiRoundtripSpecs' and 'apiGoldenSpecs'.
apiSpecs :: (HasGenericSpecs api) => Proxy api -> Spec
apiSpecs proxy = sequence_ $ map (\ ts -> roundtrip ts >> golden ts) $ mkRoundtripSpecs proxy

-- | Allows to retrieve a list of all used types in a
-- [servant](http://haskell-servant.readthedocs.org/) api as 'TypeRep's.
usedTypes :: (HasGenericSpecs api) => Proxy api -> [TypeRep]
usedTypes = map typ . mkRoundtripSpecs

mkRoundtripSpecs :: (HasGenericSpecs api) => Proxy api -> [TypeSpec]
mkRoundtripSpecs = normalize . collectRoundtripSpecs
  where
    normalize = nubBy ((==) `on` typ) . sortBy (compare `on` (show . typ))

class HasGenericSpecs api where
  collectRoundtripSpecs :: Proxy api -> [TypeSpec]

instance (HasGenericSpecs a, HasGenericSpecs b) => HasGenericSpecs (a :<|> b) where
  collectRoundtripSpecs Proxy =
    collectRoundtripSpecs (Proxy :: Proxy a) ++
    collectRoundtripSpecs (Proxy :: Proxy b)

-- * http methods

#if MIN_VERSION_servant(0, 5, 0)
instance {-# OVERLAPPABLE #-}
  (MkTypeSpecs response) =>
  HasGenericSpecs (Verb (method :: StdMethod) returnStatus contentTypes response) where

  collectRoundtripSpecs Proxy = do
    mkTypeSpecs (Proxy :: Proxy response)

instance {-# OVERLAPPING #-}
  HasGenericSpecs (Verb (method :: StdMethod) returnStatus contentTypes NoContent) where

  collectRoundtripSpecs Proxy = []

instance (MkTypeSpecs response) =>
  HasGenericSpecs (Verb (method :: StdMethod) returnStatus contentTypes (Headers hs response)) where
  collectRoundtripSpecs Proxy = mkTypeSpecs (Proxy :: Proxy response)

#else
instance {-# OVERLAPPABLE #-}
  (MkTypeSpecs response) =>
  HasGenericSpecs (Get contentTypes response) where

  collectRoundtripSpecs Proxy = do
    mkTypeSpecs (Proxy :: Proxy response)

instance {-# OVERLAPPABLE #-}
  (MkTypeSpecs response) =>
  HasGenericSpecs (Post contentTypes response) where

  collectRoundtripSpecs Proxy = mkTypeSpecs (Proxy :: Proxy response)

instance {-# OVERLAPPING #-}
  (MkTypeSpecs response) =>
  HasGenericSpecs (Get contentTypes (Headers hs response)) where

  collectRoundtripSpecs Proxy = mkTypeSpecs (Proxy :: Proxy response)

instance {-# OVERLAPPING #-}
  (MkTypeSpecs response) =>
  HasGenericSpecs (Post contentTypes (Headers hs response)) where

  collectRoundtripSpecs Proxy = mkTypeSpecs (Proxy :: Proxy response)
#endif

-- * combinators

instance (MkTypeSpecs body, HasGenericSpecs api) =>
  HasGenericSpecs (ReqBody contentTypes body :> api) where

  collectRoundtripSpecs Proxy =
    mkTypeSpecs (Proxy :: Proxy body) ++
    collectRoundtripSpecs (Proxy :: Proxy api)

instance HasGenericSpecs api => HasGenericSpecs ((path :: Symbol) :> api) where
  collectRoundtripSpecs Proxy = collectRoundtripSpecs (Proxy :: Proxy api)

instance HasGenericSpecs api  => HasGenericSpecs (Capture (sym :: Symbol) x :> api) where
  collectRoundtripSpecs Proxy = collectRoundtripSpecs (Proxy :: Proxy api)

#if MIN_VERSION_servant(0, 5, 0)
instance HasGenericSpecs api => HasGenericSpecs (AuthProtect (sym :: Symbol) :> api) where
  collectRoundtripSpecs Proxy = collectRoundtripSpecs (Proxy :: Proxy api)
#else
instance HasGenericSpecs api => HasGenericSpecs (MatrixParam name a :> api) where
  collectRoundtripSpecs Proxy = collectRoundtripSpecs (Proxy :: Proxy api)
#endif

data TypeSpec
  = TypeSpec {
    typ :: TypeRep,
    roundtrip :: Spec,
    golden :: Spec
  }

-- 'mkTypeSpecs' has to be implemented as a method of a separate class, because we
-- want to be able to have a specialized implementation for lists.
class MkTypeSpecs a where
  mkTypeSpecs :: Proxy a -> [TypeSpec]

instance (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) => MkTypeSpecs a where

  mkTypeSpecs proxy = pure $
    TypeSpec {
      typ = typeRep proxy,
      roundtrip = roundtripSpecs proxy,
      golden = goldenSpecs proxy
    }

-- The following instances will only test json serialization of element types.
-- As we trust aeson to do the right thing for standard container types, we
-- don't need to test that. (This speeds up test suites immensely.)

instance {-# OVERLAPPING #-}
  (Eq a, Show a, Typeable a, Arbitrary a, ToJSON a, FromJSON a) =>
  MkTypeSpecs [a] where

  mkTypeSpecs Proxy = pure $
    TypeSpec {
      typ = typeRep proxy,
      roundtrip = genericAesonRoundtripWithNote proxy (Just note),
      golden = goldenSpecsWithNote proxy (Just note)
    }
    where
      proxy = Proxy :: Proxy a
      note = "(as element-type in [])"

instance {-# OVERLAPPING #-}
  (Eq a, Show a, Typeable a, Arbitrary a, ToJSON a, FromJSON a) =>
  MkTypeSpecs (Maybe a) where

  mkTypeSpecs Proxy = pure $
    TypeSpec {
      typ = typeRep proxy,
      roundtrip = genericAesonRoundtripWithNote proxy (Just note),
      golden = goldenSpecsWithNote proxy (Just note)
    }
    where
      proxy = Proxy :: Proxy a
      note = "(as element-type in Maybe)"

-- We trust aeson to be correct for ().
instance {-# OVERLAPPING #-}
  MkTypeSpecs () where

  mkTypeSpecs Proxy = []
