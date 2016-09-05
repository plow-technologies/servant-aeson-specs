# Revision history for servant-aeson-specs

## 0.5.1.0  -- 2016-09-05

* Add missing pattern matches for `HasGenericSpecs`.

## 0.5.0.0  -- 2016-09-05

* Remove `Test.Aeson.GenericSpecs`, `Test.Aeson.Internal.GoldenSpecs` and `Test.Aeson.Internal.RoundtripSpecs`.
* Allow custom settings for sample size, directory name, and option of whether or not to include the module name in the directory for `apiGoldenSpecs` and `apiSpecs`.
* Depend on hspec-golden-aeson and quickcheck-arbitrary-adt.
* Add more documentation to `Servant.Aeson.Internal`.
* To do: take advantage of functionality from quickcheck-arbitrary-adt.
