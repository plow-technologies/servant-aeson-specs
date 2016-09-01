# Revision history for servant-aeson-specs

## 0.4.2.0  -- 2016-09-01

* Remove `Test.Aeson.GenericSpecs`, `Test.Aeson.Internal.GoldenSpecs` and `Test.Aeson.Internal.RoundtripSpecs`.
* Allow custom settings for sample size, directory name, and option of whether or not to include the module name in the directory for `apiGoldenSpecs` and `apiSpecs`.
* Depend on hspec-golden-aeson and quickcheck-arbitrary-adt.
