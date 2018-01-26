# Revision history for servant-aeson-specs

## 0.6.1.0 -- 2018-01-25

* Increase dependency version for servant to < 0.13.

## 0.6.0.0  -- 2018-01-04

* Increase dependency version for hspec-golden-aeson and quickcheck-arbitrary-adt. May break dependencies because of slight changes in their type classes.

## 0.5.2.0  -- 2016-09-26

* Support servant-0.9.

## 0.5.1.1  -- 2016-09-07

* Lighten depedency restriction for hspec-golden-aeson and quickcheck-arbitrary-adt.

## 0.5.1.0  -- 2016-09-05

* Add missing pattern matches for `HasGenericSpecs`.

## 0.5.0.0  -- 2016-09-05

* Remove `Test.Aeson.GenericSpecs`, `Test.Aeson.Internal.GoldenSpecs` and `Test.Aeson.Internal.RoundtripSpecs`.
* Allow custom settings for sample size, directory name, and option of whether or not to include the module name in the directory for `apiGoldenSpecs` and `apiSpecs`.
* Depend on hspec-golden-aeson and quickcheck-arbitrary-adt.
* Add more documentation to `Servant.Aeson.Internal`.
* To do: take advantage of functionality from quickcheck-arbitrary-adt.
