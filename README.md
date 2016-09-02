# servant-aeson-specs

`servant-aeson-specs` allows you to generically obtain tests to make sure that
the types you use in your servant applications can correctly be converted to
JSON and read back to Haskell using `aeson`.

## Backwards compatibility with 0.4.1

If you already have some golden files created by 0.4.1, there are two options
to maintain backwards compatibility. You can either move the files into a
directory called `golden` and set the `sampleSize` to 200:

```
apiGoldenSpecsWithSettings (defaultSettings { sampleSize = 200}) (Proxy :: Proxy SomeType)
```

or if you want to keep the golden files in `golden.json` you can do the following:

```
apiGoldenSpecsWithSettings (defaultSettings { goldenDirectoryOption = CustomDirectoryName "golden.json"
                                            , sampleSize = 200}) (Proxy :: Proxy SomeType)
```
