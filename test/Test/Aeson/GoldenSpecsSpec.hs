{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Aeson.GoldenSpecsSpec where

import           Control.Exception
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString.Lazy as DBL (readFile, writeFile)
import           Data.List
import           Data.Proxy
import           Data.String.Conversions
import           Data.Text (Text)
import           Prelude hiding (readFile, writeFile, putStrLn)
import           System.Directory
import           Test.Hspec
import           Test.Mockery.Directory
import           Test.QuickCheck.Instances ()

import           Test.Aeson.Internal.GoldenSpecs
import           Test.Utils

textP :: Proxy Text
textP = Proxy

spec :: Spec
spec = do
  describe "goldenSpecs" $ do
    around_ inTempDirectory $ do
      context "when invoked for the first time" $ do
        it "creates passing tests" $ do
          goldenSpecs textP `shouldProduceFailures` 0

        it "writes a golden file" $ do
          _ <- hspecSilently $ goldenSpecs textP
          doesFileExist "golden.json/Text.json" `shouldReturn` True

        it "writes a seed and a list of values into a golden file" $ do
          _ <- hspecSilently $ goldenSpecs textP
          contents <- readFile "golden.json/Text.json"
          case decode' contents :: Maybe (RandomSamples String) of
            Nothing -> throwIO $ ErrorCall "decoding error"
            Just _ -> return ()

        it "warns about not testing anything" $ do
          (_, output) <- hspecSilently $ goldenSpecs textP
          output `shouldContain`
            "WARNING: Running for the first time, not testing anything"

        it "writes JSON pretty-printed" $ do
          _ <- hspecSilently $ goldenSpecs textP
          samplesString <- readFile "golden.json/Text.json"
          samples :: RandomSamples Text <-
            either (throwIO . ErrorCall) return
            (eitherDecode samplesString)
          let prefix = unlines $
                 "{" :
                ("    \"seed\": " ++ show (seed samples) ++ ",") :
                 "    \"samples\": [" :
                []
          cs samplesString `shouldStartWith` prefix

      context "when invoked for the second time" $ do
        it "creates passing tests for correct encodings" $ do
          _ <- hspecSilently $ goldenSpecs textP
          goldenSpecs textP `shouldProduceFailures` 0

        it "creates failing tests for incorrect encodings of RandomSamples" $ do
          createDirectoryIfMissing True "golden.json"
          writeFile "golden.json/Text.json" "foo"
          goldenSpecs textP `shouldProduceFailures` 1

        context "when encodings changed" $ do
          it "creates failing tests for incorrect encodings of elements" $ do
            createGoldenfile (Proxy :: Proxy Int) "golden.json/Text.json"
            goldenSpecs textP `shouldProduceFailures` 1

          let createFaultySamples = do
                createDirectoryIfMissing True "golden.json"
                let faultySamples :: RandomSamples Text
                    faultySamples = RandomSamples 42 (take 200 (cycle ["foo", "bar"]))
                writeFile "golden.json/Text.json" (encode faultySamples)

          it "creates failing tests for correct encodings of wrong values" $ do
            createFaultySamples
            goldenSpecs textP `shouldProduceFailures` 1

          it "writes a new file for easy manual comparison" $ do
            createDirectoryIfMissing True "golden.json"
            let faultySamples :: RandomSamples Int
                faultySamples = RandomSamples 42 (take 200 [1 ..])
            writeFile "golden.json/Text.json" (encode faultySamples)
            _ <- hspecSilently $ goldenSpecs textP

            testSeedSamples <- mkRandomSamples textP 42
            readFile "golden.json/Text.faulty.json" `shouldReturn`
              encodePretty testSeedSamples

          it "mentions the created file in the output" $ do
            createFaultySamples
            (_, output) <- hspecSilently $ goldenSpecs textP
            output `shouldContain` "golden.json/Text.faulty.json"
