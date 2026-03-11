{-# LANGUAGE OverloadedStrings #-}

module Test.Serializotron.ShallowExampleTests where

import Data.HashSet qualified as HS
import Data.Text qualified as Text
import Serializotron
import Serializotron.ShallowExample
import System.Directory (getFileSize)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

shallowExampleTests :: TestTree
shallowExampleTests =
  testGroup
    "Shallow Serialization Example Tests"
    [ testCase "Grid topology model roundtrip" testGridTopologyRoundtrip,
      testCase "Network topology model roundtrip" testNetworkModelRoundtrip,
      testCase "Large model with shared topology roundtrip" testLargeModelRoundtrip,
      testCase "Deduplication effectiveness on shared topology" testDeduplicationEffectiveness,
      testCase "Shallow identifier uniqueness" testShallowIdentifierUniqueness
    ]

testGridTopologyRoundtrip :: Assertion
testGridTopologyRoundtrip = withSystemTempDirectory "szt-test" $ \tmpDir -> do
  let filepath = tmpDir </> "grid_model.szt"

  saveSzt filepath exampleModelWithSharedTopology

  result <- loadSzt filepath

  case result of
    Left err -> assertFailure $ "Failed to load: " ++ show err
    Right loaded -> loaded @?= exampleModelWithSharedTopology

testNetworkModelRoundtrip :: Assertion
testNetworkModelRoundtrip = withSystemTempDirectory "szt-test" $ \tmpDir -> do
  let filepath = tmpDir </> "network_model.szt"

  saveSzt filepath exampleNetworkModel

  result <- loadSzt filepath

  case result of
    Left err -> assertFailure $ "Failed to load: " ++ show err
    Right loaded -> loaded @?= exampleNetworkModel

testLargeModelRoundtrip :: Assertion
testLargeModelRoundtrip = withSystemTempDirectory "szt-test" $ \tmpDir -> do
  let filepath = tmpDir </> "large_model.szt"

  saveSztCompressedAggressive filepath exampleLargeModelWithDuplication

  result <- loadSzt filepath

  case result of
    Left err -> assertFailure $ "Failed to load: " ++ show err
    Right loaded -> do
      modelName loaded @?= modelName exampleLargeModelWithDuplication
      modelMetadata loaded @?= modelMetadata exampleLargeModelWithDuplication
      length (modelDomains loaded) @?= length (modelDomains exampleLargeModelWithDuplication)
      loaded @?= exampleLargeModelWithDuplication

-- | Test that deduplication significantly reduces file size for models with shared topology.
testDeduplicationEffectiveness :: Assertion
testDeduplicationEffectiveness = withSystemTempDirectory "szt-test" $ \tmpDir -> do
  let filepathNone = tmpDir </> "large_none.szt"
      filepathDefault = tmpDir </> "large_default.szt"
      filepathAggressive = tmpDir </> "large_aggressive.szt"

  saveSzt filepathNone exampleLargeModelWithDuplication
  saveSztCompressed filepathDefault exampleLargeModelWithDuplication
  saveSztCompressedAggressive filepathAggressive exampleLargeModelWithDuplication

  sizeNone <- getFileSize filepathNone
  sizeDefault <- getFileSize filepathDefault
  sizeAggressive <- getFileSize filepathAggressive

  assertBool "Default deduplication should reduce file size" (sizeDefault < sizeNone)
  assertBool
    "Aggressive deduplication should reduce file size at least as much as default"
    (sizeAggressive <= sizeDefault)

  assertBool
    "Default deduplication should achieve significant compression (at least 100x)"
    (fromIntegral sizeNone / fromIntegral sizeDefault > (100.0 :: Double))
  assertBool
    "Aggressive deduplication should achieve significant compression (at least 100x)"
    (fromIntegral sizeNone / fromIntegral sizeAggressive > (100.0 :: Double))

testShallowIdentifierUniqueness :: Assertion
testShallowIdentifierUniqueness = do
  let topology1 = exampleGridTopology
      topology2 = exampleNetworkTopology

      domain1 = mkDomain topology1 (HS.fromList ["(0,0)", "(0,1)"])
      domain2 = mkDomain topology1 (HS.fromList ["(1,0)", "(1,1)"])
      domain3 = mkDomain topology2 (HS.fromList ["R1", "R2"])

  assertBool
    "Different topologies should have different shallow IDs"
    (shallowIdentifier topology1 /= shallowIdentifier topology2)

  assertBool
    "Domains with different subsets should have different shallow IDs"
    (shallowIdentifier domain1 /= shallowIdentifier domain2)

  assertBool
    "Domains with different topologies should have different shallow IDs"
    (shallowIdentifier domain1 /= shallowIdentifier domain3)

  let domain1Copy = mkDomain topology1 (HS.fromList ["(0,0)", "(0,1)"])
  shallowIdentifier domain1 @?= shallowIdentifier domain1Copy
