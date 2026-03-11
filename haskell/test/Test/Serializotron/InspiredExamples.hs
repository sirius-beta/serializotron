{-# LANGUAGE OverloadedStrings #-}

module Test.Serializotron.InspiredExamples where

import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Serializotron
import Serializotron.InspiredExamples
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty
import Test.Tasty.HUnit

inspiredExamplesTests :: TestTree
inspiredExamplesTests =
  testGroup
    "Inspired Examples Tests"
    [ testCase "Organization serialization roundtrip" testOrganizationRoundtrip,
      testCase "Business process serialization roundtrip" testBusinessProcessRoundtrip,
      testCase "Tournament serialization roundtrip" testTournamentRoundtrip,
      testCase "Complex nested structure with deduplication" testComplexDeduplication,
      testCase "Large structure with aggressive compression" testLargeStructure
    ]

testOrganizationRoundtrip :: Assertion
testOrganizationRoundtrip = withSystemTempDirectory "szt-test" $ \tmpDir -> do
  let filepath = tmpDir </> "org.szt"

  -- Save the example organization
  saveSzt filepath exampleOrganization

  -- Load it back
  result <- loadSzt filepath

  case result of
    Left err -> assertFailure $ "Failed to load: " ++ show err
    Right loaded -> loaded @?= exampleOrganization

testBusinessProcessRoundtrip :: Assertion
testBusinessProcessRoundtrip = withSystemTempDirectory "szt-test" $ \tmpDir -> do
  let filepath = tmpDir </> "process.szt"

  -- Save the example business process
  saveSzt filepath exampleBusinessProcess

  -- Load it back
  result <- loadSzt filepath

  case result of
    Left err -> assertFailure $ "Failed to load: " ++ show err
    Right loaded -> loaded @?= exampleBusinessProcess

testTournamentRoundtrip :: Assertion
testTournamentRoundtrip = withSystemTempDirectory "szt-test" $ \tmpDir -> do
  let filepath = tmpDir </> "tournament.szt"

  -- Save the example tournament
  saveSzt filepath exampleTournament

  -- Load it back
  result <- loadSzt filepath

  case result of
    Left err -> assertFailure $ "Failed to load: " ++ show err
    Right loaded -> loaded @?= exampleTournament

testComplexDeduplication :: Assertion
testComplexDeduplication = withSystemTempDirectory "szt-test" $ \tmpDir -> do
  let filepath = tmpDir </> "complex.szt"

  -- Create a structure with lots of repeated components
  let sharedDept = Department "shared" "Shared Department" ["A", "B"] (Map.singleton "shared" "true")
      org1 =
        Organization
          { departments = Map.fromList [("D1", sharedDept), ("D2", sharedDept)],
            budgetAllocations = Map.singleton ("D1", "D2") 1000.0,
            orgMetrics = OrgMetrics True True (Just 2) 1000.0
          }
      org2 =
        Organization
          { departments = Map.fromList [("D3", sharedDept), ("D4", sharedDept)],
            budgetAllocations = Map.singleton ("D3", "D4") 1000.0,
            orgMetrics = OrgMetrics True True (Just 2) 1000.0
          }
      complexStructure = [org1, org2, org1, org2] -- Repeated organizations

  -- Save with aggressive deduplication
  saveSztCompressedAggressive filepath complexStructure

  -- Load it back
  result <- loadSzt filepath

  case result of
    Left err -> assertFailure $ "Failed to load: " ++ show err
    Right loaded -> loaded @?= complexStructure

testLargeStructure :: Assertion
testLargeStructure = withSystemTempDirectory "szt-test" $ \tmpDir -> do
  -- Create a large organization with many departments
  let createDept i = ("dept" <> Text.pack (show i), Department ("dept" <> Text.pack (show i)) ("Department " <> Text.pack (show i)) [] Map.empty)
      largeOrg =
        Organization
          { departments = Map.fromList $ map createDept [1 .. 100 :: Int],
            budgetAllocations = Map.empty,
            orgMetrics = OrgMetrics True False Nothing 0
          }

  -- Create multiple copies to test deduplication
  let largeStructure = replicate 10 largeOrg

  -- Test both compression strategies
  saveSzt (tmpDir </> "large_none.szt") largeStructure
  saveSztCompressed (tmpDir </> "large_default.szt") largeStructure
  saveSztCompressedAggressive (tmpDir </> "large_aggressive.szt") largeStructure

  -- Load back the aggressively compressed version
  result <- loadSzt (tmpDir </> "large_aggressive.szt")

  case result of
    Left err -> assertFailure $ "Failed to load: " ++ show err
    Right loaded -> loaded @?= largeStructure
