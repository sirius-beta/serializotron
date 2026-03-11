{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Serializotron.Properties where

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Serializotron
import Serializotron.Examples
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

-- | Generate arbitrary primitive values
genPrimitive :: Gen (Either Int (Either Double (Either Text Bool)))
genPrimitive =
  Gen.choice
    [ Left <$> Gen.int (Range.linear (-1000) 1000),
      Right . Left <$> Gen.double (Range.exponentialFloat (-1e6) 1e6),
      Right . Right . Left <$> Gen.text (Range.linear 0 100) Gen.unicode,
      Right . Right . Right <$> Gen.bool
    ]

-- | Generate arbitrary Points
genPoint :: Gen Point
genPoint = Point <$> Gen.int (Range.linear (-100) 100) <*> Gen.int (Range.linear (-100) 100)

-- | Generate arbitrary Colors
genColor :: Gen Color
genColor =
  Gen.choice
    [ pure Red,
      pure Green,
      pure Blue,
      RGB <$> Gen.int (Range.linear 0 255) <*> Gen.int (Range.linear 0 255) <*> Gen.int (Range.linear 0 255)
    ]

-- | Generate arbitrary Optional values
genOptional :: Gen a -> Gen (Optional a)
genOptional gen =
  Gen.choice
    [ pure None,
      Some <$> gen
    ]

-- | Generate arbitrary Result values
genResult :: Gen e -> Gen a -> Gen (Result e a)
genResult genE genA =
  Gen.choice
    [ Error <$> genE,
      Success <$> genA
    ]

-- | Generate arbitrary Trees (bounded depth)
genTree :: Gen a -> Gen (Tree a)
genTree genA =
  Gen.recursive
    Gen.choice
    [pure Leaf]
    [Branch <$> genA <*> genTree genA <*> genTree genA]

-- | Generate arbitrary Lists (bounded length)
genList :: Gen a -> Gen (List a)
genList genA =
  Gen.recursive
    Gen.choice
    [pure Nil]
    [Cons <$> genA <*> genList genA]

-- | Generate arbitrary Persons
genPerson :: Gen Person
genPerson =
  Person
    <$> Gen.text (Range.linear 1 50) Gen.alpha
    <*> Gen.int (Range.linear 18 100)
    <*> ((<> "@example.com") <$> Gen.text (Range.linear 1 20) Gen.alpha)
    <*> Gen.bool

-- | Generate small JsonValue structures
genJsonValue :: Gen JsonValue
genJsonValue =
  Gen.recursive
    Gen.choice
    [ pure JsonNull,
      JsonBool <$> Gen.bool,
      JsonNumber <$> Gen.double (Range.exponentialFloat (-1e6) 1e6),
      JsonString <$> Gen.text (Range.linear 0 50) Gen.unicode
    ]
    [ JsonArray <$> Gen.list (Range.linear 0 5) genJsonValue,
      JsonObject <$> Gen.list (Range.linear 0 5) ((,) <$> Gen.text (Range.linear 1 10) Gen.alpha <*> genJsonValue)
    ]

--------------------------------------------------------------------------------
-- Property Tests
--------------------------------------------------------------------------------

-- | Test that primitive types roundtrip correctly
prop_roundtrip_primitives :: Property
prop_roundtrip_primitives = property $ do
  value <- forAll genPrimitive
  case value of
    Left i -> testRoundTrip' i
    Right (Left d) -> testRoundTrip' d
    Right (Right (Left t)) -> testRoundTrip' t
    Right (Right (Right b)) -> testRoundTrip' b
  where
    testRoundTrip' :: (ToSZT a, FromSZT a, Eq a, Show a) => a -> PropertyT IO ()
    testRoundTrip' v = do
      let dynValue = toSzt v
      result <- evalEither (fromSzt dynValue)
      result === v

-- | Test that example types roundtrip correctly
prop_roundtrip_examples :: Property
prop_roundtrip_examples = property $ do
  exampleType <-
    forAll $
      Gen.choice
        [ Left <$> genPoint,
          Right . Left <$> genColor,
          Right . Right . Left <$> genOptional (Gen.int (Range.linear 0 100)),
          Right . Right . Right <$> genResult Gen.bool (Gen.int (Range.linear 0 100))
        ]

  case exampleType of
    Left point -> testRoundTrip' point
    Right (Left color) -> testRoundTrip' color
    Right (Right (Left opt)) -> testRoundTrip' opt
    Right (Right (Right res)) -> testRoundTrip' res
  where
    testRoundTrip' :: (ToSZT a, FromSZT a, Eq a, Show a) => a -> PropertyT IO ()
    testRoundTrip' v = do
      let dynValue = toSzt v
      result <- evalEither (fromSzt dynValue)
      result === v

-- | Test that deduplication preserves data correctness
prop_deduplication_preserves_data :: Property
prop_deduplication_preserves_data = property $ do
  -- Generate a structure with repetition to actually benefit from deduplication
  person <- forAll genPerson
  count <- forAll $ Gen.int (Range.linear 3 10)

  -- Create a list with repeated person data
  let repeatedData = replicate count person

  let originalDynValue = toSzt repeatedData
  let (dedupedValue, sharedTable) = deduplicateValue aggressiveDeduplicationStrategy originalDynValue

  -- Resolve references and deserialize
  resolvedValue <- evalEither (resolveReferences (Map.map Right sharedTable) dedupedValue)
  finalResult <- evalEither (fromSzt resolvedValue)

  -- Should be identical to original
  finalResult === repeatedData

  -- For now, just ensure correctness - size optimization can be investigated separately
  let originalSize = estimateSize originalDynValue
  let dedupedSize = estimateSize dedupedValue + sum (Map.map estimateSize sharedTable)
  let sharedCount = Map.size sharedTable

  -- Annotate the results for debugging
  annotate $
    "Original size: "
      ++ show originalSize
      ++ ", deduplicated: "
      ++ show dedupedSize
      ++ ", shared values: "
      ++ show sharedCount

  -- The main goal is correctness, size optimization is a bonus
  success

-- | Test file I/O roundtrips
prop_file_roundtrip :: Property
prop_file_roundtrip = property $ do
  person <- forAll genPerson

  test $ do
    result <- liftIO $ withSystemTempFile "test.szt" $ \tempFile h -> do
      hClose h -- Close handle before writing

      -- Save and load
      saveSzt tempFile person
      loadSzt tempFile

    -- Should roundtrip correctly
    case result of
      Left err -> do
        annotate $ "Load failed: " ++ show err
        failure
      Right loaded -> loaded === person

-- | Test hash consistency
prop_hash_consistency :: Property
prop_hash_consistency = property $ do
  point1 <- forAll genPoint
  point2 <- forAll genPoint

  let dynValue1 = toSzt point1
  let dynValue2 = toSzt point2
  let hash1 = computeContentHash dynValue1
  let hash2 = computeContentHash dynValue2

  -- Same values should have same hash
  when (point1 == point2) $ hash1 === hash2

  -- Different values should have different hashes (with very high probability)
  when (point1 /= point2) $ diff (hash1 /= hash2) (==) True

-- | Test fsck on valid files
prop_fsck_valid_files :: Property
prop_fsck_valid_files = property $ do
  jsonData <- forAll genJsonValue

  test $ do
    result <- liftIO $ withSystemTempFile "fsck_test.szt" $ \tempFile h -> do
      hClose h

      -- Save file and run fsck
      saveSztCompressed tempFile jsonData
      fsckSzt tempFile

    -- Should pass fsck
    diff (result ^. fsckPassed) (==) True
    diff (null $ result ^. fsckErrors) (==) True

-- | Test type info consistency
prop_type_info_consistency :: Property
prop_type_info_consistency = property $ do
  color <- forAll genColor

  let dynValue = toSzt color

  case _dvTypeInfo dynValue of
    Nothing -> do
      annotate "Expected type info but got Nothing"
      failure
    Just typeInfo -> do
      -- Should have type name
      case _tiTypeName typeInfo of
        Nothing -> do
          annotate "Expected type name but got Nothing"
          failure
        Just typeName -> do
          diff (Text.null typeName) (==) False

      -- Should have module name
      case _tiModule typeInfo of
        Nothing -> success -- Module can be optional
        Just moduleName -> do
          diff (Text.null moduleName) (==) False
