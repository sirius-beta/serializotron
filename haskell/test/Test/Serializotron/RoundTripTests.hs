{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Comprehensive round-trip property tests for Serializotron
--
-- This module is heavily inspired by and adapted from the aeson library's
-- round-trip property tests, particularly:
--   - aeson/tests/PropertyRoundTrip.hs
--   - aeson/tests/PropUtils.hs
--
-- The test structure, type coverage, and property-based testing approach
-- closely follows aeson's methodology to ensure comprehensive coverage
-- of serialization round-trips for standard Haskell types.
--
-- Original aeson code: https://github.com/haskell/aeson
-- License: BSD-3-Clause
module Test.Serializotron.RoundTripTests where

import Control.Monad.IO.Class (liftIO)
-- Standard library imports

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Ratio ((%))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Lazy qualified as LText
import Data.Time
import Data.Vector qualified as Vector
import Data.Word (Word16, Word32, Word64, Word8)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric.Natural (Natural)
import Serializotron
import Serializotron.Instances -- For all the type class instances

--------------------------------------------------------------------------------
-- Generators for Basic Types
--------------------------------------------------------------------------------

-- | Generate bounded integers
genInt :: Gen Int
genInt = Gen.int (Range.linear (-1000000) 1000000)

genInt8 :: Gen Int8
genInt8 = Gen.int8 Range.constantBounded

genInt16 :: Gen Int16
genInt16 = Gen.int16 Range.constantBounded

genInt32 :: Gen Int32
genInt32 = Gen.int32 Range.constantBounded

genInt64 :: Gen Int64
genInt64 = Gen.int64 Range.constantBounded

-- | Generate unsigned integers
genWord8 :: Gen Word8
genWord8 = Gen.word8 Range.constantBounded

genWord16 :: Gen Word16
genWord16 = Gen.word16 Range.constantBounded

genWord32 :: Gen Word32
genWord32 = Gen.word32 Range.constantBounded

genWord64 :: Gen Word64
genWord64 = Gen.word64 Range.constantBounded

-- | Generate arbitrary precision integers
genInteger :: Gen Integer
genInteger = Gen.integral $ Range.linear (-bigInteger) bigInteger

bigInteger :: Integer
bigInteger = (10 :: Integer) ^ (100 :: Integer)

-- | Generate natural numbers
genNatural :: Gen Natural
genNatural = fromIntegral <$> Gen.integral (Range.linear 0 bigInteger)

-- | Generate doubles (with special values)
genDouble :: Gen Double
genDouble =
  Gen.choice
    [ Gen.double (Range.exponentialFloat (-1e308) 1e308),
      pure (0 / 0), -- NaN
      pure (1 / 0), -- Infinity
      pure ((-1) / 0), -- -Infinity
      pure 0,
      pure (-0)
    ]

-- | Generate finite doubles (excluding NaN and Infinity for use in compound types)
genFiniteDouble :: Gen Double
genFiniteDouble =
  Gen.choice
    [ Gen.double (Range.exponentialFloat (-1e308) 1e308),
      pure 0,
      pure (-0)
    ]

-- | Generate floats
genFloat :: Gen Float
genFloat = Gen.float (Range.exponentialFloat (-1e38) 1e38)

-- | Generate rationals
genRational :: Gen Rational
genRational = do
  num <- Gen.integral (Range.linear (-1000) 1000)
  den <- Gen.integral (Range.linear 1 1000)
  pure (num % den)

-- | Generate Text
genText :: Gen Text
genText = Gen.text (Range.linear 0 100) Gen.unicode

-- | Generate Lazy Text
genLazyText :: Gen LText.Text
genLazyText = LText.fromStrict <$> genText

-- | Generate Strings
genString :: Gen String
genString = Gen.string (Range.linear 0 100) Gen.unicode

-- | Generate ByteStrings
genByteString :: Gen ByteString
genByteString = BS.pack <$> Gen.list (Range.linear 0 100) (Gen.word8 Range.constantBounded)

-- | Generate Booleans
genBool :: Gen Bool
genBool = Gen.bool

-- | Generate Characters
genChar :: Gen Char
genChar = Gen.unicode

--------------------------------------------------------------------------------
-- Generators for Time Types
--------------------------------------------------------------------------------

-- | Generate Day (dates)
genDay :: Gen Day
genDay = do
  year <- Gen.integral (Range.linear 1600 2400)
  month <- Gen.int (Range.linear 1 12)
  day <- Gen.int (Range.linear 1 28) -- Safe for all months
  pure $ fromGregorian year month day

-- | Generate TimeOfDay
genTimeOfDay :: Gen TimeOfDay
genTimeOfDay = do
  hour <- Gen.int (Range.linear 0 23)
  minute <- Gen.int (Range.linear 0 59)
  sec <- fromIntegral <$> Gen.int (Range.linear 0 59)
  pure $ TimeOfDay hour minute sec

-- | Generate LocalTime
genLocalTime :: Gen LocalTime
genLocalTime = LocalTime <$> genDay <*> genTimeOfDay

-- | Generate UTCTime
genUTCTime :: Gen UTCTime
genUTCTime = do
  day <- genDay
  secs <- fromIntegral <$> Gen.int (Range.linear 0 86399)
  pure $ UTCTime day (secondsToDiffTime secs)

-- | Generate ZonedTime
genZonedTime :: Gen ZonedTime
genZonedTime = do
  localTime <- genLocalTime
  -- Simple timezone generation (just UTC for now)
  pure $ ZonedTime localTime utc

-- | Generate DiffTime
genDiffTime :: Gen DiffTime
genDiffTime = secondsToDiffTime <$> Gen.integral (Range.linear 0 86400)

-- | Generate NominalDiffTime
genNominalDiffTime :: Gen NominalDiffTime
genNominalDiffTime = fromIntegral <$> Gen.int (Range.linear 0 86400)

--------------------------------------------------------------------------------
-- Generators for Collection Types
--------------------------------------------------------------------------------

-- | Generate lists
genList :: Gen a -> Gen [a]
genList = Gen.list (Range.linear 0 20)

-- | Generate non-empty lists
genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty gen = (:|) <$> gen <*> Gen.list (Range.linear 0 10) gen

-- | Generate Maps
genMap :: (Ord k) => Gen k -> Gen v -> Gen (Map.Map k v)
genMap genK genV = Map.fromList <$> Gen.list (Range.linear 0 10) ((,) <$> genK <*> genV)

-- | Generate Sets
genSet :: (Ord a) => Gen a -> Gen (Set.Set a)
genSet gen = Set.fromList <$> Gen.list (Range.linear 0 10) gen

-- | Generate HashMaps
genHashMap :: (Hashable k) => Gen k -> Gen v -> Gen (HashMap.HashMap k v)
genHashMap genK genV = HashMap.fromList <$> Gen.list (Range.linear 0 10) ((,) <$> genK <*> genV)

-- | Generate HashSets
genHashSet :: (Hashable a) => Gen a -> Gen (HashSet.HashSet a)
genHashSet gen = HashSet.fromList <$> Gen.list (Range.linear 0 10) gen

-- | Generate Vectors
genVector :: Gen a -> Gen (Vector.Vector a)
genVector gen = Vector.fromList <$> Gen.list (Range.linear 0 20) gen

--------------------------------------------------------------------------------
-- Generators for Optional Types
--------------------------------------------------------------------------------

-- | Generate Maybe values
genMaybe :: Gen a -> Gen (Maybe a)
genMaybe gen = Gen.choice [pure Nothing, Just <$> gen]

-- | Generate Either values
genEither :: Gen a -> Gen b -> Gen (Either a b)
genEither genA genB = Gen.choice [Left <$> genA, Right <$> genB]

--------------------------------------------------------------------------------
-- Generators for Tuples
--------------------------------------------------------------------------------

genPair :: Gen a -> Gen b -> Gen (a, b)
genPair genA genB = (,) <$> genA <*> genB

genTriple :: Gen a -> Gen b -> Gen c -> Gen (a, b, c)
genTriple genA genB genC = (,,) <$> genA <*> genB <*> genC

genQuadruple :: Gen a -> Gen b -> Gen c -> Gen d -> Gen (a, b, c, d)
genQuadruple genA genB genC genD = (,,,) <$> genA <*> genB <*> genC <*> genD

--------------------------------------------------------------------------------
-- Property Test Helpers
--------------------------------------------------------------------------------

-- | Generic round-trip test helper
testRoundTrip :: (ToSZT a, FromSZT a, Eq a, Show a) => Gen a -> Property
testRoundTrip gen = property $ do
  value <- forAll gen
  let encoded = toSzt value
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> value === decoded

-- | Generic round-trip test helper for SHALLOW serialization
-- Uses temporary files to test full shallow serialization path
testRoundTripShallow :: (ToSZT a, FromSZT a, Eq a, Show a) => String -> Gen a -> Property
testRoundTripShallow testName gen = property $ do
  value <- forAll gen

  -- Use IO to save and load with shallow serialization
  -- Use unique temp file per test to avoid file locking issues
  result <- liftIO $ do
    let tmpPath = "/tmp/serializotron-test-shallow-" ++ testName ++ ".szt"
    saveSztShallow tmpPath value
    loadSzt tmpPath

  case result of
    Left err -> do
      annotate $ "Failed to decode (shallow): " ++ show err
      failure
    Right decoded -> value === decoded

-- | Round-trip test with custom equality (for floating point)
testRoundTripApprox :: (ToSZT a, FromSZT a, Show a) => Gen a -> (a -> a -> Bool) -> Property
testRoundTripApprox gen approxEq = property $ do
  value <- forAll gen
  let encoded = toSzt value
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> assert (approxEq value decoded)

-- | Approximate equality for doubles (handling NaN and Infinity)
approxDouble :: Double -> Double -> Bool
approxDouble x y
  | isNaN x && isNaN y = True
  | isInfinite x && isInfinite y = signum x == signum y
  | otherwise = abs (x - y) < 1e-10

-- | Approximate equality for Vector Double
approxVectorDouble :: Vector.Vector Double -> Vector.Vector Double -> Bool
approxVectorDouble v1 v2
  | Vector.length v1 /= Vector.length v2 = False
  | otherwise = Vector.and $ Vector.zipWith approxDouble v1 v2

-- | Approximate equality for tuples containing doubles
approxQuadruple :: (Eq a, Eq b, Eq c) => (a, b, c, Double) -> (a, b, c, Double) -> Bool
approxQuadruple (a1, b1, c1, d1) (a2, b2, c2, d2) =
  a1 == a2 && b1 == b2 && c1 == c2 && approxDouble d1 d2

--------------------------------------------------------------------------------
-- All Round-Trip Property Tests
--------------------------------------------------------------------------------

-- Basic types
prop_roundtrip_unit :: Property
prop_roundtrip_unit = testRoundTrip (pure ())

prop_roundtrip_bool :: Property
prop_roundtrip_bool = testRoundTrip genBool

prop_roundtrip_int :: Property
prop_roundtrip_int = testRoundTrip genInt

prop_roundtrip_int8 :: Property
prop_roundtrip_int8 = testRoundTrip genInt8

prop_roundtrip_int16 :: Property
prop_roundtrip_int16 = testRoundTrip genInt16

prop_roundtrip_int32 :: Property
prop_roundtrip_int32 = testRoundTrip genInt32

prop_roundtrip_int64 :: Property
prop_roundtrip_int64 = testRoundTrip genInt64

prop_roundtrip_word8 :: Property
prop_roundtrip_word8 = testRoundTrip genWord8

prop_roundtrip_word16 :: Property
prop_roundtrip_word16 = testRoundTrip genWord16

prop_roundtrip_word32 :: Property
prop_roundtrip_word32 = testRoundTrip genWord32

prop_roundtrip_word64 :: Property
prop_roundtrip_word64 = testRoundTrip genWord64

prop_roundtrip_integer :: Property
prop_roundtrip_integer = testRoundTrip genInteger

prop_roundtrip_natural :: Property
prop_roundtrip_natural = testRoundTrip genNatural

prop_roundtrip_double :: Property
prop_roundtrip_double = testRoundTripApprox genDouble approxDouble

prop_roundtrip_float :: Property
prop_roundtrip_float = testRoundTrip genFloat

prop_roundtrip_rational :: Property
prop_roundtrip_rational = testRoundTrip genRational

prop_roundtrip_char :: Property
prop_roundtrip_char = testRoundTrip genChar

prop_roundtrip_string :: Property
prop_roundtrip_string = testRoundTrip genString

prop_roundtrip_text :: Property
prop_roundtrip_text = testRoundTrip genText

prop_roundtrip_lazy_text :: Property
prop_roundtrip_lazy_text = testRoundTrip genLazyText

prop_roundtrip_bytestring :: Property
prop_roundtrip_bytestring = testRoundTrip genByteString

-- Time types
prop_roundtrip_day :: Property
prop_roundtrip_day = testRoundTrip genDay

prop_roundtrip_timeofday :: Property
prop_roundtrip_timeofday = testRoundTrip genTimeOfDay

prop_roundtrip_localtime :: Property
prop_roundtrip_localtime = testRoundTrip genLocalTime

prop_roundtrip_utctime :: Property
prop_roundtrip_utctime = testRoundTrip genUTCTime

-- Note: ZonedTime doesn't have an Eq instance by default
-- prop_roundtrip_zonedtime :: Property
-- prop_roundtrip_zonedtime = testRoundTrip genZonedTime

prop_roundtrip_difftime :: Property
prop_roundtrip_difftime = testRoundTrip genDiffTime

prop_roundtrip_nominaldifftime :: Property
prop_roundtrip_nominaldifftime = testRoundTrip genNominalDiffTime

-- Collection types
prop_roundtrip_list_int :: Property
prop_roundtrip_list_int = testRoundTrip (genList genInt)

prop_roundtrip_list_text :: Property
prop_roundtrip_list_text = testRoundTrip (genList genText)

prop_roundtrip_nonempty_int :: Property
prop_roundtrip_nonempty_int = testRoundTrip (genNonEmpty genInt)

prop_roundtrip_map_text_int :: Property
prop_roundtrip_map_text_int = testRoundTrip (genMap genText genInt)

prop_roundtrip_set_int :: Property
prop_roundtrip_set_int = testRoundTrip (genSet genInt)

prop_roundtrip_hashmap_text_int :: Property
prop_roundtrip_hashmap_text_int = testRoundTrip (genHashMap genText genInt)

prop_roundtrip_hashset_int :: Property
prop_roundtrip_hashset_int = testRoundTrip (genHashSet genInt)

prop_roundtrip_vector_double :: Property
prop_roundtrip_vector_double = testRoundTrip (genVector genFiniteDouble)

-- Optional types
prop_roundtrip_maybe_int :: Property
prop_roundtrip_maybe_int = testRoundTrip (genMaybe genInt)

prop_roundtrip_either_text_int :: Property
prop_roundtrip_either_text_int = testRoundTrip (genEither genText genInt)

-- Tuples
prop_roundtrip_pair :: Property
prop_roundtrip_pair = testRoundTrip (genPair genInt genText)

prop_roundtrip_triple :: Property
prop_roundtrip_triple = testRoundTrip (genTriple genInt genText genBool)

prop_roundtrip_quadruple :: Property
prop_roundtrip_quadruple = testRoundTrip (genQuadruple genInt genText genBool genFiniteDouble)

-- Nested structures
prop_roundtrip_nested_lists :: Property
prop_roundtrip_nested_lists = testRoundTrip (genList (genList genInt))

prop_roundtrip_maybe_list :: Property
prop_roundtrip_maybe_list = testRoundTrip (genMaybe (genList genText))

prop_roundtrip_list_maybe :: Property
prop_roundtrip_list_maybe = testRoundTrip (genList (genMaybe genInt))

prop_roundtrip_map_of_lists :: Property
prop_roundtrip_map_of_lists = testRoundTrip (genMap genText (genList genInt))

--------------------------------------------------------------------------------
-- Shallow Serialization Tests
--------------------------------------------------------------------------------

-- | Test shallow serialization with a specific list of Text tuples
-- This test reproduces a bug where items at positions 2+ are incorrectly
-- deserialized as copies of earlier items
prop_roundtrip_shallow_text_tuple_list :: Property
prop_roundtrip_shallow_text_tuple_list = testRoundTripShallow "text_tuple_list" genTextTupleList4
  where
    genTextTupleList4 :: Gen [(Text, Text)]
    genTextTupleList4 =
      pure
        [ ("a", "b"),
          ("c", "c"),
          ("c", "a"),
          ("a", "a")
        ]

-- | Property test for random lists of Text tuples with shallow serialization
prop_roundtrip_shallow_list_text_tuples :: Property
prop_roundtrip_shallow_list_text_tuples = testRoundTripShallow "list_text_tuples" (genList (genPair genText genText))
