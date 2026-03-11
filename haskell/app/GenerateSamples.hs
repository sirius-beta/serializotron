{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (unless)
import Data.Aeson (ToJSON, Value, encode, object, toJSON, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Ratio (Ratio, denominator, numerator, (%))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Time
import Data.Vector qualified as Vector
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)
import Serializotron
import Serializotron.Examples
import Serializotron.Instances
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode (..))
import System.FilePath (replaceExtension, takeFileName, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)

saveSample :: (ToJSON a) => (FilePath -> a -> IO ()) -> FilePath -> a -> String -> IO ()
saveSample saver path value =
  saveSampleWithJson saver path value (toJSON value)

saveSampleWithJson :: (FilePath -> a -> IO ()) -> FilePath -> a -> Value -> String -> IO ()
saveSampleWithJson saver path value json description = do
  saver path value
  saveJson path json
  generatePrettyDump path
  putStrLn $ "  - " ++ takeFileName path ++ " - " ++ description

saveJson :: FilePath -> Value -> IO ()
saveJson sourcePath jsonValue = do
  let jsonPath = replaceExtension sourcePath "json"
  BL.writeFile jsonPath (encode jsonValue)

generatePrettyDump :: FilePath -> IO ()
generatePrettyDump filePath = do
  let txtPath = replaceExtension filePath "txt"
  (exitCode, stdoutText, stderrText) <- readProcessWithExitCode "./prettypb.sh" ["-m", "plain", filePath] ""
  case exitCode of
    ExitSuccess -> writeFile txtPath stdoutText
    ExitFailure code -> do
      hPutStrLn stderr $ "prettypb.sh failed for " ++ filePath ++ " with exit code " ++ show code
      unless (null stderrText) $ hPutStrLn stderr stderrText

main :: IO ()
main = do
  let samplesDir = "samples"
  createDirectoryIfMissing True samplesDir

  putStrLn $ "Generating comprehensive sample .szt files in " ++ samplesDir ++ "/"
  putStrLn ""

  putStrLn "=== Complex Structured Data ==="
  saveSample saveSzt (samplesDir </> "point.szt") examplePoint "Simple 2D point"

  saveSample saveSzt (samplesDir </> "color.szt") exampleColor "RGB color value"

  saveSample saveSzt (samplesDir </> "tree.szt") exampleTree "Binary tree of integers"

  saveSample saveSzt (samplesDir </> "person.szt") johnDoe "Person record"

  saveSample saveSzt (samplesDir </> "employee.szt") employee1 "Employee with nested records"

  saveSample saveSzt (samplesDir </> "expression.szt") exampleExpr "Mathematical expression AST"

  saveSample saveSzt (samplesDir </> "json.szt") exampleJson "JSON-like nested structure"

  saveSample saveSzt (samplesDir </> "document.szt") exampleDoc "Document with sections"

  saveSample saveSzt (samplesDir </> "graph.szt") exampleGraph "Graph with cycles"

  putStrLn ""
  putStrLn "=== Basic Primitive Types ==="
  saveSample saveSzt (samplesDir </> "int.szt") (42 :: Int) "Regular integer"

  saveSample saveSzt (samplesDir </> "int8.szt") (127 :: Int8) "8-bit signed integer"

  saveSample saveSzt (samplesDir </> "int16.szt") (32767 :: Int16) "16-bit signed integer"

  saveSample saveSzt (samplesDir </> "int32.szt") (2147483647 :: Int32) "32-bit signed integer"

  saveSample saveSzt (samplesDir </> "int64.szt") (9223372036854775807 :: Int64) "64-bit signed integer"

  saveSample saveSzt (samplesDir </> "word8.szt") (255 :: Word8) "8-bit unsigned integer"

  saveSample saveSzt (samplesDir </> "word16.szt") (65535 :: Word16) "16-bit unsigned integer"

  saveSample saveSzt (samplesDir </> "word32.szt") (4294967295 :: Word32) "32-bit unsigned integer"

  saveSample saveSzt (samplesDir </> "word64.szt") (18446744073709551615 :: Word64) "64-bit unsigned integer"

  saveSample saveSzt (samplesDir </> "integer.szt") (123456789012345678901234567890 :: Integer) "Arbitrary precision integer"

  saveSample saveSzt (samplesDir </> "natural.szt") (987654321098765432109876543210 :: Natural) "Natural number"

  saveSample saveSzt (samplesDir </> "double.szt") (3.14159265358979323846 :: Double) "Double precision float"

  saveSample saveSzt (samplesDir </> "float.szt") (2.71828 :: Float) "Single precision float"

  let rationalSample = 22 % 7 :: Rational
  let rationalJson =
        object
          [ "numerator" .= numerator rationalSample,
            "denominator" .= denominator rationalSample,
            "approximation" .= (fromRational rationalSample :: Double)
          ]
  saveSampleWithJson saveSzt (samplesDir </> "rational.szt") rationalSample rationalJson "Rational number (22/7)"

  saveSample saveSzt (samplesDir </> "text.szt") ("Hello, World!" :: Text.Text) "Text string"

  saveSample saveSzt (samplesDir </> "char.szt") ('Z' :: Char) "Character sample"

  saveSample saveSzt (samplesDir </> "bool_true.szt") True "Boolean true"

  saveSample saveSzt (samplesDir </> "bool_false.szt") False "Boolean false"

  putStrLn ""
  putStrLn "=== Time and Date Types ==="
  let testDay = fromGregorian 2024 12 25
  let testTimeOfDay = TimeOfDay 14 30 45.123
  let testLocalTime = LocalTime testDay testTimeOfDay
  let testUtcTime = UTCTime testDay (timeOfDayToTime testTimeOfDay)
  let testDiffTime = secondsToDiffTime 3661 -- 1 hour, 1 minute, 1 second
  let testNominalDiffTime = fromIntegral (86400 :: Int) :: NominalDiffTime -- 1 day in seconds
  saveSample saveSzt (samplesDir </> "utctime.szt") testUtcTime "Representative UTC time"

  saveSample saveSzt (samplesDir </> "day.szt") testDay "Date (Christmas 2024)"

  saveSample saveSzt (samplesDir </> "timeofday.szt") testTimeOfDay "Time of day with subseconds"

  saveSample saveSzt (samplesDir </> "localtime.szt") testLocalTime "Local date and time"

  saveSample saveSzt (samplesDir </> "difftime.szt") testDiffTime "Time difference (1:01:01)"

  saveSample saveSzt (samplesDir </> "nominaldifftime.szt") testNominalDiffTime "Nominal time difference (1 day)"

  putStrLn ""
  putStrLn "=== Collection Types ==="
  saveSample saveSzt (samplesDir </> "list_empty.szt") ([] :: [Int]) "Empty list"

  saveSample saveSzt (samplesDir </> "list_int.szt") ([1, 2, 3, 4, 5] :: [Int]) "List of integers"

  saveSample saveSzt (samplesDir </> "list_text.szt") (["apple", "banana", "cherry"] :: [Text.Text]) "List of text strings"

  saveSample saveSzt (samplesDir </> "nonempty.szt") (1 :| [2, 3, 4] :: NonEmpty Int) "Non-empty list"

  saveSample saveSzt (samplesDir </> "map.szt") (Map.fromList [("one", 1), ("two", 2), ("three", 3)] :: Map.Map Text.Text Int) "Map from text to integers"

  saveSample saveSzt (samplesDir </> "hashmap.szt") (HashMap.fromList [("red", 0xFF0000), ("green", 0x00FF00), ("blue", 0x0000FF)] :: HashMap.HashMap Text.Text Int) "HashMap with color values"

  saveSample saveSzt (samplesDir </> "set.szt") (Set.fromList [1, 2, 3, 5, 8, 13, 21] :: Set.Set Int) "Set of Fibonacci numbers"

  saveSample saveSzt (samplesDir </> "hashset.szt") (HashSet.fromList ["alpha", "beta", "gamma", "delta"] :: HashSet.HashSet Text.Text) "HashSet of Greek letters"

  saveSample saveSzt (samplesDir </> "vector.szt") (Vector.fromList [0.1, 0.2, 0.3, 0.4, 0.5] :: Vector.Vector Double) "Vector of doubles"

  let largeListInts = [1 .. 1_000_000] :: [Int]

  saveSample saveSzt (samplesDir </> "large_list_int.szt") largeListInts "Large list of integers"

  saveSample
    saveSztCompressedAggressive
    (samplesDir </> "large_list_int_aggressive_dedup.szt")
    largeListInts
    "Large list of integers with aggressive dedup"

  putStrLn ""
  putStrLn "=== Optional and Choice Types ==="
  saveSample saveSzt (samplesDir </> "maybe_nothing.szt") (Nothing :: Maybe Int) "Maybe Nothing"

  saveSample saveSzt (samplesDir </> "maybe_just.szt") (Just 42 :: Maybe Int) "Maybe with value"

  saveSample saveSzt (samplesDir </> "either_left.szt") (Left "Error message" :: Either Text.Text Int) "Either Left value"

  saveSample saveSzt (samplesDir </> "either_right.szt") (Right 123 :: Either Text.Text Int) "Either Right value"

  putStrLn ""
  putStrLn "=== Tuple Types ==="
  saveSample saveSzt (samplesDir </> "unit.szt") () "Unit value"

  saveSample saveSzt (samplesDir </> "pair.szt") (("hello", 42) :: (Text.Text, Int)) "Two-element tuple"

  saveSample saveSzt (samplesDir </> "triple.szt") (("x", 1.0, True) :: (Text.Text, Double, Bool)) "Three-element tuple"

  putStrLn ""
  putStrLn "=== Nested and Complex Structures ==="
  saveSample saveSzt (samplesDir </> "nested_lists.szt") ([[1, 2], [3, 4, 5], [6]] :: [[Int]]) "List of lists"

  saveSample saveSzt (samplesDir </> "maybe_list.szt") (Just ["a", "b", "c"] :: Maybe [Text.Text]) "Maybe containing a list"

  saveSample saveSzt (samplesDir </> "list_maybe.szt") ([Nothing, Just 1, Nothing, Just 2] :: [Maybe Int]) "List of Maybe values"
  saveSample
    saveSztCompressedAggressive
    (samplesDir </> "list_maybe_aggressive_dedup.szt")
    ([Nothing, Just 1, Nothing, Just 2] :: [Maybe Int])
    "List with aggressive dedup"

  saveSample saveSzt (samplesDir </> "map_of_lists.szt") (Map.fromList [("evens", [2, 4, 6]), ("odds", [1, 3, 5])] :: Map.Map Text.Text [Int]) "Map containing lists"

  putStrLn ""
  putStrLn "=== Deduplication Examples ==="
  saveSample saveSztCompressed (samplesDir </> "employee_list_compressed.szt") [employee1, employee2] "List with balanced deduplication"

  saveSample
    saveSzt
    (samplesDir </> "large_json_normal.szt")
    (JsonArray (replicate 20 exampleJson))
    "Large JSON array without dedup"

  saveSample
    saveSztCompressedAggressive
    (samplesDir </> "large_json_aggressive.szt")
    (JsonArray (replicate 20 exampleJson))
    "Large JSON array with aggressive dedup"

  let repeatedData = replicate 100 ("repeated" :: Text.Text, 42 :: Int, True)
  saveSample saveSztCompressedAggressive (samplesDir </> "highly_redundant.szt") repeatedData "100 identical tuples (high deduplication)"

  -- Create a structure with shared sub-components
  let sharedPerson = ("Alice" :: Text.Text, 30 :: Int)
  let complexStructure = [(sharedPerson, ["project1" :: Text.Text, "project2"]), (sharedPerson, ["project3"])]
  saveSample saveSztCompressedAggressive (samplesDir </> "shared_references.szt") complexStructure "Structure with shared person references"

  putStrLn ""
  putStrLn "=== Real-world Scenario ==="
  saveSample saveSztCompressed (samplesDir </> "enterprise_snapshot.szt") enterpriseSnapshot "Large enterprise snapshot"

  putStrLn ""
  putStrLn $ "Generated 50 comprehensive sample files in " ++ samplesDir ++ "/"
  putStrLn ""
  putStrLn "Sample categories:"
  putStrLn "  - Complex structured data (9 files)"
  putStrLn "  - Basic primitive types (14 files)"
  putStrLn "  - Time and date types (6 files)"
  putStrLn "  - Collection types (8 files)"
  putStrLn "  - Optional and choice types (4 files)"
  putStrLn "  - Tuple types (3 files)"
  putStrLn "  - Nested structures (4 files)"
  putStrLn "  - Deduplication examples (5 files)"
  putStrLn "  - Real-world scenario (1 file)"
