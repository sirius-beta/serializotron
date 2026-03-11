{-# LANGUAGE OverloadedStrings #-}

-- | Main test runner for Serializotron
--
-- Test organization structure inspired by aeson's test suite organization
-- (aeson/tests/Tests.hs), using Tasty as the unified test framework
-- to run property tests, unit tests, and integration tests together.
module Main where

import Test.Serializotron.InspiredExamples
import Test.Serializotron.MemoTests
import Test.Serializotron.Properties
import Test.Serializotron.RoundTripTests
import Test.Serializotron.ShallowExampleTests
import Test.Serializotron.TypeInfoTests
import Test.Serializotron.Units
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Serializotron Tests"
    [ unitTests,
      propertyTests,
      roundTripTests,
      inspiredExamplesTests,
      typeInfoTests,
      shallowExampleTests,
      memoTests
    ]

-- Convert unit tests to Tasty format
unitTests :: TestTree
unitTests =
  testGroup
    "Unit Tests"
    [ testCase "Primitive roundtrips" $ do
        result <- test_primitive_roundtrips
        assertBool "Primitive roundtrips should pass" result,
      testCase "Example roundtrips" $ do
        result <- test_example_roundtrips
        assertBool "Example roundtrips should pass" result,
      testCase "Deduplication stats" $ do
        result <- test_deduplication_stats
        assertBool "Deduplication stats should pass" result,
      testCase "Deduplication effectiveness" $ do
        result <- test_deduplication_effectiveness
        assertBool "Deduplication effectiveness should pass" result,
      testCase "File operations" $ do
        result <- test_file_operations
        assertBool "File operations should pass" result,
      testCase "Hash collision detection" $ do
        result <- test_hash_collision_detection
        assertBool "Hash collision detection should pass" result,
      testCase "FSCK clean file" $ do
        result <- test_fsck_clean_file
        assertBool "FSCK clean file should pass" result,
      testCase "Type info extraction" $ do
        result <- test_type_info_extraction
        assertBool "Type info extraction should pass" result,
      testCase "Cycle detection" $ do
        result <- test_cycle_detection
        assertBool "Cycle detection should pass" result
    ]

-- Convert Hedgehog property tests to Tasty format
propertyTests :: TestTree
propertyTests =
  testGroup
    "Property Tests"
    [ testProperty "Roundtrip primitives" prop_roundtrip_primitives,
      testProperty "Roundtrip examples" prop_roundtrip_examples,
      testProperty "Deduplication preserves data" prop_deduplication_preserves_data,
      testProperty "File roundtrip" prop_file_roundtrip,
      testProperty "Hash consistency" prop_hash_consistency,
      testProperty "FSCK valid files" prop_fsck_valid_files,
      testProperty "Type info consistency" prop_type_info_consistency
    ]

-- Convert round-trip tests to Tasty format
roundTripTests :: TestTree
roundTripTests =
  testGroup
    "Comprehensive Round-Trip Tests"
    [ testProperty "unit" prop_roundtrip_unit,
      testProperty "bool" prop_roundtrip_bool,
      testProperty "int" prop_roundtrip_int,
      testProperty "int8" prop_roundtrip_int8,
      testProperty "int16" prop_roundtrip_int16,
      testProperty "int32" prop_roundtrip_int32,
      testProperty "int64" prop_roundtrip_int64,
      testProperty "word8" prop_roundtrip_word8,
      testProperty "word16" prop_roundtrip_word16,
      testProperty "word32" prop_roundtrip_word32,
      testProperty "word64" prop_roundtrip_word64,
      testProperty "integer" prop_roundtrip_integer,
      testProperty "natural" prop_roundtrip_natural,
      testProperty "double" prop_roundtrip_double,
      testProperty "float" prop_roundtrip_float,
      testProperty "rational" prop_roundtrip_rational,
      testProperty "char" prop_roundtrip_char,
      testProperty "text" prop_roundtrip_text,
      testProperty "lazy_text" prop_roundtrip_lazy_text,
      testProperty "bytestring" prop_roundtrip_bytestring,
      testProperty "day" prop_roundtrip_day,
      testProperty "timeofday" prop_roundtrip_timeofday,
      testProperty "localtime" prop_roundtrip_localtime,
      testProperty "utctime" prop_roundtrip_utctime,
      testProperty "difftime" prop_roundtrip_difftime,
      testProperty "nominaldifftime" prop_roundtrip_nominaldifftime,
      testProperty "list_int" prop_roundtrip_list_int,
      testProperty "list_text" prop_roundtrip_list_text,
      testProperty "nonempty_int" prop_roundtrip_nonempty_int,
      testProperty "map_text_int" prop_roundtrip_map_text_int,
      testProperty "set_int" prop_roundtrip_set_int,
      testProperty "hashmap_text_int" prop_roundtrip_hashmap_text_int,
      testProperty "hashset_int" prop_roundtrip_hashset_int,
      testProperty "vector_double" prop_roundtrip_vector_double,
      testProperty "maybe_int" prop_roundtrip_maybe_int,
      testProperty "either_text_int" prop_roundtrip_either_text_int,
      testProperty "pair" prop_roundtrip_pair,
      testProperty "triple" prop_roundtrip_triple,
      testProperty "quadruple" prop_roundtrip_quadruple,
      testProperty "nested_lists" prop_roundtrip_nested_lists,
      testProperty "maybe_list" prop_roundtrip_maybe_list,
      testProperty "list_maybe" prop_roundtrip_list_maybe,
      testProperty "map_of_lists" prop_roundtrip_map_of_lists,
      -- Shallow serialization tests
      testProperty "shallow_text_tuple_list" prop_roundtrip_shallow_text_tuple_list,
      testProperty "shallow_list_text_tuples" prop_roundtrip_shallow_list_text_tuples
    ]

-- TypeInfo extraction tests
typeInfoTests :: TestTree
typeInfoTests =
  testGroup
    "TypeInfo Extraction Tests"
    [ testProperty "person_typeinfo" prop_person_typeinfo,
      testProperty "employee_typeinfo" prop_employee_typeinfo,
      testProperty "solo_typeinfo" prop_solo_typeinfo,
      testProperty "unit_typeinfo" prop_unit_typeinfo,
      testProperty "shape_typeinfo" prop_shape_typeinfo,
      testProperty "color_complete_constructors" prop_color_complete_constructors,
      testProperty "color_typeinfo_consistency" prop_color_typeinfo_consistency,
      testProperty "person_roundtrip" prop_person_roundtrip,
      testProperty "employee_roundtrip" prop_employee_roundtrip,
      testProperty "shape_roundtrip" prop_shape_roundtrip,
      testProperty "color_roundtrip" prop_color_roundtrip,
      -- Newtype wrapper tests
      testProperty "userid_newtype_wrapper" prop_userid_newtype_wrapper,
      testProperty "username_newtype_wrapper" prop_username_newtype_wrapper,
      testProperty "chained_newtype_wrapper" prop_chained_newtype_wrapper,
      testProperty "user_product_with_newtypes" prop_user_product_with_newtypes,
      testProperty "newtype_roundtrip_userid" prop_newtype_roundtrip_userid,
      testProperty "newtype_roundtrip_username" prop_newtype_roundtrip_username,
      testProperty "newtype_roundtrip_chained" prop_newtype_roundtrip_chained,
      testProperty "newtype_roundtrip_user" prop_newtype_roundtrip_user
    ]
