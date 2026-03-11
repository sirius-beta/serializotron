{-# LANGUAGE OverloadedStrings #-}

module Serializotron.TestTypeParams where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Serializotron

testTypeParams :: IO ()
testTypeParams = do
  putStrLn "=== Testing Type Parameter Extraction ==="
  putStrLn ""

  -- Test 1: Maybe Int
  putStrLn "Test 1: Maybe Int"
  let maybeInt = Just (42 :: Int)
  let dynMaybeInt = toSzt maybeInt
  case _dvTypeInfo dynMaybeInt of
    Nothing -> putStrLn "ERROR: No type info!"
    Just ti -> do
      putStrLn $ "  Type: " <> Text.unpack (fromMaybe "" (_tiTypeName ti))
      putStrLn $ "  Module: " <> Text.unpack (fromMaybe "" (_tiModule ti))
      putStrLn $ "  Type Parameters: " <> show (length (_tiTypeParameters ti))
      case _tiTypeParameters ti of
        [] -> putStrLn "    ERROR: No type parameters found!"
        [param] -> do
          putStrLn "    ✓ Found 1 type parameter:"
          putStrLn $ "      Type: " <> Text.unpack (fromMaybe "" (_tiTypeName param))
          putStrLn $ "      Module: " <> Text.unpack (fromMaybe "" (_tiModule param))
        _ -> putStrLn "    ERROR: Unexpected number of type parameters"
  putStrLn ""

  -- Test 2: Either String Int
  putStrLn "Test 2: Either String Int"
  let eitherVal = Right (123 :: Int) :: Either String Int
  let dynEither = toSzt eitherVal
  case _dvTypeInfo dynEither of
    Nothing -> putStrLn "ERROR: No type info!"
    Just ti -> do
      putStrLn $ "  Type: " <> Text.unpack (fromMaybe "" (_tiTypeName ti))
      putStrLn $ "  Module: " <> Text.unpack (fromMaybe "" (_tiModule ti))
      putStrLn $ "  Type Parameters: " <> show (length (_tiTypeParameters ti))
      case _tiTypeParameters ti of
        [] -> putStrLn "    ERROR: No type parameters found!"
        [param1, param2] -> do
          putStrLn "    ✓ Found 2 type parameters:"
          putStrLn $ "      1. Type: " <> Text.unpack (fromMaybe "" (_tiTypeName param1))
          putStrLn $ "         Module: " <> Text.unpack (fromMaybe "" (_tiModule param1))
          putStrLn $ "      2. Type: " <> Text.unpack (fromMaybe "" (_tiTypeName param2))
          putStrLn $ "         Module: " <> Text.unpack (fromMaybe "" (_tiModule param2))
        _ -> putStrLn "    ERROR: Unexpected number of type parameters"
  putStrLn ""

  -- Test 3: Save and load
  putStrLn "Test 3: Round-trip test"
  saveSzt "test-maybe.szt" maybeInt
  saveSzt "test-either.szt" eitherVal

  result1 <- loadSzt "test-maybe.szt" :: IO (Either SerializotronError (Maybe Int))
  result2 <- loadSzt "test-either.szt" :: IO (Either SerializotronError (Either String Int))

  case result1 of
    Left err -> putStrLn $ "  ERROR loading Maybe: " <> Text.unpack (formatError err)
    Right val -> putStrLn $ "  ✓ Successfully loaded Maybe: " <> show val

  case result2 of
    Left err -> putStrLn $ "  ERROR loading Either: " <> Text.unpack (formatError err)
    Right val -> putStrLn $ "  ✓ Successfully loaded Either: " <> show val

  putStrLn ""
  putStrLn "=== All tests complete ==="
