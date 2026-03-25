{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Serializotron.MemoTests where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Hashable (Hashable (..))
import Data.IORef (readIORef)
import Data.Maybe (isJust)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Proxy (..), Typeable, typeRep)
import GHC.Generics (Generic)
import GHC.Generics qualified
import Serializotron
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

-- A simple type with ShallowIdentifiable for testing.
data Widget = Widget
  { widgetName :: Text,
    widgetSize :: Int
  }
  deriving stock (Eq, Show, Generic)

instance Hashable Widget where
  hashWithSalt s (Widget n sz) = hashWithSalt s (n, sz)

instance ShallowIdentifiable Widget where
  shallowIdentifier (Widget n _) = encodeUtf8 n

instance ToSZT Widget where
  toSzt = memoizedToSzt $ \w ->
    DynamicValue
      { _dvCore = gToSZT (GHC.Generics.from w),
        _dvTypeInfo =
          Just $
            gGetTypeInfo
              (GHC.Generics.from w)
              (typeRep (Proxy @Widget)),
        _dvSchemaVersion = currentSchemaVersion,
        _dvShallowId = Just (shallowIdentifier w)
      }

instance FromSZT Widget where
  fromSzt = memoizedFromSzt defaultFromSzt

memoTests :: TestTree
memoTests =
  testGroup
    "Memoization"
    [ testCase "memoizedToSzt caches by shallowId" $ do
        resetToSztMemoCache
        let w1 = Widget "alpha" 10
            w2 = Widget "alpha" 10
            w3 = Widget "beta" 20
            dv1 = toSzt w1
            dv2 = toSzt w2
            dv3 = toSzt w3
        dv1 @?= dv2
        assertBool "Different shallowId should produce different values" (dv1 /= dv3)
        cache <- readIORef toSztMemoCache
        Map.size cache @?= 2
        resetToSztMemoCache,
      testCase "resetToSztMemoCache clears the cache" $ do
        resetToSztMemoCache
        let _dv = toSzt (Widget "test" 1)
        resetToSztMemoCache
        cache <- readIORef toSztMemoCache
        Map.size cache @?= 0,
      testCase "memoizedFromSzt round-trips correctly" $ do
        resetToSztMemoCache
        resetFromSztMemoCache
        let w = Widget "gamma" 30
            dv = toSzt w
        case fromSzt @Widget dv of
          Left err -> assertBool ("fromSzt failed: " <> show err) False
          Right r -> r @?= w
        resetToSztMemoCache
        resetFromSztMemoCache,
      testCase "memoizedFromSzt skips cache without shallowId" $ do
        resetFromSztMemoCache
        let dv =
              DynamicValue
                { _dvCore = DPrimitive (PInt 42),
                  _dvTypeInfo = Nothing,
                  _dvSchemaVersion = currentSchemaVersion,
                  _dvShallowId = Nothing
                }
        case fromSzt @Int dv of
          Left err -> assertBool ("fromSzt failed: " <> show err) False
          Right r -> r @?= 42
        cache <- readIORef fromSztMemoCache
        Map.size cache @?= 0
        resetFromSztMemoCache,
      testCase "resolveReferences stamps shallowId on shared entries" $ do
        -- Shared-table entries get a synthetic shallowId from
        -- resolveReferences, enabling memoizedFromSzt to cache them.
        let sharedDV =
              DynamicValue
                { _dvCore = DPrimitive (PInt 99),
                  _dvTypeInfo = Nothing,
                  _dvSchemaVersion = currentSchemaVersion,
                  _dvShallowId = Nothing
                }
            sharedTable = Map.singleton 0 (Right sharedDV)
            rootRef =
              DynamicValue
                { _dvCore = DReference 0,
                  _dvTypeInfo = Nothing,
                  _dvSchemaVersion = currentSchemaVersion,
                  _dvShallowId = Nothing
                }
        case resolveReferences sharedTable rootRef of
          Left err -> assertBool ("resolve failed: " <> show err) False
          Right resolved ->
            assertBool "shared entry should have shallowId"
              (isJust $ _dvShallowId resolved),
      testCase "withSztMemoization resets caches" $ do
        let _dv = toSzt (Widget "pre" 1)
        withSztMemoization $ do
          preCacheToSzt <- readIORef toSztMemoCache
          Map.size preCacheToSzt @?= 0
          let _dv2 = toSzt (Widget "inner" 2)
          pure ()
        postCacheToSzt <- readIORef toSztMemoCache
        Map.size postCacheToSzt @?= 0,
      testCase "resolveReferences preserves shallowId" $ do
        resetToSztMemoCache
        let w = Widget "delta" 40
            dv = toSzt w
            sharedTable = Map.singleton 0 (Right dv)
            rootRef =
              DynamicValue
                { _dvCore = DReference 0,
                  _dvTypeInfo = Nothing,
                  _dvSchemaVersion = currentSchemaVersion,
                  _dvShallowId = Nothing
                }
        case resolveReferences sharedTable rootRef of
          Left err -> assertBool ("resolveReferences failed: " <> show err) False
          Right resolved ->
            _dvShallowId resolved @?= _dvShallowId dv
        resetToSztMemoCache
    ]
