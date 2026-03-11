{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Additional ToSZT/FromSZT instances for standard library types
--
-- The type coverage and serialization patterns in this module are inspired
-- by aeson's approach to handling standard Haskell types, ensuring compatibility
-- with the same broad range of types that aeson supports.
--
-- This helps ensure that Serializotron can serve as a comprehensive serialization
-- alternative with similar type coverage to aeson.
--
-- Reference: https://github.com/haskell/aeson
module Serializotron.Instances where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Ratio (denominator, numerator, (%))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Time qualified as Time
import Data.Typeable (Proxy (..), Typeable, typeRep)
import Data.Vector qualified as Vector
import Data.Word (Word16, Word32, Word64, Word8)
import Numeric.Natural (Natural)
import Serializotron

--------------------------------------------------------------------------------
-- Unit type
--------------------------------------------------------------------------------

instance ToSZT () where
  toSzt () =
    DynamicValue
      { _dvCore = DUnit,
        _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "()",
                _tiModule = Just "GHC.Tuple",
                _tiConstructors = ["()"],
                _tiFieldLabels = [],
                _tiStructure = Just TSUnit,
                _tiTypeParameters = [],
                _tiNewtypeWrapper = Nothing
              },
        _dvSchemaVersion = currentSchemaVersion,
        _dvShallowId = Nothing
      }

instance FromSZT () where
  fromSzt (DynamicValue DUnit _ _ _) = Right ()
  fromSzt _ = Left (StructuralMismatch "Expected unit type")

--------------------------------------------------------------------------------
-- Integer type instances
--------------------------------------------------------------------------------

instance ToSZT Int8 where
  toSzt x = toSzt (fromIntegral x :: Int32) -- Use Int32 container

instance FromSZT Int8 where
  fromSzt dv = do
    i <- fromSzt dv :: Either SZTError Int32
    if i >= fromIntegral (minBound :: Int8) && i <= fromIntegral (maxBound :: Int8)
      then Right (fromIntegral i)
      else Left (StructuralMismatch "Int8 out of range")

instance ToSZT Int16 where
  toSzt x = toSzt (fromIntegral x :: Int32) -- Use Int32 container

instance FromSZT Int16 where
  fromSzt dv = do
    i <- fromSzt dv :: Either SZTError Int32
    if i >= fromIntegral (minBound :: Int16) && i <= fromIntegral (maxBound :: Int16)
      then Right (fromIntegral i)
      else Left (StructuralMismatch "Int16 out of range")

instance ToSZT Int32 where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PInt32 x),
        _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Int32",
                _tiModule = Just "GHC.Int",
                _tiConstructors = [],
                _tiFieldLabels = [],
                _tiStructure = Just (TSPrimitive PTInt32),
                _tiTypeParameters = [],
                _tiNewtypeWrapper = Nothing
              },
        _dvSchemaVersion = currentSchemaVersion,
        _dvShallowId = Nothing
      }

instance FromSZT Int32 where
  fromSzt (DynamicValue (DPrimitive (PInt32 x)) _ _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "Int32" "other")

instance ToSZT Int64 where
  toSzt x = toSzt (fromIntegral x :: Int) -- Int64 -> Int (both are int64 in protobuf)

instance FromSZT Int64 where
  fromSzt dv = do
    i <- fromSzt dv :: Either SZTError Int
    Right (fromIntegral i) -- No range check needed, both are 64-bit

-- Word instances
instance ToSZT Word8 where
  toSzt x = toSzt (fromIntegral x :: Word32) -- Use Word32 container

instance FromSZT Word8 where
  fromSzt dv = do
    i <- fromSzt dv :: Either SZTError Word32
    if i <= fromIntegral (maxBound :: Word8)
      then Right (fromIntegral i)
      else Left (StructuralMismatch "Word8 out of range")

instance ToSZT Word16 where
  toSzt x = toSzt (fromIntegral x :: Word32) -- Use Word32 container

instance FromSZT Word16 where
  fromSzt dv = do
    i <- fromSzt dv :: Either SZTError Word32
    if i <= fromIntegral (maxBound :: Word16)
      then Right (fromIntegral i)
      else Left (StructuralMismatch "Word16 out of range")

instance ToSZT Word32 where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PWord32 x),
        _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Word32",
                _tiModule = Just "GHC.Word",
                _tiConstructors = [],
                _tiFieldLabels = [],
                _tiStructure = Just (TSPrimitive PTWord32),
                _tiTypeParameters = [],
                _tiNewtypeWrapper = Nothing
              },
        _dvSchemaVersion = currentSchemaVersion,
        _dvShallowId = Nothing
      }

instance FromSZT Word32 where
  fromSzt (DynamicValue (DPrimitive (PWord32 x)) _ _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "Word32" "other")

instance ToSZT Word64 where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PWord64 x),
        _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Word64",
                _tiModule = Just "GHC.Word",
                _tiConstructors = [],
                _tiFieldLabels = [],
                _tiStructure = Just (TSPrimitive PTWord64),
                _tiTypeParameters = [],
                _tiNewtypeWrapper = Nothing
              },
        _dvSchemaVersion = currentSchemaVersion,
        _dvShallowId = Nothing
      }

instance FromSZT Word64 where
  fromSzt (DynamicValue (DPrimitive (PWord64 x)) _ _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "Word64" "other")

-- Integer and Natural
instance ToSZT Integer where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PInteger (Text.pack (show x))),
        _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Integer",
                _tiModule = Just "GHC.Integer",
                _tiConstructors = [],
                _tiFieldLabels = [],
                _tiStructure = Just (TSPrimitive PTInteger),
                _tiTypeParameters = [],
                _tiNewtypeWrapper = Nothing
              },
        _dvSchemaVersion = currentSchemaVersion,
        _dvShallowId = Nothing
      }

instance FromSZT Integer where
  fromSzt (DynamicValue (DPrimitive (PInteger t)) _ _ _) =
    case reads (Text.unpack t) of
      [(n, "")] -> Right n
      _ -> Left (StructuralMismatch "Invalid Integer format")
  fromSzt _ = Left (PrimitiveMismatch "Integer" "other")

instance ToSZT Natural where
  toSzt x = toSzt (toInteger x)

instance FromSZT Natural where
  fromSzt dv = do
    i <- fromSzt dv :: Either SZTError Integer
    if i >= 0
      then Right (fromInteger i)
      else Left (StructuralMismatch "Natural must be non-negative")

-- Float
instance ToSZT Float where
  toSzt x = toSzt (realToFrac x :: Double)

instance FromSZT Float where
  fromSzt dv = realToFrac <$> (fromSzt dv :: Either SZTError Double)

-- Rational
instance ToSZT Rational where
  toSzt r = toSzt (numerator r, denominator r)

instance FromSZT Rational where
  fromSzt dv = do
    (num, den) <- fromSzt dv :: Either SZTError (Integer, Integer)
    if den == 0
      then Left (StructuralMismatch "Denominator cannot be zero")
      else Right (num % den)

-- Char and String
instance ToSZT Char where
  toSzt c = toSzt (Text.singleton c)

instance FromSZT Char where
  fromSzt dv = do
    t <- fromSzt dv :: Either SZTError Text
    case Text.unpack t of
      [c] -> Right c
      _ -> Left (StructuralMismatch "Expected single character")

-- Note: String = [Char], so it uses the list instance with Char instance

-- Lazy Text
instance ToSZT LText.Text where
  toSzt lt = toSzt (LText.toStrict lt)

instance FromSZT LText.Text where
  fromSzt dv = LText.fromStrict <$> fromSzt dv

--------------------------------------------------------------------------------
-- Time instances
--------------------------------------------------------------------------------

-- Store times as ISO8601 strings for portability
instance ToSZT Time.Day where
  toSzt d = toSzt (Text.pack (show d))

instance FromSZT Time.Day where
  fromSzt dv = do
    t <- fromSzt dv :: Either SZTError Text
    case reads (Text.unpack t) of
      [(d, "")] -> Right d
      _ -> Left (StructuralMismatch "Invalid Day format")

instance ToSZT Time.TimeOfDay where
  toSzt tod = toSzt (Text.pack (show tod))

instance FromSZT Time.TimeOfDay where
  fromSzt dv = do
    t <- fromSzt dv :: Either SZTError Text
    case reads (Text.unpack t) of
      [(tod, "")] -> Right tod
      _ -> Left (StructuralMismatch "Invalid TimeOfDay format")

instance ToSZT Time.LocalTime where
  toSzt lt = toSzt (Text.pack (show lt))

instance FromSZT Time.LocalTime where
  fromSzt dv = do
    t <- fromSzt dv :: Either SZTError Text
    case reads (Text.unpack t) of
      [(lt, "")] -> Right lt
      _ -> Left (StructuralMismatch "Invalid LocalTime format")

instance ToSZT Time.UTCTime where
  toSzt ut = toSzt (Text.pack (Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" ut))

instance FromSZT Time.UTCTime where
  fromSzt dv = do
    t <- fromSzt dv :: Either SZTError Text
    case Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (Text.unpack t) of
      Just ut -> Right ut
      Nothing -> Left (StructuralMismatch "Invalid UTCTime format")

instance ToSZT Time.ZonedTime where
  toSzt zt = toSzt (Text.pack (show zt))

instance FromSZT Time.ZonedTime where
  fromSzt dv = do
    t <- fromSzt dv :: Either SZTError Text
    case reads (Text.unpack t) of
      [(zt, "")] -> Right zt
      _ -> Left (StructuralMismatch "Invalid ZonedTime format")

instance ToSZT Time.DiffTime where
  toSzt dt = toSzt (realToFrac dt :: Double)

instance FromSZT Time.DiffTime where
  fromSzt dv = realToFrac <$> (fromSzt dv :: Either SZTError Double)

instance ToSZT Time.NominalDiffTime where
  toSzt ndt = toSzt (realToFrac ndt :: Double)

instance FromSZT Time.NominalDiffTime where
  fromSzt dv = realToFrac <$> (fromSzt dv :: Either SZTError Double)

--------------------------------------------------------------------------------
-- Collection instances
--------------------------------------------------------------------------------

-- NonEmpty
instance (ToSZT a) => ToSZT (NonEmpty a) where
  toSzt ne = toSzt (NE.toList ne)

instance (FromSZT a) => FromSZT (NonEmpty a) where
  fromSzt dv = do
    lst <- fromSzt dv :: Either SZTError [a]
    case lst of
      [] -> Left (StructuralMismatch "NonEmpty cannot be empty")
      (x : xs) -> Right (x :| xs)

-- Map
instance (ToSZT k, ToSZT v) => ToSZT (Map.Map k v) where
  toSzt m = toSzt (Map.toList m)

instance (Ord k, FromSZT k, FromSZT v) => FromSZT (Map.Map k v) where
  fromSzt dv = Map.fromList <$> fromSzt dv

-- Set
instance (ToSZT a) => ToSZT (Set.Set a) where
  toSzt s = toSzt (Set.toList s)

instance (Ord a, FromSZT a) => FromSZT (Set.Set a) where
  fromSzt dv = Set.fromList <$> fromSzt dv

-- HashMap
instance (ToSZT k, ToSZT v) => ToSZT (HashMap.HashMap k v) where
  toSzt m = toSzt (HashMap.toList m)

instance (Hashable k, FromSZT k, FromSZT v) => FromSZT (HashMap.HashMap k v) where
  fromSzt dv = HashMap.fromList <$> fromSzt dv

-- HashSet
instance (ToSZT a) => ToSZT (HashSet.HashSet a) where
  toSzt s = toSzt (HashSet.toList s)

instance (Hashable a, FromSZT a) => FromSZT (HashSet.HashSet a) where
  fromSzt dv = HashSet.fromList <$> fromSzt dv

-- Vector
instance (ToSZT a) => ToSZT (Vector.Vector a) where
  toSzt v = toSzt (Vector.toList v)

instance (FromSZT a) => FromSZT (Vector.Vector a) where
  fromSzt dv = Vector.fromList <$> fromSzt dv

--------------------------------------------------------------------------------
-- Optional and Either instances
--------------------------------------------------------------------------------

-- Maybe
instance (ToSZT a, Typeable a) => ToSZT (Maybe a) where
  toSzt val =
    DynamicValue
      { _dvCore = case val of
          Nothing -> DSum 0 (DynamicValue DUnit Nothing currentSchemaVersion Nothing)
          Just x -> DSum 1 (toSzt x),
        _dvTypeInfo =
          Just $
            (typeInfoForRep (typeRep (Proxy :: Proxy (Maybe a))))
              { _tiConstructors = ["Nothing", "Just"]
              },
        _dvSchemaVersion = currentSchemaVersion,
        _dvShallowId = Nothing
      }

instance (FromSZT a) => FromSZT (Maybe a) where
  fromSzt (DynamicValue (DSum 0 _) _ _ _) = Right Nothing
  fromSzt (DynamicValue (DSum 1 v) _ _ _) = Just <$> fromSzt v
  fromSzt _ = Left (StructuralMismatch "Invalid Maybe structure")

-- Either
instance (ToSZT a, ToSZT b, Typeable a, Typeable b) => ToSZT (Either a b) where
  toSzt val =
    DynamicValue
      { _dvCore = case val of
          Left x -> DSum 0 (toSzt x)
          Right y -> DSum 1 (toSzt y),
        _dvTypeInfo =
          Just $
            (typeInfoForRep (typeRep (Proxy :: Proxy (Either a b))))
              { _tiConstructors = ["Left", "Right"]
              },
        _dvSchemaVersion = currentSchemaVersion,
        _dvShallowId = Nothing
      }

instance (FromSZT a, FromSZT b) => FromSZT (Either a b) where
  fromSzt (DynamicValue (DSum 0 v) _ _ _) = Left <$> fromSzt v
  fromSzt (DynamicValue (DSum 1 v) _ _ _) = Right <$> fromSzt v
  fromSzt _ = Left (StructuralMismatch "Invalid Either structure")
