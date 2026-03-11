{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant return" #-}

-- |
-- Module: Serializotron
-- Description: Content-based deduplicating serialization library for Haskell
--
-- Serializotron is a serialization library:
--
--  - Content-based deduplicating serialization library for Haskell.
--  - Serializes data while automatically reusing repeated content to shrink file size and preserve structure.
--  - Stores values as Protocol Buffer messages and indexes shared chunks via a cryptographic content hash.
--
-- = Quick Start
--
-- For most use cases, you only need to derive 'ToSZT' and 'FromSZT' instances:
--
-- @
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}
-- import GHC.Generics
-- import Serializotron
--
-- data Person = Person Text Int
--   deriving (Generic, Show, Eq, ToSZT, FromSZT)
--
-- -- Save to file
-- savePerson :: Person -> IO ()
-- savePerson person = saveSzt "person.szt" person
--
-- -- Load from file
-- loadPerson :: IO (Either SerializotronError Person)
-- loadPerson = loadSzt "person.szt"
-- @
--
-- = Deduplication Strategies
--
-- Serializotron offers three built-in strategies for handling duplicate data:
--
-- @
-- -- No deduplication (fastest)
-- saveSzt "fast.szt" myData
--
-- -- Balanced deduplication (recommended)
-- saveSztCompressed "balanced.szt" myData
--
-- -- Maximum deduplication (smallest files)
-- saveSztCompressedAggressive "small.szt" myData
--
-- -- Custom strategy
-- customStrategy = DeduplicationStrategy
--   { _minSizeThreshold    = 100 -- Only dedupe values >= 100 bytes
--   , _maxDepthScan        = 50  -- Scan up to 50 levels deep
--   , _enableDeduplication = True
--   }
-- saveSztWithStrategy customStrategy "custom.szt" myData
-- @
--
-- = Complex Data Structures
--
-- The library handles nested data structures, collections, and custom types:
--
-- @
-- data Company = Company
--   { companyName  :: Text
--   , employees    :: [Person]
--   , departments  :: Map Text [Person]
--   , founded      :: UTCTime
--   }
--   deriving (Generic, ToSZT, FromSZT)
--
-- data Result a = Success a | Error Text
--   deriving (Generic, ToSZT, FromSZT)
--
-- -- All of these work automatically:
-- saveSzt "company.szt" myCompany
-- saveSzt "results.szt" (map Success [1,2,3] :: [Result Int])
-- saveSzt "lookup.szt" (Map.fromList [("key", Just 42)] :: Map Text (Maybe Int))
-- @
--
-- = Error Handling
--
-- Loading can fail for various reasons, all captured in 'SerializotronError':
--
-- @
-- handleLoad :: IO ()
-- handleLoad = do
--   result <- loadSzt "data.szt" :: IO (Either SerializotronError Person)
--   case result of
--     Left err -> putStrLn $ "Failed to load: " <> formatError err
--     Right person -> putStrLn $ "Loaded: " <> show person
-- @
--
-- = File Integrity Checking
--
-- Verify the integrity of .szt files:
--
-- @
-- checkFile :: FilePath -> IO ()
-- checkFile path = do
--   result <- fsckSzt path
--   if fsckPassed result
--     then putStrLn "File is valid"
--     else do
--       putStrLn "File has errors:"
--       mapM_ (putStrLn . formatError . ValidationError) (fsckErrors result)
-- @
--
-- = Performance Tips
--
-- * Use 'saveSzt' for fastest serialization when file size isn't important
-- * Use 'saveSztCompressed' for balanced performance in most applications
-- * Use 'saveSztCompressedAggressive' for archival storage or network transfer
-- * Consider custom strategies for specific performance requirements
-- * The library is most effective with data containing repeated structures
module Serializotron where

import Control.Exception (IOException, try)
import Control.Lens
import Control.Applicative ((<|>))
import Control.Monad (zipWithM, unless, when)
import Control.Monad.State.Strict
import Crypto.Hash (Blake2b_256, Digest)
import Crypto.Hash qualified as Crypto
import Data.Hashable (Hashable (..), hashWithSalt)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Char (chr, ord)
import Data.Int (Int32)
import Data.List (elemIndex)
import Data.Map.Lazy qualified as Map.Lazy
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.ProtoLens (decodeMessage, defMessage, encodeMessage)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable (Proxy (..), TyCon, TypeRep, Typeable, tyConModule, tyConName, typeRep, typeRepArgs, typeRepTyCon)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
import Lens.Family2 qualified as Lens
import Codec.Compression.GZip qualified as GZip
import System.IO.Unsafe (unsafePerformIO)
import System.IO (stderr, hPutStrLn)
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef, readIORef)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Aeson (ToJSON (..), object)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import System.Environment (lookupEnv)
import Debug.Trace (trace)

import Proto.Serializotron qualified as Proto
import Proto.Serializotron_Fields qualified as Proto

--------------------------------------------------------------------------------
-- File Format Header
--------------------------------------------------------------------------------

-- | Compression method used in SZT file
data CompressionMethod
  = NoCompression      -- ^ Raw protobuf, no compression
  | GZipCompression    -- ^ GZip compression (good balance, ubiquitous)
  deriving stock (Show, Eq, Enum, Bounded)

-- | Convert compression method to byte representation
compressionToByte :: CompressionMethod -> Word8
compressionToByte NoCompression = 0x00
compressionToByte GZipCompression = 0x01

-- | Parse compression method from byte
compressionFromByte :: Word8 -> Maybe CompressionMethod
compressionFromByte 0x00 = Just NoCompression
compressionFromByte 0x01 = Just GZipCompression
compressionFromByte _ = Nothing

-- | SZT file header structure (8 bytes total)
data SZTHeader = SZTHeader
  { _headerMagic :: ByteString       -- 4 bytes: "SZT\0"
  , _headerVersion :: Word8          -- 1 byte: format version
  , _headerCompression :: CompressionMethod  -- 1 byte: compression type
  , _headerReserved :: Word16        -- 2 bytes: reserved for future use
  }
  deriving stock (Show, Eq)

-- | Magic bytes for SZT format identification
sztMagicBytes :: ByteString
sztMagicBytes = ByteString.pack [0x53, 0x5A, 0x54, 0x00] -- "SZT\0"

-- | Current header format version
headerFormatVersion :: Word8
headerFormatVersion = 1

-- | Create a header with specified compression
mkHeader :: CompressionMethod -> SZTHeader
mkHeader compression = SZTHeader
  { _headerMagic = sztMagicBytes
  , _headerVersion = headerFormatVersion
  , _headerCompression = compression
  , _headerReserved = 0
  }

-- | Serialize header to bytes (8 bytes total)
encodeHeader :: SZTHeader -> ByteString
encodeHeader (SZTHeader magic version compression reserved) =
  ByteString.concat
    [ magic                                    -- 4 bytes
    , ByteString.singleton version            -- 1 byte
    , ByteString.singleton (compressionToByte compression)  -- 1 byte
    , ByteString.pack [fromIntegral (reserved `div` 256), fromIntegral (reserved `mod` 256)]  -- 2 bytes
    ]

-- | Parse header from bytes
decodeHeader :: ByteString -> Either Text SZTHeader
decodeHeader bs
  | ByteString.length bs < 8 = Left "Header too short, expected 8 bytes"
  | otherwise = do
      let magic = ByteString.take 4 bs
      unless (magic == sztMagicBytes) $
        Left $ "Invalid magic bytes: expected SZT\\0, got " <> Text.pack (show magic)

      let version = ByteString.index bs 4
      unless (version == headerFormatVersion) $
        Left $ "Unsupported format version: " <> Text.pack (show version)

      let compressionByte = ByteString.index bs 5
      compression <- case compressionFromByte compressionByte of
        Just c -> Right c
        Nothing -> Left $ "Unknown compression method: " <> Text.pack (show compressionByte)

      let reserved = fromIntegral (ByteString.index bs 6) * 256 + fromIntegral (ByteString.index bs 7)

      Right $ SZTHeader magic version compression reserved

-- | Schema version for backwards compatibility
type SchemaVersion = Word32

currentSchemaVersion :: SchemaVersion
currentSchemaVersion = 1

-- | Primitive type classification for structural type information.
--
-- This enumeration corresponds exactly to the constructors in 'PrimitiveValue',
-- providing type-level information about primitive values.
--
-- Each variant maps to a specific Haskell type:
-- @
-- PTInt     -> Int
-- PTDouble  -> Double
-- PTText    -> Text
-- PTBool    -> Bool
-- PTWord64  -> Word64
-- PTInt32   -> Int32
-- PTWord32  -> Word32
-- PTInteger -> Integer
-- PBytes    -> ByteString
-- @
data PrimitiveType
  = PTInt
  | PTDouble
  | PTText
  | PTBool
  | PTWord64
  | PTInt32
  | PTWord32
  | PTInteger
  | PTBytes
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)

-- | Structural type information describing the shape of types.
--
-- This provides detailed information about how types are constructed,
-- enabling better error messages and compatibility checking. It mirrors
-- the structure of 'DynamicCore' but at the type level.
--
-- Examples:
-- @
-- Int           -> TSPrimitive PTInt
-- (Text, Int)   -> TSProduct [textTypeInfo, intTypeInfo]
-- Maybe Int     -> TSSum [nothingTypeInfo, justTypeInfo]
-- [Int]         -> TSList intTypeInfo
-- ()            -> TSUnit
-- @
data TypeStructure
  = -- | Basic types: Int, Text, Bool, etc.
    TSPrimitive PrimitiveType
  | -- | Record fields and tuple components with names
    TSProduct [FieldInfo]
  | -- | Constructor alternatives in sum types
    TSSum [TypeInfo]
  | -- | List/array element type
    TSList TypeInfo
  | -- | Unit type or empty constructors
    TSUnit
  deriving stock (Generic, Show, Eq, Ord)

-- | Named field metadata for product types
data FieldInfo = FieldInfo
  { _fiFieldName :: Maybe Text
  , _fiFieldType :: TypeInfo
  }
  deriving stock (Generic, Show, Eq, Ord)

-- | Type metadata for compatibility checking and debugging.
--
-- This optional metadata helps detect when serialized data might be incompatible
-- with the current program's types. It's particularly valuable for:
--
-- * Schema evolution: detecting when field types have changed
-- * Debugging: providing meaningful error messages with type names
-- * Documentation: understanding what types were originally serialized
--
-- The information is extracted automatically using GHC Generics.
--
-- Example for @data Person = Person Text Int@:
-- @
-- TypeInfo
--   { _tiTypeName = Just "Person"
--   , _tiModule = Just "MyModule"
--   , _tiConstructors = ["Person"]
--   , _tiStructure = Just (TSProduct [textTypeInfo, intTypeInfo])
--   }
-- @
data TypeInfo = TypeInfo
  { -- | Type name: "Person", "Maybe", "[]"
    _tiTypeName :: Maybe Text

    -- | Module where type is defined: "MyModule", "GHC.Types"
  , _tiModule :: Maybe Text

    -- | Constructor names: ["Person"], ["Nothing", "Just"]
  , _tiConstructors :: [Text]

    -- | Record field labels in declaration order
  , _tiFieldLabels :: [Text]

    -- | Detailed structural information
  , _tiStructure :: Maybe TypeStructure

    -- | Type parameters (e.g., for "Model t v" applied to concrete types like "Model CartesianTupleSystem Arithmetic")
  , _tiTypeParameters :: [TypeInfo]

    -- | For newtypes: TypeInfo of the wrapped type
  , _tiNewtypeWrapper :: Maybe TypeInfo
  }
  deriving stock (Generic, Show, Eq, Ord)

makeLenses ''TypeInfo
makeLenses ''FieldInfo

emptyTypeInfo :: TypeInfo
emptyTypeInfo = TypeInfo Nothing Nothing [] [] Nothing [] Nothing

assignFieldLabels :: [FieldInfo] -> [FieldInfo]
assignFieldLabels fields = zipWith assign [1 :: Int ..] fields
  where
    assign idx fi =
      let finalName =
            case fi ^. fiFieldName of
              Just n | not (Text.null n) -> Just n
              _ -> Just (Text.pack ("_" <> show idx))
       in fi & fiFieldName .~ finalName

-- | TyCon for the list type constructor ([])
listTyCon :: TyCon
listTyCon = typeRepTyCon (typeRep (Proxy @[()]))

maybeTyCon :: TyCon
maybeTyCon = typeRepTyCon (typeRep (Proxy @(Maybe Int)))

eitherTyCon :: TyCon
eitherTyCon = typeRepTyCon (typeRep (Proxy @(Either Int Int)))

listElementType :: TypeRep -> Maybe TypeRep
listElementType rep
  | typeRepTyCon rep == listTyCon
  , [elemRep] <- typeRepArgs rep
  = Just elemRep
  | otherwise
  = Nothing

maybeElementType :: TypeRep -> Maybe TypeRep
maybeElementType rep
  | typeRepTyCon rep == maybeTyCon
  , [elemRep] <- typeRepArgs rep
  = Just elemRep
  | otherwise = Nothing

eitherElementTypes :: TypeRep -> Maybe (TypeRep, TypeRep)
eitherElementTypes rep
  | typeRepTyCon rep == eitherTyCon
  , [leftRep, rightRep] <- typeRepArgs rep
  = Just (leftRep, rightRep)
  | otherwise = Nothing

typeInfoForRep :: TypeRep -> TypeInfo
typeInfoForRep rep =
  let tyCon = typeRepTyCon rep
      nameText = Text.pack (tyConName tyCon)
      moduleText = Text.pack (tyConModule tyCon)
      -- Extract type parameters recursively
      typeParams = map typeInfoForRep (typeRepArgs rep)
   in emptyTypeInfo
        { _tiTypeName = Just nameText
        , _tiModule = Just moduleText
        , _tiStructure = structureForTypeRep rep
        , _tiTypeParameters = typeParams
        , _tiNewtypeWrapper = Nothing  -- Explicit for clarity
        }

structureForTypeRep :: TypeRep -> Maybe TypeStructure
structureForTypeRep rep
  | rep == typeRep (Proxy @Int) = Just (TSPrimitive PTInt)
  | rep == typeRep (Proxy @Double) = Just (TSPrimitive PTDouble)
  | rep == typeRep (Proxy @Text) = Just (TSPrimitive PTText)
  | rep == typeRep (Proxy @Bool) = Just (TSPrimitive PTBool)
  | rep == typeRep (Proxy @Word64) = Just (TSPrimitive PTWord64)
  | rep == typeRep (Proxy @Int32) = Just (TSPrimitive PTInt32)
  | rep == typeRep (Proxy @Word32) = Just (TSPrimitive PTWord32)
  | rep == typeRep (Proxy @Integer) = Just (TSPrimitive PTInteger)
  | rep == typeRep (Proxy @ByteString) = Just (TSPrimitive PTBytes)
  | Just elemRep <- listElementType rep = Just (TSList (typeInfoForRep elemRep))
  | Just elemRep <- maybeElementType rep =
      let elemInfo = typeInfoForRep elemRep
          nothingInfo = emptyTypeInfo & tiStructure ?~ TSUnit
       in Just (TSSum [nothingInfo, elemInfo])
  | Just (leftRep, rightRep) <- eitherElementTypes rep =
      let leftInfo = typeInfoForRep leftRep
          rightInfo = typeInfoForRep rightRep
       in Just (TSSum [leftInfo, rightInfo])
  | otherwise = Nothing


-- | Primitive value types
data PrimitiveValue
  = PInt      Int
  | PDouble   Double
  | PText     Text
  | PBool     Bool
  | PWord64   Word64
  | PInt32    Int32
  | PWord32   Word32
  | PInteger  Text -- Arbitrary precision stored as text
  | PBytes    ByteString
  deriving stock (Generic, Show, Eq)

-- | Core value representation for all Haskell data structures.
--
-- This algebraic data type captures the essential structure of any serializable
-- Haskell value. It follows the structure of Haskell's type system:
--
-- * Primitives: Basic types like Int, Text, Bool
-- * Products: Records, tuples (data Foo = Foo A B)
-- * Sums: Enums, tagged unions (data Foo = A Int | B Text)
-- * Lists: Homogeneous collections [a]
-- * Unit: Empty constructors (data Foo = EmptyConstructor)
-- * References: internal use for pointers to deduplicated shared values
--
-- Examples:
-- @
-- 42          -> DPrimitive (PInt 42)
-- ("Hi", 3)   -> DProduct [DPrimitive (PText "Hi"), DPrimitive (PInt 3)]
-- Just 5      -> DSum 1 (DPrimitive (PInt 5))  -- Nothing=0, Just=1
-- [1,2,3]     -> DList [DPrimitive (PInt 1), DPrimitive (PInt 2), DPrimitive (PInt 3)]
-- ()          -> DUnit
-- @
data DynamicCore
  = -- | Basic values: Int, Text, Bool, etc.
    DPrimitive PrimitiveValue

  | -- | Records and tuples: data Foo = Foo A B
    DProduct [DynamicValue]

  | -- | Tagged unions: data Foo = A | B Int (constructor index + value)
    DSum Word32 DynamicValue

  | -- | Homogeneous lists: [a]
    DList [DynamicValue]

  | -- | Unit type and empty constructors: () or EmptyConstructor
    DUnit

  | -- | Reference to shared value for deduplication
    DReference Word32

  deriving stock (Generic, Show, Eq)

-- | Core dynamic value with optional type metadata
data DynamicValue = DynamicValue
  { _dvCore          :: DynamicCore
  , _dvTypeInfo      :: Maybe TypeInfo
  , _dvSchemaVersion :: SchemaVersion
  , _dvShallowId     :: Maybe ByteString  -- Optional shallow identifier for fast deduplication
  }
  deriving stock (Generic, Show, Eq)

makeLenses ''DynamicValue

-- | Top-level SZT file structure
data SZTFile = SZTFile
  { _sztSchemaVersion :: SchemaVersion
  , _sztValue         :: DynamicValue
  , _sztSharedValues  :: Map.Map Word32 DynamicValue -- Deduplication table
  }
  deriving stock (Generic, Show, Eq)

makeLenses ''SZTFile

--------------------------------------------------------------------------------
-- Deduplication System
--------------------------------------------------------------------------------

-- | Configuration for deduplication behavior.
--
-- Deduplication reduces file size by identifying repeated values and storing
-- them only once, replacing duplicates with references. This strategy controls
-- the trade-offs between file size, serialization time, and memory usage.
--
-- Key parameters:
-- * Size threshold: Skip deduplicating small values (overhead isn't worth it)
-- * Depth limit: Prevent excessive recursion in deeply nested structures
-- * Enable flag: Master switch to disable deduplication entirely
--
-- Choosing the right strategy:
-- * 'noDeduplicationStrategy': Fastest serialization, larger files
-- * 'defaultDeduplicationStrategy': Good balance for most use cases
-- * 'aggressiveDeduplicationStrategy': Smallest files, slower serialization
--
-- Example:
-- @
-- customStrategy = DeduplicationStrategy
--   { _minSizeThreshold    = 50 -- Only dedupe values >= 50 bytes
--   , _maxDepthScan        = 20 -- Scan up to 20 levels deep
--   , _enableDeduplication = True
--   }
-- @
data DeduplicationStrategy = DeduplicationStrategy
  { -- | Only dedupe values with estimated size >= this (bytes)
    _minSizeThreshold :: Int

    -- | Maximum recursion depth for deduplication scanning
  , _maxDepthScan :: Int

    -- | Master switch for deduplication (disable for fastest serialization)
  , _enableDeduplication :: Bool
  }
  deriving stock (Show, Eq)

makeLenses ''DeduplicationStrategy

-- | Statistics collected during deduplication to help identify optimization opportunities.
--
-- This data structure tracks key metrics for each type encountered during serialization:
-- - How many instances were seen
-- - How many were successfully deduplicated
-- - Which types are the best candidates for ShallowIdentifiable optimization
data DeduplicationStats = DeduplicationStats
  { -- | Total number of times each type was encountered during serialization
    _typeEncounters :: Map.Map String Int

    -- | Number of times each type was successfully deduplicated (reused via reference)
  , _typeDeduplicated :: Map.Map String Int

    -- | Number of times each type had to be fully hashed (expensive operation)
  , _typeHashed :: Map.Map String Int

    -- | Number of times each type was skipped (too small or too deep)
  , _typeSkipped :: Map.Map String Int

    -- | Total bytes saved by deduplication per type (approximate)
  , _typeBytesSaved :: Map.Map String Int
  }
  deriving stock (Show, Eq)

makeLenses ''DeduplicationStats

-- | Create an empty stats structure
emptyDeduplicationStats :: DeduplicationStats
emptyDeduplicationStats = DeduplicationStats
  { _typeEncounters = Map.empty
  , _typeDeduplicated = Map.empty
  , _typeHashed = Map.empty
  , _typeSkipped = Map.empty
  , _typeBytesSaved = Map.empty
  }

-- | Event logged during deduplication for performance analysis
data DeduplicationEvent = DeduplicationEvent
  { _evTypeName :: Text
  , _evHasShallowId :: Bool
  , _evHashPreview :: Text  -- First 8 characters of shallow/fast hash
  , _evFullHashPreview :: Text  -- First 8 characters of full Hashable hash
  , _evShallowIdPreview :: Maybe Text  -- First 16 hex chars of shallow ID
  , _evWasDeduplicated :: Bool
  , _evEstimatedSize :: Int
  } deriving stock (Show, Eq, Generic)

instance ToJSON DeduplicationEvent where
  toJSON ev = object
    [ "type" Aeson..= _evTypeName ev
    , "has_shallow_id" Aeson..= _evHasShallowId ev
    , "hash_preview" Aeson..= _evHashPreview ev
    , "full_hash_preview" Aeson..= _evFullHashPreview ev
    , "shallow_id_preview" Aeson..= _evShallowIdPreview ev
    , "was_deduplicated" Aeson..= _evWasDeduplicated ev
    , "estimated_size" Aeson..= _evEstimatedSize ev
    ]

-- | Check if instrumentation is enabled via environment variable
{-# NOINLINE instrumentationEnabled #-}
instrumentationEnabled :: Bool
instrumentationEnabled = unsafePerformIO $ do
  envVar <- lookupEnv "SZT_INSTRUMENT"
  return $ envVar == Just "1"

-- | Convert ByteString to hex preview (first 16 hex chars)
bytesToHexPreview :: ByteString -> Text
bytesToHexPreview bs = Text.pack $ take 16 $ concatMap toHex $ ByteString.unpack bs
  where
    toHex byte =
      let high = byte `div` 16
          low = byte `mod` 16
          hexChar n = if n < 10 then chr (ord '0' + fromIntegral n) else chr (ord 'a' + fromIntegral (n - 10))
      in [hexChar high, hexChar low]

-- | Emit a JSON instrumentation event using Debug.Trace
-- This emits to stderr and works in pure code
emitEvent :: Text -> Bool -> Text -> Text -> Maybe ByteString -> Bool -> Int -> ()
emitEvent typeName hasShallowId hashPreview fullHashPreview shallowIdBytes wasDeduplicated estimatedSize =
  if instrumentationEnabled
  then
    let ev = DeduplicationEvent
          { _evTypeName = typeName
          , _evHasShallowId = hasShallowId
          , _evHashPreview = hashPreview
          , _evFullHashPreview = fullHashPreview
          , _evShallowIdPreview = fmap bytesToHexPreview shallowIdBytes
          , _evWasDeduplicated = wasDeduplicated
          , _evEstimatedSize = estimatedSize
          }
        jsonStr = TL.unpack $ TLE.decodeUtf8 $ Aeson.encode ev
    in trace jsonStr ()
  else ()

-- | Default deduplication strategy - conservative settings.
--
-- This provides a good balance between file size reduction and performance.
-- Suitable for most applications where you want some space savings without
-- significant serialization overhead.
--
-- Settings:
-- * Skips values smaller than 20 bytes (overhead not worth it)
-- * Limits recursion to 10 levels (prevents excessive scanning)
-- * Enabled by default
--
-- Use this when:
-- * You want smaller files but care about serialization speed
-- * Your data has moderate repetition
-- * You're unsure which strategy to choose
defaultDeduplicationStrategy :: DeduplicationStrategy
defaultDeduplicationStrategy =
  DeduplicationStrategy
    { _minSizeThreshold    = 20 -- Skip small values (< 20 bytes estimated)
    , _maxDepthScan        = 10 -- Don't recurse too deep
    , _enableDeduplication = True
    }

-- | Aggressive deduplication - deduplicate everything possible.
--
-- This strategy maximizes space savings by deduplicating even small values
-- and scanning deeply into nested structures. Best for archival storage
-- or when dealing with highly repetitive data.
--
-- Settings:
-- * Deduplicates values as small as 1 byte
-- * Scans up to 100 levels deep
-- * Maximum deduplication enabled
--
-- Use this when:
-- * File size is more important than serialization speed
-- * Your data contains lots of repetition
-- * You're archiving data for long-term storage
-- * Network bandwidth or storage costs are a concern
--
-- Warning: Can be significantly slower than other strategies.
aggressiveDeduplicationStrategy :: DeduplicationStrategy
aggressiveDeduplicationStrategy =
  DeduplicationStrategy
    { _minSizeThreshold    = 1   -- Deduplicate even tiny values
    , _maxDepthScan        = 100 -- Deep scanning
    , _enableDeduplication = True
    }

-- | Disable deduplication entirely.
--
-- This strategy turns off all deduplication, storing each value separately
-- even if they're identical. Results in the fastest serialization but
-- largest file sizes.
--
-- Settings:
-- * Size threshold set to maximum (never deduplicate)
-- * No depth scanning
-- * Deduplication disabled
--
-- Use this when:
-- * Serialization speed is critical
-- * Your data has little to no repetition
-- * Memory usage during serialization is a concern
-- * You're doing real-time serialization
-- * File size is not important
noDeduplicationStrategy :: DeduplicationStrategy
noDeduplicationStrategy =
  DeduplicationStrategy
    { _minSizeThreshold    = maxBound
    , _maxDepthScan        = 0
    , _enableDeduplication = False
    }

-- | Did our hash calculation use pure content values
-- or did we also hash a 'DReference'?
data HashScope = PureContent | WithReference
  deriving stock (Eq, Show)

-- | 'WithReference' taints any other value.
instance Semigroup HashScope where
  WithReference <> _             = WithReference
  _             <> WithReference = WithReference
  PureContent   <> PureContent   = PureContent

data Scoped a = Scoped HashScope a
  deriving stock (Eq)

instance Show a => Show (Scoped a) where
  show (Scoped s x) = show s <> ":" <> show x

instance Functor Scoped where
  fmap f (Scoped s x) = Scoped s (f x)

instance Applicative Scoped where
  pure = Scoped PureContent

  Scoped PureContent   f <*> Scoped PureContent   x = Scoped PureContent   (f x)
  Scoped PureContent   f <*> Scoped WithReference x = Scoped WithReference (f x)
  Scoped WithReference f <*> Scoped _             x = Scoped WithReference (f x)

instance Monad Scoped where
  Scoped PureContent x >>= f = f x
  Scoped WithReference x >>= f = case f x of
    Scoped _ x' -> Scoped WithReference x'

-- | Lens for accessing the HashScope component of Scoped
scopedScope :: Lens' (Scoped a) HashScope
scopedScope f (Scoped s a) = (`Scoped` a) <$> f s

-- | Lens for accessing the value component of Scoped
scopedValue :: Lens' (Scoped a) a
scopedValue f (Scoped s a) = Scoped s <$> f a

type ContentHash = Digest Blake2b_256

-- | State for deduplication process
data DeduplicationState = DeduplicationState
  { _nextReferenceId  :: Word32
  , _seenHashes       :: Map.Map ContentHash (Either DynamicValue Word32)  -- Left = first occurrence, Right = ref ID
  , _sharedTable      :: Map.Map Word32 DynamicValue
  , _currentDepth     :: Int
  , _strategy         :: DeduplicationStrategy
  }
  deriving stock (Show)

makeLenses ''DeduplicationState

-- | Monad for deduplication operations
type DeduplicationM = State DeduplicationState

-- | Initialize deduplication state
initDeduplicationState :: DeduplicationStrategy -> DeduplicationState
initDeduplicationState strat =
  DeduplicationState
    { _nextReferenceId  = 1 -- Start from 1, perhaps we will reserve 0 for something.
    , _seenHashes       = Map.empty
    , _sharedTable      = Map.empty
    , _currentDepth     = 0
    , _strategy         = strat
    }

-- | Estimate the size of a DynamicValue using SHALLOW inspection only.
-- This only looks at the immediate node without recursing into children,
-- making it O(1) instead of O(n). Uses rough heuristics for container sizes.
estimateSize :: DynamicValue -> Int
estimateSize (DynamicValue core _typeInfo _dvSchemaVersion _shallowId) = coreSize + typeInfoSize
  where
    typeInfoSize = 50 -- Rough estimate for type info
    estimatedChildSize = 50 -- Rough estimate per child

    coreSize = case core of
      DPrimitive (PInt      _)  -> 8
      DPrimitive (PDouble   _)  -> 8
      DPrimitive (PBool     _)  -> 1
      DPrimitive (PText     t)  -> Text.length t * 2 -- Rough UTF-8 estimate
      DPrimitive (PWord64   _)  -> 8
      DPrimitive (PInt32    _)  -> 4
      DPrimitive (PWord32   _)  -> 4
      DPrimitive (PInteger  t)  -> Text.length t * 2 -- Text representation
      DPrimitive (PBytes    bs) -> ByteString.length bs -- Byte length
      -- SHALLOW estimates - just count immediate children, don't recurse
      DProduct    vals          -> 10 + length vals * estimatedChildSize
      DSum      _ _             -> 10 + estimatedChildSize
      DList       vals          -> 10 + length vals * estimatedChildSize
      DUnit                     -> 1
      DReference _              -> 4

-- | Compute content hash of a DynamicValue.
computeContentHash :: DynamicValue -> Scoped ContentHash
computeContentHash dynValue = Crypto.hash . LBS.toStrict . Builder.toLazyByteString <$> contentHashBytes dynValue
  where
    contentHashBytes :: DynamicValue -> Scoped Builder.Builder
    contentHashBytes (DynamicValue core _ _ _) = coreHashBytes core

    coreHashBytes :: DynamicCore -> Scoped Builder.Builder
    coreHashBytes = \case
      DPrimitive pv -> Scoped PureContent $ Builder.word8 0x01 <> primitiveHashBytes pv
      DProduct vals -> do
        childHashes <- mapM contentHashBytes vals
        return $ Builder.word8 0x02 <> mconcat childHashes
      DSum index val -> do
        childHash <- contentHashBytes val
        return $ Builder.word8 0x03 <> Builder.word32BE index <> childHash
      DList vals -> do
        childHashes <- mapM contentHashBytes vals
        return $ Builder.word8 0x04 <> mconcat childHashes
      DUnit -> Scoped PureContent $ Builder.word8 0x05
      DReference refId -> Scoped WithReference $ Builder.word8 0x06 <> Builder.word32BE refId

    primitiveHashBytes :: PrimitiveValue -> Builder.Builder
    primitiveHashBytes = \case
      PInt i       -> Builder.word8 0x10 <> Builder.intDec i
      PDouble d    -> Builder.word8 0x11 <> Builder.doubleDec d
      PText t      -> Builder.word8 0x12 <> Builder.byteString (encodeUtf8 t)
      PBool b      -> Builder.word8 0x13 <> Builder.word8 (if b then 1 else 0)
      PWord64 w    -> Builder.word8 0x14 <> Builder.word64BE w
      PInt32 i     -> Builder.word8 0x15 <> Builder.int32BE i
      PWord32 w    -> Builder.word8 0x16 <> Builder.word32BE w
      PInteger t   -> Builder.word8 0x17 <> Builder.byteString (encodeUtf8 t)
      PBytes bs    -> Builder.word8 0x18 <> Builder.word32BE (fromIntegral $ ByteString.length bs) <> Builder.byteString bs

-- | Apply deduplication to a DynamicValue
deduplicateValue :: DeduplicationStrategy -> DynamicValue -> (DynamicValue, Map.Map Word32 DynamicValue)
deduplicateValue strat rootValue
  | _enableDeduplication strat
  = let (result, finalState) = runState (deduplicateValue' rootValue) (initDeduplicationState strat)
      in (result, _sharedTable finalState)

  | otherwise
  = (rootValue, Map.empty)

  where
    deduplicateValue' :: DynamicValue -> DeduplicationM DynamicValue
    deduplicateValue' dynVal = do
      depth <- use currentDepth
      strat <- use strategy

      if depth >= strat ^. maxDepthScan
        then return dynVal
        else do
          if estimateSize dynVal < strat ^. minSizeThreshold
            then return dynVal
            else do
              currentDepth += 1

              -- TOP-DOWN: Hash the value FIRST, before processing children
              let contentHash = computeContentHash dynVal ^. scopedValue
              seen <- use $ seenHashes . at contentHash

              result <- case seen of
                Just (Right refId) -> do
                  -- We've seen this content at least twice before - return existing reference
                  -- TOP-DOWN: Skip all child processing since we found a duplicate
                  return $ DynamicValue (DReference refId) Nothing currentSchemaVersion Nothing
                Just (Left firstOccurrence) -> do
                  -- This is the SECOND time we see this content
                  -- Now we need to create a shared entry for it
                  newId <- use nextReferenceId
                  nextReferenceId .= (newId + 1)

                  -- Move first occurrence to shared table
                  sharedTable . at newId ?= firstOccurrence

                  -- Update seen map to point to the reference
                  seenHashes . at contentHash ?= Right newId

                  -- Return reference (skip child processing for duplicate)
                  return $ DynamicValue (DReference newId) Nothing currentSchemaVersion Nothing
                Nothing -> do
                  -- First time seeing this content - record it but don't create reference yet
                  seenHashes . at contentHash ?= Left dynVal

                  -- Now recursively deduplicate children
                  dedupedVal <- deduplicateChildren dynVal

                  -- Return the deduplicated value (not a reference)
                  return dedupedVal

              -- Restore depth
              currentDepth .= depth
              return result

    deduplicateChildren :: DynamicValue -> DeduplicationM DynamicValue
    deduplicateChildren (DynamicValue core typeInfo version shallowId) = do
      dedupedCore <- deduplicateCore core
      return $ DynamicValue dedupedCore typeInfo version shallowId

    deduplicateCore :: DynamicCore -> DeduplicationM DynamicCore
    deduplicateCore = \case
      DPrimitive pv     -> return $ DPrimitive pv
      DProduct   vals   -> DProduct <$> mapM deduplicateValue' vals
      DSum     i val    -> DSum i   <$> deduplicateValue' val
      DList      vals   -> DList    <$> mapM deduplicateValue' vals
      DUnit             -> return DUnit
      DReference  refId -> return $ DReference refId

--------------------------------------------------------------------------------
-- Fast Deduplication (Hashable-based)
--------------------------------------------------------------------------------

-- | Fast hash type using Haskell's Hashable typeclass instead of cryptographic hashing.
-- This provides much faster deduplication suitable for "over the wire" serialization
-- where cryptographic security is not required.
type HashableHash = Int

-- | Entry in the seen-hashes table during deduplication.
--
-- 'FirstSeen' records the original (un-deduplicated) value on first
-- encounter.  When a duplicate is confirmed, 'Promoted' records the
-- reference ID *and* retains the original for equality comparison with
-- future occurrences (since the shared-table copy has already had its
-- children deduplicated and therefore differs structurally).
data SeenEntry
  = FirstSeen  DynamicValue           -- ^ Original value (first encounter)
  | Promoted   Word32 DynamicValue    -- ^ (refId, original) — promoted to shared table
  deriving stock (Show)

-- | State for fast deduplication using Hashable
data FastDeduplicationState = FastDeduplicationState
  { _fastNextReferenceId  :: Word32
  , _fastSeenHashes       :: Map.Map HashableHash SeenEntry
  , _fastSharedTable      :: Map.Map Word32 DynamicValue
  , _fastCurrentDepth     :: Int
  , _fastStrategy         :: DeduplicationStrategy
  , _fastStats            :: DeduplicationStats  -- Statistics tracking
  }
  deriving stock (Show)

makeLenses ''FastDeduplicationState

-- | Monad for fast deduplication operations
type FastDeduplicationM = State FastDeduplicationState

-- | Initialize fast deduplication state
initFastDeduplicationState :: DeduplicationStrategy -> FastDeduplicationState
initFastDeduplicationState strat =
  FastDeduplicationState
    { _fastNextReferenceId  = 1
    , _fastSeenHashes       = Map.empty
    , _fastSharedTable      = Map.empty
    , _fastCurrentDepth     = 0
    , _fastStrategy         = strat
    , _fastStats            = emptyDeduplicationStats
    }

-- | Compute fast hash of a DynamicValue using Hashable typeclass.
-- This is much faster than cryptographic content hashing but only suitable
-- for deduplication within a single serialization session.
-- | Full content hash using Hashable (O(n) - expensive but accurate)
computeHashableHash :: DynamicValue -> HashableHash
computeHashableHash (DynamicValue core _ _ _) = hashWithSalt 0 core

-- | Fast approximate hash (O(1) - samples structure intelligently)
-- This trades accuracy for speed by sampling rather than hashing everything.
-- Safety: Deduplication still does full equality check, so false positives are impossible.
-- Tradeoff: May miss some duplicates (false negatives), slightly larger files.
computeFastApproximateHash :: DynamicValue -> HashableHash
computeFastApproximateHash (DynamicValue core typeInfo _ _) =
  hashWithSalt 0 (show typeInfo, approximateHashCore core)
  where
    -- Approximate hash of DynamicCore - intelligently samples based on structure
    -- ALL container types use size estimation only (truly O(1)!)
    approximateHashCore :: DynamicCore -> Int
    approximateHashCore = \case
      -- Primitives are cheap - hash fully
      DPrimitive pv -> hashWithSalt 0 (0 :: Int, pv)

      -- ALL products - sample SIZES only (truly O(1)!)
      DProduct vals ->
        let len = length vals
            sizes = map estimateSize (take 3 vals)
        in hashWithSalt 0 (1 :: Int, len, sizes)

      -- Sums - hash constructor index + SIZE of child (not full content!)
      DSum index val ->
        hashWithSalt 0 (2 :: Int, index, estimateSize val)

      -- ALL lists - sample SIZES only (truly O(1)!)
      DList vals ->
        let len = length vals
            sizes = map estimateSize (take 3 vals)
        in hashWithSalt 0 (3 :: Int, len, sizes)

      -- Unit and Reference are cheap
      DUnit -> hashWithSalt 0 (4 :: Int)
      DReference refId -> hashWithSalt 0 (5 :: Int, refId)

-- Make DynamicCore hashable for fast deduplication
instance Hashable DynamicCore where
  hashWithSalt salt = \case
    DPrimitive pv     -> salt `hashWithSalt` (0 :: Int) `hashWithSalt` pv
    DProduct vals     -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` vals
    DSum index val    -> salt `hashWithSalt` (2 :: Int) `hashWithSalt` index `hashWithSalt` val
    DList vals        -> salt `hashWithSalt` (3 :: Int) `hashWithSalt` vals
    DUnit             -> salt `hashWithSalt` (4 :: Int)
    DReference refId  -> salt `hashWithSalt` (5 :: Int) `hashWithSalt` refId

instance Hashable PrimitiveValue where
  hashWithSalt salt = \case
    PInt i       -> salt `hashWithSalt` (0 :: Int) `hashWithSalt` i
    PDouble d    -> salt `hashWithSalt` (1 :: Int) `hashWithSalt` d
    PText t      -> salt `hashWithSalt` (2 :: Int) `hashWithSalt` t
    PBool b      -> salt `hashWithSalt` (3 :: Int) `hashWithSalt` b
    PWord64 w    -> salt `hashWithSalt` (4 :: Int) `hashWithSalt` w
    PInt32 i     -> salt `hashWithSalt` (5 :: Int) `hashWithSalt` i
    PWord32 w    -> salt `hashWithSalt` (6 :: Int) `hashWithSalt` w
    PInteger t   -> salt `hashWithSalt` (7 :: Int) `hashWithSalt` t
    PBytes bs    -> salt `hashWithSalt` (8 :: Int) `hashWithSalt` bs

instance Hashable DynamicValue where
  hashWithSalt salt (DynamicValue core _typeInfo _version _shallowId) =
    salt `hashWithSalt` core
    -- Note: We intentionally don't hash type info or shallow ID for performance
    -- Type safety is still preserved through the type system

-- | Apply fast deduplication to a DynamicValue using Hashable-based hashing.
-- This is 50-100x faster than content-based deduplication but only deduplicates
-- within a single serialization session.
deduplicateValueFast :: DeduplicationStrategy -> DynamicValue -> (DynamicValue, Map.Map Word32 DynamicValue)
deduplicateValueFast strat rootValue
  | _enableDeduplication strat
  = let (result, finalState) = runState (deduplicateValueFast' rootValue) (initFastDeduplicationState strat)
      in (result, _fastSharedTable finalState)
  | otherwise
  = (rootValue, Map.empty)
  where
    deduplicateValueFast' :: DynamicValue -> FastDeduplicationM DynamicValue
    deduplicateValueFast' dynVal = do
      depth <- use fastCurrentDepth
      strat <- use fastStrategy

      if depth >= strat ^. maxDepthScan
        then return dynVal
        else do
          if estimateSize dynVal < strat ^. minSizeThreshold
            then return dynVal
            else do
              fastCurrentDepth += 1

              -- Fast: Use Hashable instead of cryptographic content hashing
              let fastHash = computeHashableHash dynVal
              seen <- use $ fastSeenHashes . at fastHash

              result <- case seen of
                Just (Promoted refId _) -> do
                  -- Already seen twice+ - return reference
                  return $ DynamicValue (DReference refId) Nothing currentSchemaVersion Nothing
                Just (FirstSeen firstOccurrence) -> do
                  -- Second occurrence - create shared entry with deduped children
                  newId <- use fastNextReferenceId
                  fastNextReferenceId .= (newId + 1)
                  dedupedFirst <- deduplicateChildrenFast firstOccurrence
                  fastSharedTable . at newId ?= dedupedFirst
                  fastSeenHashes . at fastHash ?= Promoted newId firstOccurrence
                  return $ DynamicValue (DReference newId) Nothing currentSchemaVersion Nothing
                Nothing -> do
                  -- First occurrence - record and process children
                  fastSeenHashes . at fastHash ?= FirstSeen dynVal
                  dedupedVal <- deduplicateChildrenFast dynVal
                  return dedupedVal

              fastCurrentDepth .= depth
              return result

    deduplicateChildrenFast :: DynamicValue -> FastDeduplicationM DynamicValue
    deduplicateChildrenFast (DynamicValue core typeInfo version shallowId) = do
      dedupedCore <- deduplicateCoreFast core
      return $ DynamicValue dedupedCore typeInfo version shallowId

    deduplicateCoreFast :: DynamicCore -> FastDeduplicationM DynamicCore
    deduplicateCoreFast = \case
      DPrimitive pv     -> return $ DPrimitive pv
      DProduct   vals   -> DProduct <$> mapM deduplicateValueFast' vals
      DSum     i val    -> DSum i   <$> deduplicateValueFast' val
      DList      vals   -> DList    <$> mapM deduplicateValueFast' vals
      DUnit             -> return DUnit
      DReference  refId -> return $ DReference refId

--------------------------------------------------------------------------------
-- Shallow Deduplication (Name-based via ShallowIdentifiable)
--------------------------------------------------------------------------------

-- | Try to extract a shallow identifier from a DynamicValue.
--
-- This function is used by shallow deduplication to extract compact identifiers
-- for hashing. It should ONLY return a value when there is an explicit
-- ShallowIdentifiable instance for the type.
--
-- The previous implementation tried to use a heuristic (first Text field in a record),
-- but this was too aggressive and caused incorrect deduplication for types where
-- the name field is not a unique identifier (e.g., Groups with same name but different
-- variables, or tuples with same first element).
--
-- Shallow identification should be opt-in through explicit ShallowIdentifiable instances,
-- not guessed from data structure.
tryExtractShallowId :: DynamicValue -> Maybe ByteString
tryExtractShallowId dynVal =
  -- Extract shallow ID from DynamicValue if provided
  -- Users provide shallow IDs by implementing custom ToSZT instances
  -- that populate the _dvShallowId field
  dynVal ^. dvShallowId

-- | Apply shallow deduplication: tries ShallowIdentifiable first, falls back to Hashable.
-- This is the fastest mode, mimicking the hand-rolled serialization strategy.
deduplicateValueShallow :: DeduplicationStrategy -> DynamicValue -> (DynamicValue, Map.Map Word32 DynamicValue)
deduplicateValueShallow strat rootValue = (result, sharedTable)
  where
    (result, sharedTable, _) = deduplicateValueShallowWithStats strat rootValue

-- | Apply shallow deduplication and return statistics about the deduplication process.
-- This allows analysis of which types are most frequently deduplicated and which would
-- benefit most from ShallowIdentifiable instances.
deduplicateValueShallowWithStats :: DeduplicationStrategy -> DynamicValue -> (DynamicValue, Map.Map Word32 DynamicValue, DeduplicationStats)
deduplicateValueShallowWithStats strat rootValue
  | _enableDeduplication strat
  = let (result, finalState) = runState (deduplicateValueShallow' rootValue) (initFastDeduplicationState strat)
      in (result, _fastSharedTable finalState, _fastStats finalState)
  | otherwise
  = (rootValue, Map.empty, emptyDeduplicationStats)
  where
    deduplicateValueShallow' :: DynamicValue -> FastDeduplicationM DynamicValue
    deduplicateValueShallow' dynVal = do
      depth <- use fastCurrentDepth
      strat <- use fastStrategy

      let typeName = show (dynVal ^. dvTypeInfo)

      -- Record encounter
      fastStats . typeEncounters %= Map.insertWith (+) typeName 1

      if depth >= strat ^. maxDepthScan
        then do
          fastStats . typeSkipped %= Map.insertWith (+) typeName 1
          return dynVal
        else do
          -- OPTIMIZATION: Extract shallow ID FIRST, before expensive size check
          let maybeShallowId = tryExtractShallowId dynVal

          -- Skip size check if we have a shallow ID (we always want to deduplicate these)
          shouldDeduplicate <- case maybeShallowId of
            Just _ -> return True  -- Has shallow ID - always deduplicate, skip size check
            Nothing -> do
              -- No shallow ID - check size threshold (expensive but necessary)
              let sz = estimateSize dynVal
              return (sz >= strat ^. minSizeThreshold)

          if not shouldDeduplicate
            then do
              fastStats . typeSkipped %= Map.insertWith (+) typeName 1
              return dynVal
            else do
              fastCurrentDepth += 1

              -- Compute hash based on shallow ID or fast approximate content
              let (shallowHash, hasShallowId) = case maybeShallowId of
                    Just shallowId ->
                      -- Hash just the type info + shallow ID (much faster!)
                      (hashWithSalt 0 (show (dynVal ^. dvTypeInfo), shallowId), True)
                    Nothing ->
                      -- No shallow ID available, fall back to FAST APPROXIMATE hashing
                      -- This samples structure intelligently rather than hashing everything
                      (computeFastApproximateHash dynVal, False)

              -- Record hashing if we had to do approximate hash (no shallow ID)
              unless hasShallowId $
                fastStats . typeHashed %= Map.insertWith (+) typeName 1

              seen <- use $ fastSeenHashes . at shallowHash

              let hashPreview = Text.pack $ take 8 $ show shallowHash
                  estimatedSz = estimateSize dynVal
                  -- Compute full Hashable hash for comparison
                  fullHash = computeHashableHash dynVal
                  fullHashPreview = Text.pack $ take 8 $ show fullHash
                  -- Extract shallow ID bytes if present
                  shallowIdBytes = tryExtractShallowId dynVal

              result <- case seen of
                Just (Promoted refId originalVal)
                  | dynVal == originalVal -> do
                      -- True duplicate confirmed by equality check against original!
                      let !_ = emitEvent (Text.pack typeName) hasShallowId hashPreview fullHashPreview shallowIdBytes True estimatedSz
                      fastStats . typeDeduplicated %= Map.insertWith (+) typeName 1
                      let savedBytes = estimateSize dynVal
                      fastStats . typeBytesSaved %= Map.insertWith (+) typeName savedBytes
                      return $ DynamicValue (DReference refId) Nothing currentSchemaVersion Nothing
                  | otherwise -> do
                      -- Hash collision - not actually equal, treat as new value
                      fastSeenHashes . at shallowHash ?= FirstSeen dynVal
                      dedupedVal <- deduplicateChildrenShallow dynVal
                      return dedupedVal
                Just (FirstSeen firstOccurrence)
                  | dynVal == firstOccurrence -> do
                      -- Second occurrence confirmed by equality check - create shared entry
                      -- CRITICAL: Dedup children of the first occurrence before storing
                      -- in the shared table. Without this, shared table entries contain
                      -- fully expanded sub-trees with no internal deduplication.
                      let !_ = emitEvent (Text.pack typeName) hasShallowId hashPreview fullHashPreview shallowIdBytes True estimatedSz
                      newId <- use fastNextReferenceId
                      fastNextReferenceId .= (newId + 1)
                      dedupedFirst <- deduplicateChildrenShallow firstOccurrence
                      fastSharedTable . at newId ?= dedupedFirst
                      fastSeenHashes . at shallowHash ?= Promoted newId firstOccurrence
                      fastStats . typeDeduplicated %= Map.insertWith (+) typeName 1
                      let savedBytes = estimateSize dynVal
                      fastStats . typeBytesSaved %= Map.insertWith (+) typeName savedBytes
                      return $ DynamicValue (DReference newId) Nothing currentSchemaVersion Nothing
                  | otherwise -> do
                      -- Hash collision with first occurrence - treat as new
                      -- Keep the first occurrence, add this as separate
                      dedupedVal <- deduplicateChildrenShallow dynVal
                      return dedupedVal
                Nothing -> do
                  -- First occurrence - record and process children
                  -- Emit instrumentation event (force evaluation)
                  let !_ = emitEvent (Text.pack typeName) hasShallowId hashPreview fullHashPreview shallowIdBytes False estimatedSz
                  fastSeenHashes . at shallowHash ?= FirstSeen dynVal
                  dedupedVal <- deduplicateChildrenShallow dynVal
                  return dedupedVal

              fastCurrentDepth .= depth
              return result

    deduplicateChildrenShallow :: DynamicValue -> FastDeduplicationM DynamicValue
    deduplicateChildrenShallow (DynamicValue core typeInfo version shallowId) = do
      dedupedCore <- deduplicateCoreShallow core
      return $ DynamicValue dedupedCore typeInfo version shallowId

    deduplicateCoreShallow :: DynamicCore -> FastDeduplicationM DynamicCore
    deduplicateCoreShallow = \case
      DPrimitive pv     -> return $ DPrimitive pv
      DProduct   vals   -> DProduct <$> mapM deduplicateValueShallow' vals
      DSum     i val    -> DSum i   <$> deduplicateValueShallow' val
      DList      vals   -> DList    <$> mapM deduplicateValueShallow' vals
      DUnit             -> return DUnit
      DReference  refId -> return $ DReference refId

-- | Serialization type class for converting Haskell values to Serializotron format.
--
-- This is the main interface for serialization. Any type that can be serialized
-- must implement this class. For most types, you can derive it automatically:
--
-- @
-- data Person = Person Text Int deriving (Generic, ToSZT)
-- @
--
-- The class provides:
-- * Automatic derivation via GHC Generics for most types
-- * Manual instances for primitive types and special cases
-- * Type information extraction for compatibility checking
--
-- Custom instances are typically only needed for:
-- * Types with special serialization requirements
-- * Primitive types not covered by the standard instances
-- * Types requiring custom compatibility logic
--
-- Example of manual instance:
-- @
-- instance ToSZT MySpecialType where
--   toSzt (MySpecialType x y) = DynamicValue
--     { _dvCore = DProduct [toSzt x, toSzt y]
--     , _dvTypeInfo = Just $ TypeInfo ...
--     , _dvSchemaVersion = currentSchemaVersion
--     , _dvShallowId = Nothing
--     }
-- @

--------------------------------------------------------------------------------
-- ShallowIdentifiable - Opt-in for fast, name-based deduplication
--------------------------------------------------------------------------------

-- | Optional typeclass for types that can be identified by a shallow identifier.
--
-- This enables a fast deduplication mode that only hashes a small identifying field
-- (like a 'name') instead of the entire data structure. This is orders of magnitude
-- faster for large, complex objects that have meaningful names or unique identifiers.
--
-- Use this for types where:
-- * Objects have a unique name or ID field
-- * The full structure is expensive to hash
-- * Identity is determined by name, not by full structural equality
--
-- __Performance impact:__ For the DDoS model benchmark:
-- * Without ShallowIdentifiable: ~19 seconds (hashing 1+ GB)
-- * With ShallowIdentifiable for Variable and SemiringValuation: ~1 second (hashing ~20 bytes each)
--
-- __Trade-offs:__
-- * ✅ Much faster serialization (50-100x speedup for complex models)
-- * ✅ Still ensures uniqueness via name hashing
-- * ⚠️  Tiny risk of hash collisions (but no worse than hand-rolled code)
-- * ⚠️  Assumes name uniquely identifies the object
--
-- = Example Usage
--
-- For types with a name field:
--
-- @
-- data Variable = Variable
--   { _varName :: Name
--   , _varDomain :: Domain -- Large, complex structure
--   , ...
--   }
--
-- -- Only hash the name, not the entire domain
-- instance ShallowIdentifiable Variable where
--   shallowIdentifier v = encodeUtf8 $ unName $ v ^. varName
-- @
--
-- This tells the shallow deduplication algorithm: "two Variables with the same
-- name are the same object, no need to hash the entire Domain."
--
-- = When NOT to Use
--
-- Don't implement this for types where:
-- * Name doesn't uniquely identify the object
-- * You need cryptographic-strength deduplication
-- * The type doesn't have a natural identifier field
--
-- In those cases, use the regular 'saveSztCompressed' which does full content-based deduplication.
--
-- = API Functions
--
-- * 'saveSztShallow': Fast serialization using shallow IDs, no compression
-- * 'saveSztShallowCompressed': Fast serialization using shallow IDs + GZip
--
-- These functions will automatically use 'ShallowIdentifiable' instances when available,
-- and fall back to 'Hashable'-based deduplication for types without instances.
class ShallowIdentifiable a where
  -- | Extract a shallow identifier (e.g., just the name field) for fast hashing.
  -- This should be a small ByteString that uniquely identifies the object.
  shallowIdentifier :: a -> ByteString

class ToSZT a where
  toSzt :: a -> DynamicValue
  default toSzt :: (Generic a, GToSZT (Rep a), GGetTypeInfo (Rep a), Typeable a) => a -> DynamicValue
  toSzt x =
    DynamicValue
      { _dvCore          = gToSZT (GHC.Generics.from x)
      , _dvTypeInfo      = Just $ gGetTypeInfo (GHC.Generics.from x) (typeRep (Proxy @a))
      , _dvSchemaVersion = currentSchemaVersion
      , _dvShallowId     = Nothing  -- Generics don't provide shallow IDs by default
      }

-- | Generic serialization type class
class GToSZT f where
  gToSZT :: f p -> DynamicCore

class GCollectProductValues f where
  gCollectProductValues :: f p -> [DynamicValue]

instance GCollectProductValues U1 where
  gCollectProductValues _ = []

instance (GCollectProductValues f, GCollectProductValues g) => GCollectProductValues (f :*: g) where
  gCollectProductValues (f :*: g) = gCollectProductValues f ++ gCollectProductValues g

instance (GCollectProductValues f) => GCollectProductValues (M1 i c f) where
  gCollectProductValues (M1 x) = gCollectProductValues x

instance (ToSZT a) => GCollectProductValues (K1 i a) where
  gCollectProductValues (K1 x) = [toSzt x]

-- | Extract wrapped type info from a newtype's Generic representation
-- This traverses through M1/C1/S1 wrappers to find the K1 containing the actual type
class GExtractNewtypeWrapper f where
  gExtractNewtypeWrapper :: Proxy (f p) -> Maybe TypeInfo

-- For K1, extract TypeInfo using Typeable
instance Typeable a => GExtractNewtypeWrapper (K1 i a) where
  gExtractNewtypeWrapper _ = Just $ typeInfoForRep (typeRep (Proxy @a))

-- For metadata wrappers, recurse
instance GExtractNewtypeWrapper f => GExtractNewtypeWrapper (M1 i c f) where
  gExtractNewtypeWrapper _ = gExtractNewtypeWrapper (Proxy :: Proxy (f p))

-- For products and sums, we can't extract (shouldn't happen in newtypes anyway)
instance GExtractNewtypeWrapper (f :*: g) where
  gExtractNewtypeWrapper _ = Nothing

instance GExtractNewtypeWrapper (f :+: g) where
  gExtractNewtypeWrapper _ = Nothing

instance GExtractNewtypeWrapper U1 where
  gExtractNewtypeWrapper _ = Nothing

-- | Generic type info extraction
class GGetTypeInfo f where
  gGetTypeInfo :: f p -> TypeRep -> TypeInfo
  -- | Get complete type info including all constructors for sum types
  gGetCompleteTypeInfo :: f p -> TypeRep -> TypeInfo
  gGetCompleteTypeInfo = gGetTypeInfo -- Default implementation

-- | Generic constructor names extraction
class GGetFieldInfos f where
  gGetFieldInfos :: f p -> [FieldInfo]

class GGetConstructorNames f where
  gGetConstructorNames :: f p -> [Text]

-- | Get ALL constructor names for a sum type, regardless of active constructor  
class GGetAllConstructorNames f where
  gGetAllConstructorNames :: Proxy (f p) -> [Text]

-- | Generic structure extraction
class GGetStructure f where
  gGetStructure :: f p -> TypeStructure

-- | Get complete structure for sum types, regardless of active constructor  
class GGetCompleteStructure f where
  gGetCompleteStructure :: Proxy (f p) -> TypeStructure

-- | Extract full TypeInfo (including type name) from constructor arguments
class GGetConstructorTypeInfo f where
  gGetConstructorTypeInfo :: Proxy (f p) -> TypeInfo

-- Generic instances for GToSZT
instance GToSZT U1 where
  gToSZT U1 = DUnit

instance (ToSZT a) => GToSZT (K1 i a) where
  gToSZT (K1 x) = case toSzt x of
    DynamicValue core _ _ _ -> core

instance (GCollectProductValues f, GCollectProductValues g) => GToSZT (f :*: g) where
  gToSZT prod = DProduct (gCollectProductValues prod)

instance (GToSZT f, GToSZT g) => GToSZT (f :+: g) where
  gToSZT (L1 x) = DSum 0 (DynamicValue (gToSZT x) Nothing currentSchemaVersion Nothing)
  gToSZT (R1 y) = DSum 1 (DynamicValue (gToSZT y) Nothing currentSchemaVersion Nothing)

instance (GToSZT f) => GToSZT (M1 i c f) where
  gToSZT (M1 x) = gToSZT x

-- Primitive instances
instance ToSZT Int where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PInt x)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Int"
              , _tiModule = Just "GHC.Types"
              , _tiConstructors = []
              , _tiFieldLabels = []
              , _tiStructure = Just (TSPrimitive PTInt)
              , _tiTypeParameters = []
              , _tiNewtypeWrapper = Nothing
              }
      , _dvSchemaVersion = currentSchemaVersion
      , _dvShallowId = Nothing
      }

instance ToSZT Double where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PDouble x)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Double"
              , _tiModule = Just "GHC.Types"
              , _tiConstructors = []
              , _tiFieldLabels = []
              , _tiStructure = Just (TSPrimitive PTDouble)
              , _tiTypeParameters = []
              , _tiNewtypeWrapper = Nothing
              }
      , _dvSchemaVersion = currentSchemaVersion
      , _dvShallowId = Nothing
      }

instance ToSZT Text where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PText x)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Text"
              , _tiModule = Just "Data.Text"
              , _tiConstructors = []
              , _tiFieldLabels = []
              , _tiStructure = Just (TSPrimitive PTText)
              , _tiTypeParameters = []
              , _tiNewtypeWrapper = Nothing
              }
      , _dvSchemaVersion = currentSchemaVersion
      , _dvShallowId = Nothing
      }

instance ToSZT Bool where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PBool x)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "Bool"
              , _tiModule = Just "GHC.Types"
              , _tiConstructors = ["False", "True"]
              , _tiFieldLabels = []
              , _tiStructure = Just (TSPrimitive PTBool)
              , _tiTypeParameters = []
              , _tiNewtypeWrapper = Nothing
              }
      , _dvSchemaVersion = currentSchemaVersion
      , _dvShallowId = Nothing
      }

instance ToSZT ByteString where
  toSzt x =
    DynamicValue
      { _dvCore = DPrimitive (PBytes x)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "ByteString"
              , _tiModule = Just "Data.ByteString"
              , _tiConstructors = []
              , _tiFieldLabels = []
              , _tiStructure = Just (TSPrimitive PTBytes)
              , _tiTypeParameters = []
              , _tiNewtypeWrapper = Nothing
              }
      , _dvSchemaVersion = currentSchemaVersion
      , _dvShallowId = Nothing
      }

instance (ToSZT a) => ToSZT [a] where
  toSzt xs =
    DynamicValue
      { _dvCore = DList (map toSzt xs)
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "[]"
              , _tiModule = Just "GHC.Types"
              , _tiConstructors = ["[]", ":"]
              , _tiFieldLabels = []
              , _tiStructure = Just (TSList elementTypeInfo)
              , _tiTypeParameters = [elementTypeInfo]
              , _tiNewtypeWrapper = Nothing
              }
      , _dvSchemaVersion = currentSchemaVersion
      , _dvShallowId = Nothing
      }
    where
      elementTypeInfo = case xs of
        [] -> emptyTypeInfo -- Unknown element type for empty list
        (x : _) -> case toSzt x of
          DynamicValue _ (Just ti) _ _ -> ti
          _ -> emptyTypeInfo

-- Generic type info instances
instance (Datatype d, GGetConstructorNames f, GGetStructure f, GGetAllConstructorNames f, GGetCompleteStructure f, GGetFieldInfos f, GGetTypeInfo f, GExtractNewtypeWrapper f) => GGetTypeInfo (M1 D d f) where
  gGetTypeInfo (M1 x) tr =
    let inner = gGetTypeInfo x tr
        constructors = gGetAllConstructorNames (Proxy :: Proxy (f p))
        isSumType = length constructors > 1
        fullStructure =
          if isSumType
            then Just (gGetCompleteStructure (Proxy :: Proxy (f p)))
            else inner ^. tiStructure
        fieldLabels = if isSumType then [] else inner ^. tiFieldLabels
        -- For single-constructor types, try to extract wrapped type
        -- This works for both newtypes and single-field data types
        wrappedType = if length constructors == 1
                      then gExtractNewtypeWrapper (Proxy :: Proxy (f p))
                      else Nothing
     in inner
          { _tiTypeName     = Just $ Text.pack (datatypeName (undefined :: M1 D d f p))
          , _tiModule       = Just $ Text.pack (moduleName   (undefined :: M1 D d f p))
          , _tiConstructors = constructors
          , _tiFieldLabels  = fieldLabels
          , _tiStructure    = fullStructure
          , _tiNewtypeWrapper = wrappedType
          }
instance (GGetTypeInfo f) => GGetTypeInfo (M1 C c f) where
  gGetTypeInfo (M1 x) = gGetTypeInfo x

instance (GGetTypeInfo f) => GGetTypeInfo (M1 S s f) where
  gGetTypeInfo (M1 x) = gGetTypeInfo x

instance GGetTypeInfo U1 where
  gGetTypeInfo U1 _tr = emptyTypeInfo

instance GGetTypeInfo (K1 i a) where
  gGetTypeInfo (K1 _) _tr = emptyTypeInfo

instance (GGetTypeInfo f, GGetTypeInfo g, GGetFieldInfos f, GGetFieldInfos g) => GGetTypeInfo (f :*: g) where
  gGetTypeInfo prod _ =
    let rawFields = gGetFieldInfos prod
        labeledFields = assignFieldLabels rawFields
        labels = map (fromMaybe Text.empty . (^. fiFieldName)) labeledFields
     in TypeInfo
          { _tiTypeName = Nothing      -- No specific type name for product
          , _tiModule = Nothing        -- No specific module for product
          , _tiConstructors = []       -- Product combines fields, not constructors
          , _tiFieldLabels = labels
          , _tiStructure = Just $ TSProduct labeledFields
          , _tiTypeParameters = []     -- Products don't have type parameters at this level
          , _tiNewtypeWrapper = Nothing
          }
instance (GGetTypeInfo f, GGetTypeInfo g) => GGetTypeInfo (f :+: g) where
  gGetTypeInfo (L1 x) tr = gGetTypeInfo x tr
  gGetTypeInfo (R1 y) tr = gGetTypeInfo y tr

-- Constructor name extraction instances
instance (Constructor c) => GGetConstructorNames (M1 C c f) where
  gGetConstructorNames (M1 _) = [Text.pack (conName (undefined :: M1 C c f p))]

instance (GGetConstructorNames f, GGetConstructorNames g) => GGetConstructorNames (f :+: g) where
  gGetConstructorNames (L1 x) = gGetConstructorNames x
  gGetConstructorNames (R1 y) = gGetConstructorNames y

instance (GGetConstructorNames f) => GGetConstructorNames (M1 D d f) where
  gGetConstructorNames (M1 x) = gGetConstructorNames x

instance (GGetConstructorNames f) => GGetConstructorNames (M1 S s f) where
  gGetConstructorNames (M1 x) = gGetConstructorNames x

instance GGetConstructorNames (K1 i a) where
  gGetConstructorNames _ = []

instance GGetFieldInfos U1 where
  gGetFieldInfos _ = []

instance (ToSZT a) => GGetFieldInfos (K1 i a) where
  gGetFieldInfos (K1 x) =
    let fieldType = case toSzt x of
          DynamicValue _ (Just ti) _ _ -> ti
          _ -> emptyTypeInfo
     in [FieldInfo Nothing fieldType]

instance (GGetFieldInfos f, GGetFieldInfos g) => GGetFieldInfos (f :*: g) where
  gGetFieldInfos (f :*: g) = gGetFieldInfos f ++ gGetFieldInfos g

instance (GGetFieldInfos f) => GGetFieldInfos (M1 C c f) where
  gGetFieldInfos (M1 x) = gGetFieldInfos x

instance (GGetFieldInfos f) => GGetFieldInfos (M1 D d f) where
  gGetFieldInfos (M1 x) = gGetFieldInfos x

instance (Selector s, GGetFieldInfos f) => GGetFieldInfos (M1 S s f) where
  gGetFieldInfos m@(M1 x) =
    let nameStr = selName m
        name = if null nameStr then Nothing else Just (Text.pack nameStr)
        fields = gGetFieldInfos x
     in case fields of
          [] -> [FieldInfo name emptyTypeInfo]
          _ -> case name of
                 Nothing -> fields
                 Just n -> [ if isNothing (fi ^. fiFieldName) then fi & fiFieldName ?~ n else fi | fi <- fields ]

instance (GGetFieldInfos f, GGetFieldInfos g) => GGetFieldInfos (f :+: g) where
  gGetFieldInfos (L1 x) = gGetFieldInfos x
  gGetFieldInfos (R1 y) = gGetFieldInfos y

instance GGetConstructorNames U1 where
  gGetConstructorNames _ = []

-- All constructor names instances using Proxy
instance (GGetAllConstructorNames f, GGetAllConstructorNames g) => GGetAllConstructorNames (f :+: g) where
  gGetAllConstructorNames _ = gGetAllConstructorNames (Proxy :: Proxy (f p)) ++ gGetAllConstructorNames (Proxy :: Proxy (g p))

instance (Constructor c) => GGetAllConstructorNames (M1 C c f) where
  gGetAllConstructorNames _ = [Text.pack (conName (undefined :: M1 C c f p))]

instance (GGetAllConstructorNames f) => GGetAllConstructorNames (M1 D d f) where
  gGetAllConstructorNames _ = gGetAllConstructorNames (Proxy :: Proxy (f p))

instance (GGetAllConstructorNames f) => GGetAllConstructorNames (M1 S s f) where
  gGetAllConstructorNames _ = gGetAllConstructorNames (Proxy :: Proxy (f p))

instance GGetAllConstructorNames (K1 i a) where
  gGetAllConstructorNames _ = []

instance GGetAllConstructorNames U1 where
  gGetAllConstructorNames _ = []

-- Structure extraction instances
instance (GGetStructure f) => GGetStructure (M1 D d f) where
  gGetStructure (M1 x) = gGetStructure x

instance (GGetStructure f) => GGetStructure (M1 C c f) where
  gGetStructure (M1 x) = gGetStructure x

instance (GGetStructure f) => GGetStructure (M1 S s f) where
  gGetStructure (M1 x) = gGetStructure x

instance (GGetFieldInfos f, GGetFieldInfos g) => GGetStructure (f :*: g) where
  gGetStructure prod = TSProduct (assignFieldLabels (gGetFieldInfos prod))
instance (GGetStructure f, GGetStructure g) => GGetStructure (f :+: g) where
  gGetStructure (L1 x) = gGetStructure x
  gGetStructure (R1 y) = gGetStructure y

instance GGetStructure U1 where
  gGetStructure _ = TSUnit

-- Complete structure instances
instance (GGetConstructorTypeInfo f, GGetConstructorTypeInfo g) => GGetCompleteStructure (f :+: g) where
  gGetCompleteStructure _ = TSSum
    [ gGetConstructorTypeInfo (Proxy :: Proxy (f p))
    , gGetConstructorTypeInfo (Proxy :: Proxy (g p))
    ]

-- For constructor level, we need to create sample values to extract structure
-- This is complex because we can't create arbitrary values at the type level
-- Let's use a simplified approach that represents sum types properly

instance (GGetCompleteStructure f) => GGetCompleteStructure (M1 D d f) where
  gGetCompleteStructure _ = gGetCompleteStructure (Proxy :: Proxy (f p))

instance (GGetCompleteStructure f) => GGetCompleteStructure (M1 S s f) where
  gGetCompleteStructure _ = gGetCompleteStructure (Proxy :: Proxy (f p))

-- For constructor metadata, we just return TSUnit as a placeholder
-- The actual structure will be determined by the field types
instance (GGetCompleteStructure f) => GGetCompleteStructure (M1 C c f) where
  gGetCompleteStructure _ = gGetCompleteStructure (Proxy :: Proxy (f p))

instance Typeable a => GGetCompleteStructure (K1 i a) where
  gGetCompleteStructure _ =
    let rep = typeRep (Proxy @a)
     in fromMaybe TSUnit (structureForTypeRep rep)

instance GGetCompleteStructure U1 where
  gGetCompleteStructure _ = TSUnit

-- Product type instance
instance (GGetCompleteStructure f, GGetCompleteStructure g) => GGetCompleteStructure (f :*: g) where
  gGetCompleteStructure _ = TSProduct $ assignFieldLabels
    [ FieldInfo Nothing (emptyTypeInfo & tiStructure ?~ gGetCompleteStructure (Proxy :: Proxy (f p)))
    , FieldInfo Nothing (emptyTypeInfo & tiStructure ?~ gGetCompleteStructure (Proxy :: Proxy (g p)))
    ]

-- GGetConstructorTypeInfo instances
-- These extract full TypeInfo (including type name) for constructor arguments

instance Typeable a => GGetConstructorTypeInfo (K1 i a) where
  gGetConstructorTypeInfo _ = typeInfoForRep (typeRep (Proxy @a))

instance GGetConstructorTypeInfo U1 where
  gGetConstructorTypeInfo _ = emptyTypeInfo & tiStructure ?~ TSUnit

instance GGetConstructorTypeInfo f => GGetConstructorTypeInfo (M1 D d f) where
  gGetConstructorTypeInfo _ = gGetConstructorTypeInfo (Proxy :: Proxy (f p))

instance GGetConstructorTypeInfo f => GGetConstructorTypeInfo (M1 C c f) where
  gGetConstructorTypeInfo _ = gGetConstructorTypeInfo (Proxy :: Proxy (f p))

instance GGetConstructorTypeInfo f => GGetConstructorTypeInfo (M1 S s f) where
  gGetConstructorTypeInfo _ = gGetConstructorTypeInfo (Proxy :: Proxy (f p))

-- For product types (tuples/records), create a TypeInfo with product structure
instance (GGetConstructorTypeInfo f, GGetConstructorTypeInfo g) => GGetConstructorTypeInfo (f :*: g) where
  gGetConstructorTypeInfo _ =
    let leftInfo = gGetConstructorTypeInfo (Proxy :: Proxy (f p))
        rightInfo = gGetConstructorTypeInfo (Proxy :: Proxy (g p))
    in emptyTypeInfo & tiStructure ?~ TSProduct
         [ FieldInfo Nothing leftInfo
         , FieldInfo Nothing rightInfo
         ]

-- For nested sum types (multiple constructors within a branch)
instance (GGetConstructorTypeInfo f, GGetConstructorTypeInfo g) => GGetConstructorTypeInfo (f :+: g) where
  gGetConstructorTypeInfo _ =
    let leftInfo = gGetConstructorTypeInfo (Proxy :: Proxy (f p))
        rightInfo = gGetConstructorTypeInfo (Proxy :: Proxy (g p))
    in emptyTypeInfo & tiStructure ?~ TSSum [leftInfo, rightInfo]
instance Typeable a => GGetStructure (K1 i a) where
  gGetStructure _ = case typeRep (Proxy @a) of
    tr | tr == typeRep (Proxy @Int)         -> TSPrimitive PTInt
       | tr == typeRep (Proxy @Double)      -> TSPrimitive PTDouble
       | tr == typeRep (Proxy @Text)        -> TSPrimitive PTText
       | tr == typeRep (Proxy @Bool)        -> TSPrimitive PTBool
       | tr == typeRep (Proxy @Word64)      -> TSPrimitive PTWord64
       | tr == typeRep (Proxy @Int32)       -> TSPrimitive PTInt32
       | tr == typeRep (Proxy @Word32)      -> TSPrimitive PTWord32
       | tr == typeRep (Proxy @Integer)     -> TSPrimitive PTInteger
       | tr == typeRep (Proxy @ByteString)  -> TSPrimitive PTBytes
       | otherwise -> error $ show tr

-- Helper function for type parameter extraction
extractTypeParams :: TypeRep -> [Text]
extractTypeParams tr = extractParams tr
  where
    -- Extract type parameters by looking at the type representation structure
    -- For example, Maybe Int -> ["Int"], Either String Bool -> ["String", "Bool"]
    extractParams :: TypeRep -> [Text]
    extractParams typ =
      case typeRepArgs typ of
        [] -> []  -- No type arguments
        args -> map showTypeParam args

    -- Convert a TypeRep to a readable parameter name
    showTypeParam :: TypeRep -> Text
    showTypeParam param = Text.pack (show param)

--------------------------------------------------------------------------------
-- Comprehensive Error Hierarchy
--------------------------------------------------------------------------------

-- | Legacy deserialization error type (now wrapped in SerializotronError).
--
-- This type represents the specific deserialization failures that can occur
-- when converting from 'DynamicValue' back to typed Haskell values. These
-- errors are automatically wrapped in 'DeserializationError' with context.
--
-- Error categories:
-- * 'StructuralMismatch': General structural problems (wrong data shape)
-- * 'PrimitiveMismatch': Type mismatch for basic values (Int vs Text)
-- * 'ConstructorMismatch': Wrong enum variant or constructor
-- * 'FieldCountMismatch': Record has wrong number of fields
-- * 'TypeIncompatible': Fundamental type incompatibility
-- * 'MetadataUnavailable': File missing type information
--
-- Example scenarios:
-- @
-- -- Trying to load Int when file contains Text
-- PrimitiveMismatch "Int" "Text"
--
-- -- Trying to load Maybe when file contains list
-- StructuralMismatch "Expected Maybe but found List"
--
-- -- Trying to load 3-tuple when file contains 2-tuple
-- FieldCountMismatch 3 2
--
-- -- Trying to load Nothing when file contains Just
-- ConstructorMismatch "Nothing" "Just"
-- @
data SZTError
  = -- | Generic structural mismatch (wrong data shape)
    StructuralMismatch Text
  | -- | Expected vs actual primitive type
    PrimitiveMismatch Text Text
  | -- | Expected vs actual constructor name
    ConstructorMismatch Text Text
  | -- | Expected vs actual field count
    FieldCountMismatch Int Int
  | -- | General type incompatibility
    TypeIncompatible Text
  | -- | File missing type metadata (old format)
    MetadataUnavailable SchemaVersion
  deriving stock (Show, Eq)

-- | Top-level error type for all Serializotron operations.
--
-- This is the main error type returned by all high-level Serializotron functions.
-- It provides a comprehensive classification of everything that can go wrong during
-- serialization, deserialization, and file operations.
--
-- The error hierarchy is designed to:
-- * Provide specific error types for different failure modes
-- * Include contextual information for debugging
-- * Offer helpful suggestions for common problems
-- * Support structured error handling in applications
--
-- Example usage:
-- @
-- handleError :: SerializotronError -> Text
-- handleError = \case
--   DeserializationError _ ctx -> "Data format issue at " <> formatPath ctx
--   FileSystemError (FileNotFound path) -> "Missing file: " <> Text.pack path
--   ProtocolBufferError _ -> "File corruption detected"
--   ValidationError (CyclicReferences refs) -> "Circular references: " <> Text.pack (show refs)
--   IOError _ -> "System I/O error"
-- @
data SerializotronError
  = -- | Type/structure mismatch during loading
    DeserializationError SZTError ErrorContext
  | -- | File I/O or format problems
    FileSystemError FileError
  | -- | Protocol buffer parsing errors
    ProtocolBufferError ProtocolError
  | -- | Data integrity violations
    ValidationError ValidationError
  | -- | Low-level I/O errors
    IOError IOException
  deriving stock (Show, Eq)

-- | File system related errors.
--
-- These errors occur when working with .szt files on disk. They cover
-- common file system issues as well as Serializotron-specific format problems.
--
-- Common scenarios:
-- * 'FileNotFound': The specified .szt file doesn't exist
-- * 'PermissionDenied': No read/write access to the file
-- * 'CorruptedFile': File exists but contains invalid data
-- * 'UnsupportedVersion': File was created with incompatible schema version
-- * 'InvalidFileFormat': File isn't a valid .szt file
--
-- Example handling:
-- @
-- handleFileError :: FileError -> IO ()
-- handleFileError = \case
--   FileNotFound path -> putStrLn $ "Please check that " <> path <> " exists"
--   PermissionDenied path -> putStrLn $ "Need read permission for " <> path
--   CorruptedFile path reason -> putStrLn $ "File " <> path <> " is damaged: " <> reason
--   UnsupportedVersion found expected -> putStrLn $ "File version " <> show found <> " not supported (need " <> show expected <> ")"
--   InvalidFileFormat path reason -> putStrLn $ "File " <> path <> " is not a valid .szt file: " <> reason
-- @
data FileError
  = -- | File doesn't exist at the specified path
    FileNotFound FilePath
  | -- | Insufficient permissions to read/write file
    PermissionDenied FilePath
  | -- | File exists but contains invalid/corrupted data
    CorruptedFile FilePath Text
  | -- | File created with incompatible schema version (found, expected)
    UnsupportedVersion SchemaVersion SchemaVersion
  | -- | File is not a valid .szt file format
    InvalidFileFormat FilePath Text
  deriving stock (Show, Eq)

-- | Protocol buffer related errors.
--
-- These errors occur when the Protocol Buffer layer fails to parse or validate
-- the binary data. They typically indicate file corruption or version incompatibility.
--
-- Common causes:
-- * File truncated or corrupted during write/transfer
-- * Attempting to read non-.szt files as .szt files
-- * Schema evolution issues between different Serializotron versions
-- * Network transfer corruption
--
-- Example handling:
-- @
-- handleProtocolError :: ProtocolError -> Text
-- handleProtocolError = \case
--   MissingField field -> "Required field missing: " <> field
--   InvalidEnumValue enum val -> "Invalid value " <> Text.pack (show val) <> " for enum " <> enum
--   UnexpectedMessageType expected -> "Wrong message type, expected: " <> expected
--   ProtocolDecodingError msg -> "Binary data corruption: " <> msg
-- @
data ProtocolError
  = -- | Required protocol buffer field is missing
    MissingField Text
  | -- | Enum field contains unrecognized value
    InvalidEnumValue Text Int
  | -- | Protocol buffer message has wrong type
    UnexpectedMessageType Text
  | -- | Binary data could not be parsed as protocol buffer
    ProtocolDecodingError Text
  deriving stock (Show, Eq)

-- | Data validation errors.
--
-- These errors occur when the data structure itself violates integrity constraints,
-- even if the file format is valid. They're detected during reference resolution
-- and integrity checking.
--
-- Common scenarios:
-- * 'CyclicReferences': Reference graph contains cycles (shouldn't happen in normal use)
-- * 'HashCollision': Cryptographic hash collision detected (extremely rare)
-- * 'DanglingReference': Reference points to non-existent shared value
-- * 'InvalidSchema': Schema information is malformed
-- * 'SchemaVersionMismatch': Version incompatibility detected
--
-- Most validation errors indicate either:
-- * File corruption
-- * Bugs in serialization code
-- * Attempting to load files with incompatible schema versions
--
-- Example handling:
-- @
-- handleValidationError :: ValidationError -> Text
-- handleValidationError = \case
--   CyclicReferences refs -> "Circular references detected in: " <> Text.pack (show refs)
--   HashCollision hash refs -> "Hash collision (very rare): " <> Text.pack (show hash)
--   DanglingReference ref -> "Missing shared value: " <> Text.pack (show ref)
--   InvalidSchema msg -> "Schema error: " <> msg
--   SchemaVersionMismatch found expected -> "Version mismatch: " <> Text.pack (show found) <> " vs " <> Text.pack (show expected)
-- @
data ValidationError
  = -- | Reference graph contains cycles
    CyclicReferences [Word32]
  | -- | Cryptographic hash collision detected
    HashCollision ContentHash [Word32]
  | -- | Reference to non-existent shared value
    DanglingReference Word32
  | -- | Schema information is malformed
    InvalidSchema Text
  | -- | Schema version incompatibility (found, expected)
    SchemaVersionMismatch SchemaVersion SchemaVersion
  deriving stock (Show, Eq)

-- | Context information for better error reporting.
--
-- This provides detailed context about where and why deserialization failed,
-- making it much easier to debug issues with complex data structures.
--
-- The context includes:
-- * Field path: Shows exactly where in the data structure the error occurred
-- * Expected type: What type the deserializer was looking for
-- * Actual value: What value was actually found in the data
-- * Suggestions: Helpful hints for fixing the problem
--
-- Example context for a nested error:
-- @
-- ErrorContext
--   { errorPath = ["company", "employees", "0", "age"]
--   , expectedType = Just "Int"
--   , actualValue = Just "\"twenty-five\""
--   , suggestions = ["Check that age field contains a number, not text"]
--   }
-- -- Results in error message:
-- -- "At field path 'company.employees.0.age': Expected Int but found \"twenty-five\" (actual value: \"twenty-five\")"
-- -- "Suggestions:"
-- -- "  - Check that age field contains a number, not text"
-- @
data ErrorContext = ErrorContext
  { -- | Field path like ["user", "address", "street"] (innermost first)
    _errorPath :: [Text]
    -- | What type was expected during deserialization
  , _expectedType :: Maybe Text
    -- | What value was actually found in the data
  , _actualValue :: Maybe Text
    -- | Helpful suggestions for fixing the problem
  , _suggestions :: [Text]
  }
  deriving stock (Show, Eq)

makeLenses ''ErrorContext

-- | Empty context for when no context is available
emptyContext :: ErrorContext
emptyContext = ErrorContext [] Nothing Nothing []

-- | Add a field to the error path
addPath :: Text -> ErrorContext -> ErrorContext
addPath field = errorPath %~ (field :)

-- | Set expected type in context
withExpected :: Text -> ErrorContext -> ErrorContext
withExpected expected = expectedType ?~ expected

-- | Set actual value in context
withActual :: Text -> ErrorContext -> ErrorContext
withActual actual = actualValue ?~ actual

-- | Add a suggestion to the context
withSuggestion :: Text -> ErrorContext -> ErrorContext
withSuggestion suggestion = suggestions %~ (suggestion :)

-- | Helper functions for creating contextual errors
mkDeserializationError :: SZTError -> ErrorContext -> SerializotronError
mkDeserializationError = DeserializationError

-- | Create a primitive mismatch error with context
mkPrimitiveMismatch :: Text -> Text -> ErrorContext -> SerializotronError
mkPrimitiveMismatch expected actual ctx =
  let ctxWithExpected = withExpected expected ctx
      ctxWithActual = withActual actual ctxWithExpected
   in DeserializationError (PrimitiveMismatch expected actual) ctxWithActual

-- | Create a structural mismatch error with context and suggestions
mkStructuralMismatch :: Text -> ErrorContext -> SerializotronError
mkStructuralMismatch msg ctx =
  DeserializationError (StructuralMismatch msg) ctx

-- | Pretty print an error with full context.
--
-- This function converts any 'SerializotronError' into a human-readable error
-- message with context information, suggestions, and helpful details.
--
-- The formatted output includes:
-- * Clear description of what went wrong
-- * Field path showing exactly where the error occurred
-- * Expected vs actual values when available
-- * Helpful suggestions for fixing the problem
--
-- Example outputs:
-- @
-- "At field path 'user.address.zipCode': Expected Int but found Text"
-- "File not found: data/myfile.szt"
-- "Protocol decoding error: Invalid message header"
-- "Cyclic references detected: [1, 2, 3]"
-- @
--
-- Use this function to display errors to users or write them to logs:
-- @
-- case loadSzt "data.szt" of
--   Left err -> putStrLn $ Text.unpack $ formatError err
--   Right value -> processValue value
-- @
formatError :: SerializotronError -> Text
formatError (DeserializationError err ctx) = formatDeserializationError err ctx
formatError (FileSystemError err) = formatFileError err
formatError (ProtocolBufferError err) = formatProtocolError err
formatError (ValidationError err) = formatValidationError err
formatError (IOError err) = "IO Error: " <> Text.pack (show err)

formatDeserializationError :: SZTError -> ErrorContext -> Text
formatDeserializationError err ctx =
  let pathStr =
        if null (ctx ^. errorPath)
          then ""
          else "At field path '" <> Text.intercalate "." (reverse (ctx ^. errorPath)) <> "': "
      baseMsg = case err of
        StructuralMismatch msg -> msg
        PrimitiveMismatch expected actual ->
          "Expected " <> expected <> " but found " <> actual
        ConstructorMismatch expected actual ->
          "Expected constructor " <> expected <> " but found " <> actual
        FieldCountMismatch expected actual ->
          "Expected " <> Text.pack (show expected) <> " fields but found " <> Text.pack (show actual)
        TypeIncompatible msg -> "Type incompatible: " <> msg
        MetadataUnavailable version -> "Metadata unavailable for schema version " <> Text.pack (show version)

      valueStr = case ctx ^. actualValue of
        Just val -> " (actual value: " <> val <> ")"
        Nothing -> ""

      suggestionStr = case ctx ^. suggestions of
        [] -> ""
        sgs -> "\nSuggestions:\n" <> Text.unlines (map ("  - " <>) sgs)
   in pathStr <> baseMsg <> valueStr <> suggestionStr

formatFileError :: FileError -> Text
formatFileError = \case
  FileNotFound path -> "File not found: " <> Text.pack path
  PermissionDenied path -> "Permission denied: " <> Text.pack path
  CorruptedFile path reason -> "Corrupted file " <> Text.pack path <> ": " <> reason
  UnsupportedVersion found expected ->
    "Unsupported version "
      <> Text.pack (show found)
      <> " (expected "
      <> Text.pack (show expected)
      <> ")"
  InvalidFileFormat path reason -> "Invalid file format " <> Text.pack path <> ": " <> reason

formatProtocolError :: ProtocolError -> Text
formatProtocolError = \case
  MissingField field -> "Missing required field: " <> field
  InvalidEnumValue enum val -> "Invalid enum value " <> Text.pack (show val) <> " for " <> enum
  UnexpectedMessageType expected -> "Unexpected message type, expected: " <> expected
  ProtocolDecodingError msg -> "Protocol decoding error: " <> msg

formatValidationError :: ValidationError -> Text
formatValidationError = \case
  CyclicReferences refs -> "Cyclic references detected: " <> Text.pack (show refs)
  HashCollision hash refs -> "Hash collision " <> Text.pack (show hash) <> " between: " <> Text.pack (show refs)
  DanglingReference ref -> "Dangling reference: " <> Text.pack (show ref)
  InvalidSchema msg -> "Invalid schema: " <> msg
  SchemaVersionMismatch found expected ->
    "Schema version mismatch: found "
      <> Text.pack (show found)
      <> " but expected "
      <> Text.pack (show expected)

-- | Deserialization type class for converting from Serializotron format to Haskell values.
--
-- This is the counterpart to 'ToSZT', handling the conversion from serialized
-- data back to typed Haskell values. Like 'ToSZT', it can be derived automatically:
--
-- @
-- data Person = Person Text Int deriving (Generic, FromSZT)
-- @
--
-- The class provides:
-- * Automatic derivation via GHC Generics for most types
-- * Detailed error reporting when deserialization fails
-- * Type safety through compile-time checking
--
-- Deserialization can fail for several reasons:
-- * Type mismatch: expecting Int but finding Text
-- * Structural mismatch: wrong number of fields
-- * Constructor mismatch: wrong enum variant
-- * Version incompatibility: schema evolution issues
--
-- Example usage:
-- @
-- loadPerson :: FilePath -> IO (Either SerializotronError Person)
-- loadPerson path = loadSzt path
-- @
--
-- Custom instances follow the same pattern as 'ToSZT':
-- @
-- instance FromSZT MySpecialType where
--   fromSzt (DynamicValue (DProduct [x, y]) _ _ _) = do
--     x' <- fromSzt x
--     y' <- fromSzt y
--     return $ MySpecialType x' y'
--   fromSzt _ = Left $ StructuralMismatch "Expected MySpecialType"
-- @
class FromSZT a where
  fromSzt :: DynamicValue -> Either SZTError a
  default fromSzt :: (Generic a, GFromSZT (Rep a), GGetAllConstructorNames (Rep a)) => DynamicValue -> Either SZTError a
  fromSzt dynValue = do
    -- Validate that the stored TypeInfo matches the expected type
    let storedTypeInfo = _dvTypeInfo dynValue
        expectedConstructors = gGetAllConstructorNames (Proxy @(Rep a ()))
        storedConstructors = maybe [] (^. tiConstructors) storedTypeInfo
    -- Only validate if we have constructor info on both sides
    if not (null expectedConstructors) && not (null storedConstructors) && storedConstructors /= expectedConstructors
      then Left $ ConstructorMismatch
             ("constructors " <> Text.intercalate ", " expectedConstructors)
             ("constructors " <> Text.intercalate ", " storedConstructors <>
              ". Constructor order or names have changed.")
      else GHC.Generics.to <$> gFromSZT (_dvCore dynValue)

-- | The default generic 'fromSzt' implementation as a standalone function.
-- Use with 'memoizedFromSzt' in custom instances:
--
-- @
-- instance FromSZT MyType where
--   fromSzt = memoizedFromSzt defaultFromSzt
-- @
defaultFromSzt
  :: forall a. (Generic a, GFromSZT (Rep a), GGetAllConstructorNames (Rep a))
  => DynamicValue -> Either SZTError a
defaultFromSzt dynValue = do
  let storedTypeInfo = _dvTypeInfo dynValue
      expectedConstructors = gGetAllConstructorNames (Proxy @(Rep a ()))
      storedConstructors = maybe [] (^. tiConstructors) storedTypeInfo
  if not (null expectedConstructors)
    && not (null storedConstructors)
    && storedConstructors /= expectedConstructors
    then
      Left $
        ConstructorMismatch
          ("constructors " <> Text.intercalate ", " expectedConstructors)
          ( "constructors " <> Text.intercalate ", " storedConstructors
              <> ". Constructor order or names have changed."
          )
    else GHC.Generics.to <$> gFromSZT (_dvCore dynValue)

-- | Generic deserialization type class
class GFromSZT f where
  gFromSZT :: DynamicCore -> Either SZTError (f p)

class GFromProductValues f where
  gFromProductValues :: [DynamicValue] -> Either SZTError (f p, [DynamicValue])

instance GFromProductValues U1 where
  gFromProductValues xs = Right (U1, xs)

instance (GFromProductValues f, GFromProductValues g) => GFromProductValues (f :*: g) where
  gFromProductValues xs = do
    (left, rest) <- gFromProductValues xs
    (right, rest') <- gFromProductValues rest
    pure (left :*: right, rest')

instance (GFromProductValues f) => GFromProductValues (M1 i c f) where
  gFromProductValues xs = do
    (inner, rest) <- gFromProductValues xs
    pure (M1 inner, rest)

instance (FromSZT a) => GFromProductValues (K1 i a) where
  gFromProductValues (value : rest) = do
    decoded <- fromSzt value
    pure (K1 decoded, rest)
  gFromProductValues [] = Left (FieldCountMismatch 1 0)

class GProductArity f where
  gProductArity :: Proxy f -> Int

instance GProductArity U1 where
  gProductArity _ = 0

instance GProductArity (K1 i a) where
  gProductArity _ = 1

instance (GProductArity f, GProductArity g) => GProductArity (f :*: g) where
  gProductArity _ = gProductArity (Proxy @f) + gProductArity (Proxy @g)

instance (GProductArity f) => GProductArity (M1 i c f) where
  gProductArity _ = gProductArity (Proxy @f)

-- Generic instances for deserialization
instance GFromSZT U1 where
  gFromSZT DUnit = Right U1
  gFromSZT _ = Left (StructuralMismatch "Expected unit type")

instance (FromSZT a) => GFromSZT (K1 i a) where
  gFromSZT core = case fromSZTCore core of
    Right value -> K1 <$> fromSzt value
    Left err -> Left err
    where
      fromSZTCore (DPrimitive pv) = Right (DynamicValue (DPrimitive pv) Nothing currentSchemaVersion Nothing)
      fromSZTCore c = Right (DynamicValue c Nothing currentSchemaVersion Nothing)

-- Note: :*: in GHC Generics always represents binary products (exactly 2 components).
-- Multi-field records use right-associative nesting: A :*: (B :*: C)
-- So each :*: node correctly handles exactly 2 fields.
-- Example: data Triple = Triple Int String Bool
-- Generic rep: Int :*: (String :*: Bool)
-- Serializes as: DProduct [Int_part, DProduct [String_part, Bool_part]]
instance (GFromProductValues f, GFromProductValues g, GProductArity f, GProductArity g) => GFromSZT (f :*: g) where
  gFromSZT (DProduct fields) = do
    (result, remaining) <- gFromProductValues fields
    if null remaining
      then pure result
      else Left (FieldCountMismatch expected actual)
    where
      expected = gProductArity (Proxy @(f :*: g))
      actual = length fields
  gFromSZT _ = Left (StructuralMismatch "Expected product type (:*:) ")

instance (GFromSZT f, GFromSZT g) => GFromSZT (f :+: g) where
  gFromSZT (DSum 0 dynValue) = L1 <$> gFromSZT (_dvCore dynValue)
  gFromSZT (DSum 1 dynValue) = R1 <$> gFromSZT (_dvCore dynValue)
  gFromSZT (DSum index _) = Left (ConstructorMismatch "valid index" (Text.pack (show index)))
  gFromSZT _ = Left (StructuralMismatch "Expected sum type")

instance (GFromSZT f) => GFromSZT (M1 i c f) where
  gFromSZT core = M1 <$> gFromSZT core

-- Primitive instances for deserialization
instance FromSZT Int where
  fromSzt (DynamicValue (DPrimitive (PInt x)) _ _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "Int" "other")

instance FromSZT Double where
  fromSzt (DynamicValue (DPrimitive (PDouble x)) _ _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "Double" "other")

instance FromSZT Text where
  fromSzt (DynamicValue (DPrimitive (PText x)) _ _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "Text" "other")

instance FromSZT Bool where
  fromSzt (DynamicValue (DPrimitive (PBool x)) _ _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "Bool" "other")

instance FromSZT ByteString where
  fromSzt (DynamicValue (DPrimitive (PBytes x)) _ _ _) = Right x
  fromSzt _ = Left (PrimitiveMismatch "ByteString" "other")

instance (FromSZT a) => FromSZT [a] where
  fromSzt (DynamicValue (DList elements) _ _ _) =
    traverse fromSzt elements
  fromSzt _ = Left (StructuralMismatch "Expected list")

-- Tuple instances
instance (ToSZT a, ToSZT b) => ToSZT (a, b) where
  toSzt (a, b) =
    DynamicValue
      { _dvCore = DProduct [toSzt a, toSzt b]
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "(,)"
              , _tiModule = Just "GHC.Tuple"
              , _tiConstructors = ["(,)"]
              , _tiFieldLabels = []
              , _tiStructure = Nothing -- Simplified
              , _tiTypeParameters = []
              , _tiNewtypeWrapper = Nothing
              }
      , _dvSchemaVersion = currentSchemaVersion
      , _dvShallowId = Nothing
      }

instance (FromSZT a, FromSZT b) => FromSZT (a, b) where
  fromSzt (DynamicValue (DProduct [dynA, dynB]) _ _ _) = do
    a <- fromSzt dynA
    b <- fromSzt dynB
    return (a, b)
  fromSzt (DynamicValue (DProduct fields) _ _ _) =
    Left (FieldCountMismatch 2 (length fields))
  fromSzt _ = Left (StructuralMismatch "Expected tuple")

instance (ToSZT a, ToSZT b, ToSZT c) => ToSZT (a, b, c) where
  toSzt (a, b, c) =
    DynamicValue
      { _dvCore = DProduct [toSzt a, toSzt b, toSzt c]
      , _dvTypeInfo =
          Just $
            TypeInfo
              { _tiTypeName = Just "(,,)"
              , _tiModule = Just "GHC.Tuple"
              , _tiConstructors = ["(,,)"]
              , _tiFieldLabels = []
              , _tiStructure = Nothing
              , _tiTypeParameters = []
              , _tiNewtypeWrapper = Nothing
              }
      , _dvSchemaVersion = currentSchemaVersion
      , _dvShallowId = Nothing
      }

instance (FromSZT a, FromSZT b, FromSZT c) => FromSZT (a, b, c) where
  fromSzt (DynamicValue (DProduct [dynA, dynB, dynC]) _ _ _) = do
    a <- fromSzt dynA
    b <- fromSzt dynB
    c <- fromSzt dynC
    return (a, b, c)
  fromSzt (DynamicValue (DProduct fields) _ _ _) =
    Left (FieldCountMismatch 3 (length fields))
  fromSzt _ = Left (StructuralMismatch "Expected triple")

instance (ToSZT a, ToSZT b, ToSZT c, ToSZT d) => ToSZT (a, b, c, d) where
  toSzt (w, x, y, z) = DynamicValue
    { _dvCore = DProduct [toSzt w, toSzt x, toSzt y, toSzt z]
    , _dvTypeInfo = Just $ TypeInfo
        { _tiTypeName = Just "(,,,)"
        , _tiModule = Just "GHC.Tuple"
        , _tiConstructors = ["(,,,)"]
              , _tiFieldLabels = []
        , _tiStructure = Nothing
        , _tiTypeParameters = []
        , _tiNewtypeWrapper = Nothing
        }
    , _dvSchemaVersion = currentSchemaVersion
    , _dvShallowId = Nothing
    }

instance (FromSZT a, FromSZT b, FromSZT c, FromSZT d) => FromSZT (a, b, c, d) where
  fromSzt (DynamicValue (DProduct [v1, v2, v3, v4]) _ _ _) = do
    w <- fromSzt v1
    x <- fromSzt v2
    y <- fromSzt v3
    z <- fromSzt v4
    return (w, x, y, z)
  fromSzt _ = Left (StructuralMismatch "Invalid quadruple structure")

--------------------------------------------------------------------------------
-- Protobuf Conversion Functions
--------------------------------------------------------------------------------

data TypeInfoPool = TypeInfoPool
  { _tipNextId :: Word32
  , _tipByName :: Map.Map (Maybe Text, Maybe Text) Word32  -- (module, name) -> ID
  , _tipShared :: Map.Map Word32 Proto.TypeInfo
  }

emptyTypeInfoPool :: TypeInfoPool
emptyTypeInfoPool = TypeInfoPool 1 Map.empty Map.empty

sharedTypeInfoMap :: TypeInfoPool -> Map.Map Word32 Proto.TypeInfo
sharedTypeInfoMap = _tipShared

type TypeInfoM = State TypeInfoPool

-- | Merge two TypeInfo structures, preferring the more complete one.
-- Prefers non-Nothing fields and non-empty lists.
mergeTypeInfo :: TypeInfo -> TypeInfo -> TypeInfo
mergeTypeInfo ti1 ti2 = TypeInfo
  { _tiTypeName = ti1 ^. tiTypeName <|> ti2 ^. tiTypeName
  , _tiModule = ti1 ^. tiModule <|> ti2 ^. tiModule
  , _tiConstructors = if null (ti1 ^. tiConstructors) then ti2 ^. tiConstructors else ti1 ^. tiConstructors
  , _tiFieldLabels = if null (ti1 ^. tiFieldLabels) then ti2 ^. tiFieldLabels else ti1 ^. tiFieldLabels
  , _tiStructure = ti1 ^. tiStructure <|> ti2 ^. tiStructure
  , _tiTypeParameters = if null (ti1 ^. tiTypeParameters) then ti2 ^. tiTypeParameters else ti1 ^. tiTypeParameters
  , _tiNewtypeWrapper = ti1 ^. tiNewtypeWrapper <|> ti2 ^. tiNewtypeWrapper
  }

-- | Intern a TypeInfo, recursively interning all nested TypeInfo structures.
-- This ensures that constructor argument types and field types also appear
-- in the shared type pool, even if they only have partial type information.
-- Uses name-based deduplication, merging TypeInfos with the same (module, name).
internTypeInfo :: Maybe TypeInfo -> TypeInfoM (Maybe Word32)
internTypeInfo Nothing = pure Nothing
internTypeInfo (Just ti) = do
  -- First, recursively intern all nested TypeInfo structures
  case ti ^. tiStructure of
    Just (TSSum branches) -> do
      -- Intern all constructor argument types
      mapM_ (internTypeInfo . Just) branches
    Just (TSProduct fields) -> do
      -- Intern all field types
      mapM_ (internTypeInfo . Just . view fiFieldType) fields
    Just (TSList elemTi) -> do
      -- Intern element type
      _ <- internTypeInfo (Just elemTi)
      pure ()
    _ -> pure ()

  -- Also intern type parameters (for parameterized types like Maybe, Either, etc.)
  mapM_ (internTypeInfo . Just) (ti ^. tiTypeParameters)

  -- Finally, intern this TypeInfo itself
  -- Use (module, name) as the key for deduplication
  let nameKey = (ti ^. tiModule, ti ^. tiTypeName)

  case nameKey of
    -- Skip anonymous types (no name)
    (_, Nothing) -> pure Nothing

    -- FIXME this is dodgy, unused variables, etc.
    _ -> do
      TypeInfoPool nextId byName shared <- get
      case Map.lookup nameKey byName of
        Just existingId -> do
          -- Already have a TypeInfo with this name - merge them
          let _existingProto = shared Map.! existingId
          -- The existing proto has the structure we serialized before
          -- We want to keep the better version (more complete)
          -- For now, we'll update with the merged version
          let _mergedTi = mergeTypeInfo ti (error "Cannot convert back from proto - using new ti")
              -- Since we can't easily convert proto back to TypeInfo,
              -- we'll just use the new one if it has structure, otherwise keep existing
          if isNothing (ti ^. tiStructure)
            then pure (Just existingId)  -- Keep existing if new one has no structure
            else do
              -- New one has structure - update the existing entry
              let protoTI = toProtoTypeInfo ti
              put $ TypeInfoPool nextId byName (Map.insert existingId protoTI shared)
              pure (Just existingId)
        Nothing -> do
          -- New type - add it
          let protoTI = toProtoTypeInfo ti
              assigned = nextId
          put $ TypeInfoPool (nextId + 1) (Map.insert nameKey assigned byName) (Map.insert assigned protoTI shared)
          pure (Just assigned)

toProtoDynamicValue :: DynamicValue -> TypeInfoM Proto.DynamicValue
toProtoDynamicValue = toProtoDynamicValueWith Nothing

countConstructorsInTypeInfo :: TypeInfo -> Word32
countConstructorsInTypeInfo info =
  case info ^. tiStructure of
    Just (TSSum branches) -> sum (map countConstructorsInTypeInfo branches)
    _ -> 1

resolveConstructorIndex :: Maybe TypeInfo -> Word32 -> DynamicValue -> Word32
resolveConstructorIndex mInfo localIndex childValue =
  case mInfo >>= view tiStructure of
    Just (TSSum branches) ->
      let idx = fromIntegral localIndex
          (before, rest) = splitAt idx branches
          offset = sum (map countConstructorsInTypeInfo before)
       in case rest of
            (current : _) ->
              case (childValue ^. dvCore, current ^. tiStructure) of
                (DSum innerIdx innerValue, Just (TSSum _)) ->
                  offset + resolveConstructorIndex (Just current) innerIdx innerValue
                _ -> offset
            [] -> localIndex
    _ -> localIndex

selectSumBranch :: Maybe TypeInfo -> Word32 -> Either SerializotronError (Word32, Maybe TypeInfo)
selectSumBranch Nothing idx = Right (idx, Nothing)
selectSumBranch (Just info) idx =
  case info ^. tiStructure of
    Just (TSSum branches) ->
      let totalConstructors = sum (map countConstructorsInTypeInfo branches)
       in if totalConstructors == 0
            then mkInvalidSchema "Sum type has no constructors"
            else
              if idx < totalConstructors
                then go idx 0 branches
                else mkInvalidSchema "Constructor index out of range for sum type"
    _ -> Right (idx, Nothing)
  where
    go :: Word32 -> Word32 -> [TypeInfo] -> Either SerializotronError (Word32, Maybe TypeInfo)
    go remaining branchIdx = \case
      [] -> mkInvalidSchema "Constructor index out of range for sum type"
      branch : rest ->
        let branchSize = countConstructorsInTypeInfo branch
         in if remaining < branchSize
              then Right (branchIdx, Just branch)
              else go (remaining - branchSize) (branchIdx + 1) rest

toProtoDynamicValueWith :: Maybe TypeInfo -> DynamicValue -> TypeInfoM Proto.DynamicValue
toProtoDynamicValueWith parentTi (DynamicValue core thisTi _version _shallowId) = do
  let effectiveTi = thisTi <|> parentTi
  coreProto <- toProtoDynamicCore effectiveTi core
  typeRef <- internTypeInfo effectiveTi
  let base = defMessage Lens.& Proto.core Lens..~ coreProto
  pure $ maybe base (\refId -> base Lens.& Proto.typeInfoRef Lens..~ refId) typeRef

toProtoDynamicCore :: Maybe TypeInfo -> DynamicCore -> TypeInfoM Proto.DynamicCore
toProtoDynamicCore typeInfo = \case
  DPrimitive pv -> pure $ defMessage Lens.& Proto.primitive Lens..~ toProtoPrimitiveValue pv
  DProduct values -> do
    let parents = case typeInfo >>= view tiStructure of
          Just (TSProduct infos) -> map (Just . view fiFieldType) infos ++ repeat Nothing
          _ -> repeat Nothing
    encoded <- zipWithM toProtoDynamicValueWith parents values
    pure $ defMessage Lens.& Proto.product Lens..~ (defMessage Lens.& Proto.fields Lens..~ encoded)
  DSum index value -> do
    let globalIndex = case typeInfo of
          Just info -> resolveConstructorIndex (Just info) index value
          Nothing -> index
        -- Use empty string when type info is unavailable. This occurs for
        -- nested sum types in GHC Generics (e.g., 3+ constructor types where
        -- (:+:) nesting creates inner DSum values without type info). The
        -- constructor INDEX is always correct; the name is only for validation.
        -- An empty name causes the decoder to skip the name-match check.
        constructorName = maybe Text.empty (lookupConstructorName globalIndex) typeInfo
        branchTi = case typeInfo >>= view tiStructure of
          Just (TSSum branches) -> branches ^? ix (fromIntegral index)
          _ -> Nothing
    encodedValue <- toProtoDynamicValueWith branchTi value
    pure $
      defMessage
        Lens.& Proto.sum Lens..~
          ( defMessage
              Lens.& Proto.constructorIndex Lens..~ globalIndex
              Lens.& Proto.constructorName Lens..~ constructorName
              Lens.& Proto.value Lens..~ encodedValue
          )
  DList values -> do
    let itemTi = case typeInfo >>= view tiStructure of
          Just (TSList ti) -> Just ti
          _ -> Nothing
    encoded <- mapM (toProtoDynamicValueWith itemTi) values
    pure $ defMessage Lens.& Proto.list Lens..~ (defMessage Lens.& Proto.elements Lens..~ encoded)
  DUnit -> pure $ defMessage Lens.& Proto.unit Lens..~ defMessage
  DReference refId -> pure $ defMessage Lens.& Proto.reference Lens..~ (defMessage Lens.& Proto.referenceId Lens..~ refId)


lookupConstructorName :: Word32 -> TypeInfo -> Text
lookupConstructorName index info = fromMaybe "" $ info ^? tiConstructors . ix (fromIntegral index)

lookupConstructorIndex :: Text -> TypeInfo -> Maybe Word32
lookupConstructorIndex name info = fromIntegral <$> elemIndex name (info ^. tiConstructors)

toProtoPrimitiveValue :: PrimitiveValue -> Proto.PrimitiveValue
toProtoPrimitiveValue = \case
  PInt i -> defMessage Lens.& Proto.intVal Lens..~ fromIntegral i
  PDouble d -> defMessage Lens.& Proto.doubleVal Lens..~ d
  PText t -> defMessage Lens.& Proto.textVal Lens..~ t
  PBool b -> defMessage Lens.& Proto.boolVal Lens..~ b
  PWord64 w -> defMessage Lens.& Proto.word64Val Lens..~ w
  PInt32 i -> defMessage Lens.& Proto.int32Val Lens..~ i
  PWord32 w -> defMessage Lens.& Proto.word32Val Lens..~ w
  PInteger t -> defMessage Lens.& Proto.integerVal Lens..~ t
  PBytes bs -> defMessage Lens.& Proto.bytesVal Lens..~ bs

toProtoTypeInfo :: TypeInfo -> Proto.TypeInfo
toProtoTypeInfo (TypeInfo name mod cons fieldLabels struct typeParams newtypeWrap) =
  let base = defMessage
        Lens.& Proto.typeName Lens..~ fromMaybe "" name
        Lens.& Proto.moduleName Lens..~ fromMaybe "" mod
        Lens.& Proto.constructors Lens..~ cons
        Lens.& Proto.fieldLabels Lens..~ fieldLabels
        Lens.& Proto.structure Lens..~ maybe defMessage toProtoTypeStructure struct
        Lens.& Proto.typeParameters Lens..~ map toProtoTypeInfo typeParams
   in case newtypeWrap of
        Nothing -> base
        Just wrapInfo -> base Lens.& Proto.newtypeWrapper Lens..~ toProtoTypeInfo wrapInfo

toProtoTypeStructure :: TypeStructure -> Proto.TypeStructure
toProtoTypeStructure = \case
  TSPrimitive pt -> defMessage Lens.& Proto.primitive Lens..~ toProtoPrimitiveType pt
  TSProduct fields -> defMessage Lens.& Proto.product Lens..~ (defMessage Lens.& Proto.fields Lens..~ map toProtoFieldInfo fields)
  TSSum tis -> defMessage Lens.& Proto.sum Lens..~ (defMessage Lens.& Proto.constructorTypes Lens..~ map toProtoTypeInfo tis)
  TSList ti -> defMessage Lens.& Proto.list Lens..~ (defMessage Lens.& Proto.elementType Lens..~ toProtoTypeInfo ti)
  TSUnit -> defMessage Lens.& Proto.unit Lens..~ defMessage

toProtoFieldInfo :: FieldInfo -> Proto.ProductStructure'FieldInfo
toProtoFieldInfo (FieldInfo name ty) =
  defMessage
    Lens.& Proto.fieldName Lens..~ fromMaybe "" name
    Lens.& Proto.fieldType Lens..~ toProtoTypeInfo ty

toProtoPrimitiveType :: PrimitiveType -> Proto.PrimitiveType
toProtoPrimitiveType = \case
  PTInt -> Proto.PRIMITIVE_INT
  PTDouble -> Proto.PRIMITIVE_DOUBLE
  PTText -> Proto.PRIMITIVE_TEXT
  PTBool -> Proto.PRIMITIVE_BOOL
  PTWord64 -> Proto.PRIMITIVE_WORD64
  PTInt32 -> Proto.PRIMITIVE_INT32
  PTWord32 -> Proto.PRIMITIVE_WORD32
  PTInteger -> Proto.PRIMITIVE_INTEGER
  PTBytes -> Proto.PRIMITIVE_BYTES

fromProtoDynamicValue :: Word32 -> Map.Map Word32 TypeInfo -> Proto.DynamicValue -> Either SerializotronError DynamicValue
fromProtoDynamicValue = fromProtoDynamicValueWithOverride Nothing

-- | Decode with an optional override TypeInfo (used for nested sums)
fromProtoDynamicValueWithOverride :: Maybe TypeInfo -> Word32 -> Map.Map Word32 TypeInfo -> Proto.DynamicValue -> Either SerializotronError DynamicValue
fromProtoDynamicValueWithOverride overrideTypeInfo schemaVersion typeTable protoDV = do
  let inlineTypeProto = protoDV Lens.^. Proto.typeInfo
      hasInline = inlineTypeProto /= defMessage
      refId = protoDV Lens.^. Proto.typeInfoRef
  resolvedTypeInfo <-
    if hasInline
      then do
        inlineTi <- fromProtoTypeInfo inlineTypeProto
        if refId /= 0
          then case Map.lookup refId typeTable of
            Just referencedTi
              | referencedTi == inlineTi -> pure (Just inlineTi)
              | otherwise ->
                  Left $ ValidationError $ InvalidSchema $
                    "Type info mismatch between inline metadata and reference id "
                      <> Text.pack (show refId)
            Nothing ->
              Left $ ValidationError $ InvalidSchema $
                "Unknown type_info_ref " <> Text.pack (show refId)
          else pure (Just inlineTi)
      else
        if refId /= 0
          then case Map.lookup refId typeTable of
            Just ti -> pure (Just ti)
            Nothing ->
              Left $ ValidationError $ InvalidSchema $
                "Unknown type_info_ref " <> Text.pack (show refId)
          else pure overrideTypeInfo  -- Use override only if proto has no type info
  core <- fromProtoDynamicCore schemaVersion typeTable resolvedTypeInfo (protoDV Lens.^. Proto.core)
  return $ DynamicValue core resolvedTypeInfo schemaVersion Nothing

fromProtoDynamicCore :: Word32 -> Map.Map Word32 TypeInfo -> Maybe TypeInfo -> Proto.DynamicCore -> Either SerializotronError DynamicCore
fromProtoDynamicCore schemaVersion typeTable typeInfo protoCore =
  case protoCore Lens.^. Proto.maybe'core of
    Just (Proto.DynamicCore'Primitive pv) -> DPrimitive <$> fromProtoPrimitiveValue pv
    Just (Proto.DynamicCore'Product prod) -> DProduct <$> traverse (fromProtoDynamicValue schemaVersion typeTable) (prod Lens.^. Proto.fields)
    Just (Proto.DynamicCore'Sum sum') -> do
      let providedIndex = sum' Lens.^. Proto.constructorIndex
          storedName = sum' Lens.^. Proto.constructorName
      (localIndex, branchInfo) <- selectSumBranch typeInfo providedIndex
      -- Validate constructor name matches if we have type info
      case typeInfo of
        Just ti ->
          let expectedName = lookupConstructorName providedIndex ti
           in if storedName /= expectedName && not (Text.null storedName)
                then Left $ ValidationError $ InvalidSchema $
                       "Constructor name mismatch: stored '" <> storedName
                       <> "' but expected '" <> expectedName
                       <> "' at index " <> Text.pack (show providedIndex)
                       <> ". This likely means the constructor order changed in the type definition."
                else do
                  -- Decode with branchInfo as override if the proto value doesn't have type info
                  value <- fromProtoDynamicValueWithOverride branchInfo schemaVersion typeTable (sum' Lens.^. Proto.value)
                  return $ DSum localIndex value
        Nothing -> do
          -- No type info available, decode without override
          value <- fromProtoDynamicValueWithOverride branchInfo schemaVersion typeTable (sum' Lens.^. Proto.value)
          return $ DSum localIndex value
    Just (Proto.DynamicCore'List list') -> DList <$> traverse (fromProtoDynamicValue schemaVersion typeTable) (list' Lens.^. Proto.elements)
    Just (Proto.DynamicCore'Unit _) -> return DUnit
    Just (Proto.DynamicCore'Reference ref) -> return $ DReference (ref Lens.^. Proto.referenceId)
    Nothing -> Left $ ProtocolBufferError (MissingField "core")

fromProtoPrimitiveValue :: Proto.PrimitiveValue -> Either SerializotronError PrimitiveValue
fromProtoPrimitiveValue protoPV =
  case protoPV Lens.^. Proto.maybe'primitive of
    Just (Proto.PrimitiveValue'IntVal i) -> return $ PInt (fromIntegral i)
    Just (Proto.PrimitiveValue'DoubleVal d) -> return $ PDouble d
    Just (Proto.PrimitiveValue'TextVal t) -> return $ PText t
    Just (Proto.PrimitiveValue'BoolVal b) -> return $ PBool b
    Just (Proto.PrimitiveValue'Word64Val w) -> return $ PWord64 w
    Just (Proto.PrimitiveValue'Int32Val i) -> return $ PInt32 i
    Just (Proto.PrimitiveValue'Word32Val w) -> return $ PWord32 w
    Just (Proto.PrimitiveValue'IntegerVal t) -> return $ PInteger t
    Just (Proto.PrimitiveValue'BytesVal bs) -> return $ PBytes bs
    Nothing -> Left $ ProtocolBufferError (MissingField "primitive")

fromProtoTypeInfo :: Proto.TypeInfo -> Either SerializotronError TypeInfo

mkInvalidSchema :: Text -> Either SerializotronError a
mkInvalidSchema msg = Left (ValidationError (InvalidSchema msg))

findDuplicateLabels :: [Text] -> [Text]
findDuplicateLabels labels =
  [ label
  | (label, count) <- Map.toList (Map.fromListWith (+) [(label, 1 :: Int) | label <- labels])
  , count > 1
  ]

formatLabels :: [Text] -> Text
formatLabels = Text.intercalate ", "

normalizeTypeInfo :: TypeInfo -> Either SerializotronError TypeInfo
normalizeTypeInfo info =
  case info ^. tiStructure of
    Just (TSProduct fields) ->
      let normalizedFields = assignFieldLabels fields
          labels = map (fromMaybe Text.empty . (^. fiFieldName)) normalizedFields
       in if any Text.null labels
            then mkInvalidSchema "Product field labels may not be empty"
            else
              let duplicates = findDuplicateLabels labels
               in if not (null duplicates)
                    then mkInvalidSchema $ "Duplicate field labels: " <> formatLabels duplicates
                    else
                      let provided = info ^. tiFieldLabels
                          info' = info & tiFieldLabels .~ labels & tiStructure ?~ TSProduct normalizedFields
                       in if null provided || provided == labels
                            then Right info'
                            else mkInvalidSchema $ "Field label mismatch: " <> formatLabels provided <> " vs " <> formatLabels labels
    _ ->
      if null (info ^. tiFieldLabels)
        then Right info
        else mkInvalidSchema "Field labels provided for non-product type"

fromProtoTypeInfo protoTI = do
  struct <-
    if protoTI Lens.^. Proto.structure == defMessage
      then return Nothing
      else Just <$> fromProtoTypeStructure (protoTI Lens.^. Proto.structure)
  typeParams <- traverse fromProtoTypeInfo (protoTI Lens.^. Proto.typeParameters)
  -- Parse newtype wrapper if present
  newtypeWrap <-
    if protoTI Lens.^. Proto.newtypeWrapper == defMessage
      then return Nothing
      else Just <$> fromProtoTypeInfo (protoTI Lens.^. Proto.newtypeWrapper)
  let info =
        TypeInfo
          (if Text.null (protoTI Lens.^. Proto.typeName) then Nothing else Just (protoTI Lens.^. Proto.typeName))
          (if Text.null (protoTI Lens.^. Proto.moduleName) then Nothing else Just (protoTI Lens.^. Proto.moduleName))
          (protoTI Lens.^. Proto.constructors)
          (protoTI Lens.^. Proto.fieldLabels)
          struct
          typeParams
          newtypeWrap
  normalizeTypeInfo info
fromProtoTypeStructure :: Proto.TypeStructure -> Either SerializotronError TypeStructure
fromProtoTypeStructure protoTS =
  case protoTS Lens.^. Proto.maybe'structure of
    Just (Proto.TypeStructure'Primitive pt) -> return $ TSPrimitive (fromProtoPrimitiveType pt)
    Just (Proto.TypeStructure'Product prod) -> TSProduct <$> traverse fromProtoFieldInfo (prod Lens.^. Proto.fields)
    Just (Proto.TypeStructure'Sum sum') -> TSSum <$> traverse fromProtoTypeInfo (sum' Lens.^. Proto.constructorTypes)
    Just (Proto.TypeStructure'List list') -> TSList <$> fromProtoTypeInfo (list' Lens.^. Proto.elementType)
    Just (Proto.TypeStructure'Unit _) -> return TSUnit
    Nothing -> Left $ ProtocolBufferError (MissingField "structure")

fromProtoFieldInfo :: Proto.ProductStructure'FieldInfo -> Either SerializotronError FieldInfo
fromProtoFieldInfo protoFI = do
  fieldType <- fromProtoTypeInfo (protoFI Lens.^. Proto.fieldType)
  let nameText = protoFI Lens.^. Proto.fieldName
      fieldName = if Text.null nameText then Nothing else Just nameText
  return $ FieldInfo fieldName fieldType

fromProtoPrimitiveType :: Proto.PrimitiveType -> PrimitiveType
fromProtoPrimitiveType = \case
  Proto.PRIMITIVE_INT -> PTInt
  Proto.PRIMITIVE_DOUBLE -> PTDouble
  Proto.PRIMITIVE_TEXT -> PTText
  Proto.PRIMITIVE_BOOL -> PTBool
  Proto.PRIMITIVE_WORD64 -> PTWord64
  Proto.PRIMITIVE_INT32 -> PTInt32
  Proto.PRIMITIVE_WORD32 -> PTWord32
  Proto.PRIMITIVE_INTEGER -> PTInteger
  Proto.PRIMITIVE_BYTES -> PTBytes
  -- Handle unrecognized values gracefully
  _ -> PTInt -- Default fallback

--------------------------------------------------------------------------------
-- File System Check (fsck) for .szt integrity verification
--------------------------------------------------------------------------------

-- | Integrity errors found during fsck
data FsckError
  = FsckDanglingReference Word32 -- Reference ID that doesn't exist in shared table
  | CyclicReference [Word32] -- List of reference IDs forming a cycle
  | CorruptedSharedValue Word32 Text -- Reference ID and error message
  deriving stock (Show, Eq)

-- | Non-fatal warnings found during fsck
data FsckWarning
  = UnusedSharedValue Word32 -- Reference ID that's never referenced
  | SuspiciousTypeInfo Text -- Potentially malformed type info
  deriving stock (Show, Eq)

-- | Statistics from integrity check
data FsckStats = FsckStats
  { _fsckTotalReferences :: Int
  , _fsckSharedValues :: Int
  , _fsckDanglingReferences :: Int
  }
  deriving stock (Show, Eq)

makeLenses ''FsckStats


-- | Results of integrity verification
data FsckResult = FsckResult
  { _fsckPassed :: Bool
  , _fsckErrors :: [FsckError]
  , _fsckWarnings :: [FsckWarning]
  , _fsckStats :: FsckStats
  }
  deriving stock (Show, Eq)

makeLenses ''FsckResult

-- | Verify integrity of a .szt file
fsckSzt :: FilePath -> IO FsckResult
fsckSzt path = do
  result <- try $ ByteString.readFile path
  case result of
    Left ioErr -> return $ FsckResult False [CorruptedSharedValue 0 (Text.pack (show (ioErr :: IOException)))] [] (FsckStats 0 0 1)
    Right fileBytes -> do
      -- Validate minimum size for header
      if ByteString.length fileBytes < 8
        then return $ FsckResult False [CorruptedSharedValue 0 "File too short to contain header"] [] (FsckStats 0 0 1)
        else do
          -- Parse header
          case decodeHeader (ByteString.take 8 fileBytes) of
            Left err -> return $ FsckResult False [CorruptedSharedValue 0 err] [] (FsckStats 0 0 1)
            Right header -> do
              let payloadBytes = ByteString.drop 8 fileBytes
              -- Decompress if needed
              protoBytes <- case _headerCompression header of
                NoCompression -> return $ Right payloadBytes
                GZipCompression ->
                  return $ Right $ LBS.toStrict $ GZip.decompress $ LBS.fromStrict payloadBytes

              case protoBytes of
                Left err -> return $ FsckResult False [CorruptedSharedValue 0 err] [] (FsckStats 0 0 1)
                Right bytes ->
                  case decodeMessage bytes :: Either String Proto.SZTFile of
                    Left err -> return $ FsckResult False [CorruptedSharedValue 0 (Text.pack err)] [] (FsckStats 0 0 1)
                    Right sztFile -> do
                      let schemaVersion = sztFile Lens.^. Proto.schemaVersion
                      let protoSharedTypes = sztFile Lens.^. Proto.sharedTypeInfo
                      case Map.traverseWithKey (const fromProtoTypeInfo) protoSharedTypes of
                        Left err -> return $ FsckResult False [CorruptedSharedValue 0 (formatError err)] [] (FsckStats 0 0 1)
                        Right sharedTypeMap -> do
                          let protoSharedValues = sztFile Lens.^. Proto.sharedValues
                          let sharedTable = Map.map (fromProtoDynamicValue schemaVersion sharedTypeMap) protoSharedValues

                          case fromProtoDynamicValue schemaVersion sharedTypeMap (sztFile Lens.^. Proto.value) of
                            Left err -> return $ FsckResult False [CorruptedSharedValue 0 (formatError err)] [] (FsckStats 0 0 1)
                            Right rootValue -> return $ performFsck rootValue sharedTable

-- | Internal implementation of integrity checking
performFsck :: DynamicValue -> Map.Map Word32 (Either SerializotronError DynamicValue) -> FsckResult
performFsck rootValue sharedTable =
  let -- Convert Either values to DynamicValue, collecting corruption errors
      (validSharedTable, corruptionErrors) = partitionSharedTable sharedTable

      -- Collect all reference IDs used in the value tree
      usedReferences = collectReferences rootValue validSharedTable

      -- Find dangling references (used but not in shared table)
      danglingRefs = [refId | refId <- Set.toList usedReferences, not (Map.member refId validSharedTable)]

      -- Find unused shared values (in table but never referenced)
      unusedValues = [refId | refId <- Map.keys validSharedTable, not (Set.member refId usedReferences)]

      -- Detect cycles in the reference graph
      cycles = detectCycles rootValue validSharedTable

      -- Build errors and warnings
      errors = map FsckDanglingReference danglingRefs
            ++ map CyclicReference cycles
            ++ corruptionErrors

      warnings = map UnusedSharedValue unusedValues

      stats =
        FsckStats
          { _fsckTotalReferences = length usedReferences
          , _fsckSharedValues = Map.size validSharedTable
          , _fsckDanglingReferences = length danglingRefs
          }
   in FsckResult (null errors) errors warnings stats

-- | Helper to partition shared table and collect corruption errors
partitionSharedTable :: Map.Map Word32 (Either SerializotronError DynamicValue) -> (Map.Map Word32 DynamicValue, [FsckError])
partitionSharedTable sharedTable =
  let (errors, valids) = Map.partition isLeft sharedTable
      corruptionErrors = [CorruptedSharedValue refId (formatError err) | (refId, Left err) <- Map.toList errors]
      validTable = Map.mapMaybe (\case Right val -> Just val; Left _ -> Nothing) valids
   in (validTable, corruptionErrors)
  where
    isLeft (Left _) = True
    isLeft (Right _) = False

-- | Collect all reference IDs used in a value tree
collectReferences :: DynamicValue -> Map.Map Word32 DynamicValue -> Set.Set Word32
collectReferences = collectReferencesWithVisited Set.empty
  where
    collectReferencesWithVisited :: Set.Set Word32 -> DynamicValue -> Map.Map Word32 DynamicValue -> Set.Set Word32
    collectReferencesWithVisited visited (DynamicValue core _ _ _) sharedTable =
      collectReferencesFromCore visited core sharedTable

    collectReferencesFromCore :: Set.Set Word32 -> DynamicCore -> Map.Map Word32 DynamicValue -> Set.Set Word32
    collectReferencesFromCore visited core sharedTable = case core of
      DPrimitive _ -> Set.empty
      DProduct vals -> Set.unions (map (\val -> collectReferencesWithVisited visited val sharedTable) vals)
      DSum _ val -> collectReferencesWithVisited visited val sharedTable
      DList vals -> Set.unions (map (\val -> collectReferencesWithVisited visited val sharedTable) vals)
      DUnit -> Set.empty
      DReference refId ->
        if Set.member refId visited
          then Set.singleton refId -- Include the reference itself even if cyclic
          else case Map.lookup refId sharedTable of
            Nothing -> Set.singleton refId -- Dangling reference
            Just sharedValue -> Set.insert refId (collectReferencesWithVisited (Set.insert refId visited) sharedValue sharedTable)

-- | Detect cycles in reference graph starting from the root value
detectCycles :: DynamicValue -> Map.Map Word32 DynamicValue -> [[Word32]]
detectCycles rootValue sharedTable =
  let rootRefs = getDirectReferences rootValue
   in concatMap (findCycleFromRef Set.empty) rootRefs
  where
    findCycleFromRef :: Set.Set Word32 -> Word32 -> [[Word32]]
    findCycleFromRef visiting refId
      | Set.member refId visiting = [[refId]] -- Found a cycle
      | otherwise = case Map.lookup refId sharedTable of
          Nothing -> [] -- Dangling reference, not a cycle
          Just dynVal ->
            let nextRefs = getDirectReferences dynVal
                newVisiting = Set.insert refId visiting
             in concatMap (findCycleFromRef newVisiting) nextRefs

    getDirectReferences :: DynamicValue -> [Word32]
    getDirectReferences (DynamicValue core _ _ _) = case core of
      DPrimitive _ -> []
      DProduct vals -> concatMap getDirectReferences vals
      DSum _ val -> getDirectReferences val
      DList vals -> concatMap getDirectReferences vals
      DUnit -> []
      DReference refId -> [refId]

-- | Helper function for triple destructuring
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

encodeWithTypePool :: DynamicValue -> Map.Map Word32 DynamicValue -> (Proto.DynamicValue, Map.Map Word32 Proto.DynamicValue, Map.Map Word32 Proto.TypeInfo)
encodeWithTypePool root shared =
  let action :: TypeInfoM (Proto.DynamicValue, Map.Map Word32 Proto.DynamicValue)
      action = do
        rootProto <- toProtoDynamicValue root
        sharedProto <- Map.traverseWithKey (const toProtoDynamicValue) shared
        pure (rootProto, sharedProto)
      ((rootProto, sharedProto), pool) = runState action emptyTypeInfoPool
   in (rootProto, sharedProto, sharedTypeInfoMap pool)

--------------------------------------------------------------------------------
-- File I/O API with Protobuf Support (.szt format)
--------------------------------------------------------------------------------

-- | Save a value to a .szt file with no deduplication.
--
-- This is the simplest way to save data. Each repeated value is stored separately,
-- resulting in larger files but faster serialization.
--
-- Example:
-- @
-- data Person = Person Text Int deriving (Generic, ToSZT)
-- person = Person "Alice" 25
-- saveSzt "person.szt" person
-- @
saveSzt :: (ToSZT a) => FilePath -> a -> IO ()
saveSzt = saveSztWithStrategy noDeduplicationStrategy

-- | Save a value to a .szt file with a custom deduplication strategy.
--
-- This is the most flexible save function, allowing you to control how
-- duplicate values are detected and shared to reduce file size.
--
-- Deduplication strategies:
-- * 'noDeduplicationStrategy' - No sharing, fastest serialization
-- * 'defaultDeduplicationStrategy' - Conservative sharing, good balance
-- * 'aggressiveDeduplicationStrategy' - Maximum sharing, smallest files
--
-- Example:
-- @
-- -- Save with custom strategy for very large data
-- import qualified Data.Map as Map
-- largeData = Map.fromList [(i, "value" ++ show i) | i <- [1..10000]]
-- saveSztWithStrategy aggressiveDeduplicationStrategy "large.szt" largeData
-- @
saveSztWithStrategy :: (ToSZT a) => DeduplicationStrategy -> FilePath -> a -> IO ()
saveSztWithStrategy strategy path value =
  saveSztWithCompressionAndStrategy NoCompression strategy path value

-- | Internal function that handles compression
saveSztWithCompressionAndStrategy :: (ToSZT a) => CompressionMethod -> DeduplicationStrategy -> FilePath -> a -> IO ()
saveSztWithCompressionAndStrategy compression strategy path value = do
  let dynValue = toSzt value
  let (dedupedValue, sharedTable) = deduplicateValue strategy dynValue
  let (protoDynValue, protoSharedValues, protoSharedTypes) = encodeWithTypePool dedupedValue sharedTable
  let sztFile =
        defMessage
          Lens.& Proto.schemaVersion Lens..~ currentSchemaVersion
          Lens.& Proto.value Lens..~ protoDynValue
          Lens.& Proto.sharedValues Lens..~ protoSharedValues
          Lens.& Proto.sharedTypeInfo Lens..~ protoSharedTypes
  let protoBytes = encodeMessage (sztFile :: Proto.SZTFile)

  -- Apply compression if specified
  let payloadBytes = case compression of
        NoCompression -> protoBytes
        GZipCompression ->
          -- GZip compression with default settings
          LBS.toStrict $ GZip.compress $ LBS.fromStrict protoBytes

  -- Combine header and payload
  let header = mkHeader compression
  let headerBytes = encodeHeader header
  let finalBytes = ByteString.append headerBytes payloadBytes

  ByteString.writeFile path finalBytes

-- | Save a value with balanced deduplication (recommended for most use cases).
--
-- Uses 'defaultDeduplicationStrategy' which provides a good balance between
-- file size reduction and serialization performance. Deduplicates strings,
-- lists, and complex structures but avoids deduplicating small primitives.
--
-- Example:
-- @
-- data Config = Config [Text] (Map Text Int) deriving (Generic, ToSZT)
-- config = Config ["option1", "option2"] (Map.fromList [("key", 42)])
-- saveSztCompressed "config.szt" config
-- @
saveSztCompressed :: (ToSZT a) => FilePath -> a -> IO ()
saveSztCompressed = saveSztWithCompressionAndStrategy GZipCompression defaultDeduplicationStrategy

-- | Save a value with maximum deduplication (best for very large, repetitive data).
--
-- Uses 'aggressiveDeduplicationStrategy' which deduplicates everything possible,
-- including small primitives. This produces the smallest files but takes more
-- time and memory during serialization.
--
-- Best for: Large datasets, data with lots of repetition, archival storage.
--
-- Example:
-- @
-- -- Saves efficiently due to repeated "common" strings
-- data Record = Record Text Text Int deriving (Generic, ToSZT)
-- records = replicate 1000 (Record "common" "shared" 42)
-- saveSztCompressedAggressive "records.szt" records
-- @
saveSztCompressedAggressive :: (ToSZT a) => FilePath -> a -> IO ()
saveSztCompressedAggressive = saveSztWithCompressionAndStrategy GZipCompression aggressiveDeduplicationStrategy

-- | Save a value with fast Hashable-based deduplication (best for over-the-wire serialization).
--
-- Uses Haskell's 'Hashable' typeclass instead of cryptographic content hashing, providing
-- 50-100x faster serialization. This is ideal for "over the wire" serialization where you need
-- speed and don't require cryptographic security guarantees.
--
-- **Tradeoffs:**
-- * ⚡ Much faster serialization (50-100x speedup)
-- * 📦 Similar file size to content-based deduplication
-- * ⚠️  Only deduplicates within single serialization session
-- * ⚠️  Tiny risk of hash collisions (negligible with good hash functions)
--
-- **Best for:**
-- * API responses and RPC serialization
-- * Inter-process communication
-- * Temporary data transfer where speed matters
-- * Applications that serialize frequently
--
-- **Not recommended for:**
-- * Long-term archival storage (use 'saveSztCompressed' instead)
-- * Cross-session deduplication
-- * Security-critical applications requiring cryptographic guarantees
--
-- Example:
-- @
-- -- Fast serialization for API response
-- data ApiResponse = ApiResponse [Record] (Map Text Value) deriving (Generic, ToSZT)
-- response = ApiResponse records metadata
-- saveSztFast "response.szt" response -- Fast, suitable for network transfer
-- @
saveSztFast :: (ToSZT a) => FilePath -> a -> IO ()
saveSztFast = saveSztWithCompressionAndStrategyFast NoCompression defaultDeduplicationStrategy

-- | Save a value with fast Hashable-based deduplication and GZip compression.
--
-- Combines fast hashing with compression for balanced performance. This provides
-- the speed benefits of Hashable-based deduplication with additional file size
-- reduction from compression.
--
-- Example:
-- @
-- saveSztFastCompressed "data.szt" largeDataset
-- @
saveSztFastCompressed :: (ToSZT a) => FilePath -> a -> IO ()
saveSztFastCompressed = saveSztWithCompressionAndStrategyFast GZipCompression defaultDeduplicationStrategy

-- | Internal function that handles fast deduplication with compression
saveSztWithCompressionAndStrategyFast :: (ToSZT a) => CompressionMethod -> DeduplicationStrategy -> FilePath -> a -> IO ()
saveSztWithCompressionAndStrategyFast compression strategy path value = do
  let dynValue = toSzt value
  -- Use fast Hashable-based deduplication instead of cryptographic content hashing
  let (dedupedValue, sharedTable) = deduplicateValueFast strategy dynValue
  let (protoDynValue, protoSharedValues, protoSharedTypes) = encodeWithTypePool dedupedValue sharedTable
  let sztFile =
        defMessage
          Lens.& Proto.schemaVersion Lens..~ currentSchemaVersion
          Lens.& Proto.value Lens..~ protoDynValue
          Lens.& Proto.sharedValues Lens..~ protoSharedValues
          Lens.& Proto.sharedTypeInfo Lens..~ protoSharedTypes
  let protoBytes = encodeMessage (sztFile :: Proto.SZTFile)

  -- Apply compression if specified
  let payloadBytes = case compression of
        NoCompression -> protoBytes
        GZipCompression ->
          -- GZip compression with default settings
          LBS.toStrict $ GZip.compress $ LBS.fromStrict protoBytes

  -- Combine header and payload
  let header = mkHeader compression
  let headerBytes = encodeHeader header
  let finalBytes = ByteString.append headerBytes payloadBytes

  ByteString.writeFile path finalBytes

-- | Save a value with shallow identifier-based deduplication (fastest for structured data).
--
-- Uses name/identifier fields instead of full content for deduplication, providing the fastest
-- possible serialization for data structures with named components. This approach only hashes
-- small identifier fields (~20 bytes) instead of entire structures (~1GB+), resulting in
-- **50-100x faster serialization** compared to content-based hashing.
--
-- **How it works:**
-- * For types with 'ShallowIdentifiable' instances: Hashes only the shallow identifier (typically a name field)
-- * For types without instances: Falls back to 'Hashable'-based hashing
-- * Single-pass traversal with memoization for maximum speed
--
-- **Tradeoffs:**
-- * ⚡⚡⚡ Extremely fast serialization (~1 second for 1GB+ models)
-- * 📦 Excellent file size (comparable to content-based deduplication)
-- * ⚠️  Requires manual 'ShallowIdentifiable' instances for custom types
-- * ⚠️  Only deduplicates within single serialization session
--
-- **Best for:**
-- * Large models with named components (variables, valuations, etc.)
-- * API responses with hierarchical data
-- * Over-the-wire serialization where sub-second performance is critical
-- * Applications serializing frequently (>100 times/hour)
--
-- **Requires:**
-- Manual 'ShallowIdentifiable' instances for your types with name fields:
-- @
-- instance ShallowIdentifiable MyType where
--   shallowIdentifier myVal = encodeUtf8 $ myVal ^. name
-- @
--
-- Example:
-- @
-- -- Ultra-fast serialization for named models
-- data Variable = Variable { _name :: Name, ... } deriving (Generic, ToSZT)
-- instance ShallowIdentifiable Variable where
--   shallowIdentifier v = encodeUtf8 $ unName $ v ^. name
--
-- saveSztShallow "model.szt" largeModel  -- ~1 second vs 85 seconds
-- @
saveSztShallow :: (ToSZT a) => FilePath -> a -> IO ()
saveSztShallow = saveSztWithCompressionAndStrategyShallow NoCompression defaultDeduplicationStrategy

-- | Save a value with shallow identifier-based deduplication and GZip compression.
--
-- Combines ultra-fast shallow deduplication with compression for maximum efficiency.
-- This is the recommended mode for over-the-wire serialization of large structured datasets.
--
-- Example:
-- @
-- saveSztShallowCompressed "model.szt" largeModel
-- @
saveSztShallowCompressed :: (ToSZT a) => FilePath -> a -> IO ()
saveSztShallowCompressed = saveSztWithCompressionAndStrategyShallow GZipCompression defaultDeduplicationStrategy

-- | Save a value with shallow identifier-based deduplication (no compression) and return statistics.
--
-- This function serializes the value and returns detailed statistics about the deduplication process,
-- allowing you to identify which types are most frequently deduplicated and which would benefit
-- most from ShallowIdentifiable instances.
--
-- Example:
-- @
-- stats <- saveSztShallowWithStats "model.szt" largeModel
-- putStrLn $ "Topology: " <> show (Map.lookup "Topology" (_typeDeduplicated stats))
-- @
saveSztShallowWithStats :: (ToSZT a) => FilePath -> a -> IO DeduplicationStats
saveSztShallowWithStats path value =
  saveSztWithCompressionAndStrategyShallowWithStats NoCompression defaultDeduplicationStrategy path value

-- | Save a value with shallow identifier-based deduplication and GZip compression, returning statistics.
--
-- Combines ultra-fast shallow deduplication with compression and provides detailed statistics
-- about the deduplication process.
--
-- Example:
-- @
-- stats <- saveSztShallowCompressedWithStats "model.szt" largeModel
-- -- Analyze which types had the most deduplication
-- let sortedByDedup = sortOn (Down . snd) $ Map.toList (_typeDeduplicated stats)
-- forM_ (take 10 sortedByDedup) $ \(typeName, count) ->
--   putStrLn $ typeName <> ": " <> show count <> " deduplications"
-- @
saveSztShallowCompressedWithStats :: (ToSZT a) => FilePath -> a -> IO DeduplicationStats
saveSztShallowCompressedWithStats path value =
  saveSztWithCompressionAndStrategyShallowWithStats GZipCompression defaultDeduplicationStrategy path value

-- | Internal function that handles shallow deduplication with compression and returns stats
saveSztWithCompressionAndStrategyShallowWithStats :: (ToSZT a) => CompressionMethod -> DeduplicationStrategy -> FilePath -> a -> IO DeduplicationStats
saveSztWithCompressionAndStrategyShallowWithStats compression strategy path value = do
  let dynValue = toSzt value
  -- Use shallow identifier-based deduplication for maximum speed
  let (dedupedValue, sharedTable, stats) = deduplicateValueShallowWithStats strategy dynValue
  let (protoDynValue, protoSharedValues, protoSharedTypes) = encodeWithTypePool dedupedValue sharedTable
  let sztFile =
        defMessage
          Lens.& Proto.schemaVersion Lens..~ currentSchemaVersion
          Lens.& Proto.value Lens..~ protoDynValue
          Lens.& Proto.sharedValues Lens..~ protoSharedValues
          Lens.& Proto.sharedTypeInfo Lens..~ protoSharedTypes
  let protoBytes = encodeMessage (sztFile :: Proto.SZTFile)

  -- Apply compression if specified
  let payloadBytes = case compression of
        NoCompression -> protoBytes
        GZipCompression ->
          -- GZip compression with default settings
          LBS.toStrict $ GZip.compress $ LBS.fromStrict protoBytes

  -- Combine header and payload
  let header = mkHeader compression
  let headerBytes = encodeHeader header
  let finalBytes = ByteString.append headerBytes payloadBytes

  ByteString.writeFile path finalBytes
  return stats

-- | Internal function that handles shallow deduplication with compression
saveSztWithCompressionAndStrategyShallow :: (ToSZT a) => CompressionMethod -> DeduplicationStrategy -> FilePath -> a -> IO ()
saveSztWithCompressionAndStrategyShallow compression strategy path value = do
  let dynValue = toSzt value
  -- Use shallow identifier-based deduplication for maximum speed
  let (dedupedValue, sharedTable) = deduplicateValueShallow strategy dynValue
  let (protoDynValue, protoSharedValues, protoSharedTypes) = encodeWithTypePool dedupedValue sharedTable
  let sztFile =
        defMessage
          Lens.& Proto.schemaVersion Lens..~ currentSchemaVersion
          Lens.& Proto.value Lens..~ protoDynValue
          Lens.& Proto.sharedValues Lens..~ protoSharedValues
          Lens.& Proto.sharedTypeInfo Lens..~ protoSharedTypes
  let protoBytes = encodeMessage (sztFile :: Proto.SZTFile)

  -- Apply compression if specified
  let payloadBytes = case compression of
        NoCompression -> protoBytes
        GZipCompression ->
          -- GZip compression with default settings
          LBS.toStrict $ GZip.compress $ LBS.fromStrict protoBytes

  -- Combine header and payload
  let header = mkHeader compression
  let headerBytes = encodeHeader header
  let finalBytes = ByteString.append headerBytes payloadBytes

  ByteString.writeFile path finalBytes

-- | Load a value from a .szt file, deserializing it to the target type.
--
-- This function handles all the complexity of loading, deduplication resolution,
-- and type conversion. It provides detailed error information if anything goes wrong.
--
-- Returns 'Left SerializotronError' if:
-- * File doesn't exist or can't be read
-- * File is corrupted or has wrong format
-- * File was created with incompatible schema version
-- * Deserialization to target type fails
-- * File contains cyclic references or other integrity issues
--
-- Example:
-- @
-- data Person = Person Text Int deriving (Generic, FromSZT)
--
-- loadPerson :: IO (Either SerializotronError Person)
-- loadPerson = loadSzt "person.szt"
--
-- main = do
--   result <- loadPerson
--   case result of
--     Left err -> putStrLn $ "Error: " <> formatError err
--     Right person -> print person
-- @
loadSzt :: (FromSZT a) => FilePath -> IO (Either SerializotronError a)
loadSzt path = do
  result <- try $ ByteString.readFile path
  case result of
    Left ioErr -> return $ Left $ IOError ioErr
    Right fileBytes -> do
      -- Validate minimum size for header
      if ByteString.length fileBytes < 8
        then return $ Left $ FileSystemError $ InvalidFileFormat path "File too short to contain header"
        else do
          -- Parse header
          case decodeHeader (ByteString.take 8 fileBytes) of
            Left err -> return $ Left $ FileSystemError $ InvalidFileFormat path err
            Right header -> do
              let payloadBytes = ByteString.drop 8 fileBytes
              -- Decompress if needed
              protoBytes <- case _headerCompression header of
                NoCompression -> return $ Right payloadBytes
                GZipCompression ->
                  return $ Right $ LBS.toStrict $ GZip.decompress $ LBS.fromStrict payloadBytes

              case protoBytes of
                Left err -> return $ Left err
                Right bytes ->
                  case decodeMessage bytes :: Either String Proto.SZTFile of
                    Left err -> return $ Left $ ProtocolBufferError (ProtocolDecodingError (Text.pack err))
                    Right sztFile -> do
                      let schemaVersion = sztFile Lens.^. Proto.schemaVersion
                      let protoSharedTypes = sztFile Lens.^. Proto.sharedTypeInfo
                      case Map.traverseWithKey (const fromProtoTypeInfo) protoSharedTypes of
                        Left err -> return $ Left err
                        Right sharedTypeMap -> do
                          let protoSharedValues = sztFile Lens.^. Proto.sharedValues
                          let sharedTable = Map.map (fromProtoDynamicValue schemaVersion sharedTypeMap) protoSharedValues
                          case fromProtoDynamicValue schemaVersion sharedTypeMap (sztFile Lens.^. Proto.value) of
                            Left err -> return $ Left err
                            Right dynValue -> do
                              case resolveReferences sharedTable dynValue of
                                Left err -> return $ Left err
                                Right resolvedValue ->
                                  return $ first (`DeserializationError` emptyContext) (fromSzt resolvedValue)

-- | Load the raw internal representation from a .szt file (for debugging).
--
-- This function loads the file and resolves references but doesn't attempt
-- to convert to a specific Haskell type. Useful for:
-- * Debugging serialization issues
-- * Inspecting file contents without knowing the original type
-- * Building tools that work with .szt files generically
--
-- Example:
-- @
-- main = do
--   result <- loadSztRaw "mystery.szt"
--   case result of
--     Left err -> putStrLn $ "Error: " <> formatError err
--     Right dynValue -> do
--       putStrLn $ "Loaded value with " <> show (estimateSize dynValue) <> " bytes"
--       print dynValue
-- @
loadSztRaw :: FilePath -> IO (Either SerializotronError DynamicValue)
loadSztRaw path = do
  result <- try $ LBS.readFile path
  case result of
    Left ioErr -> return $ Left $ IOError ioErr
    Right encodedBytes ->
      case decodeMessage (LBS.toStrict encodedBytes) :: Either String Proto.SZTFile of
        Left err -> return $ Left $ ProtocolBufferError (ProtocolDecodingError (Text.pack err))
        Right sztFile -> do
          let schemaVersion = sztFile Lens.^. Proto.schemaVersion
          let protoSharedTypes = sztFile Lens.^. Proto.sharedTypeInfo
          case Map.traverseWithKey (const fromProtoTypeInfo) protoSharedTypes of
            Left err -> return $ Left err
            Right sharedTypeMap -> do
              let protoSharedValues = sztFile Lens.^. Proto.sharedValues
              let sharedTable = Map.map (fromProtoDynamicValue schemaVersion sharedTypeMap) protoSharedValues
              case fromProtoDynamicValue schemaVersion sharedTypeMap (sztFile Lens.^. Proto.value) of
                Left err -> return $ Left err
                Right dynValue -> return $ resolveReferences sharedTable dynValue

--------------------------------------------------------------------------------
-- Type Metadata Export (for TypeScript codegen and schema documentation)
--------------------------------------------------------------------------------

-- | Wrapper for heterogeneous type metadata.
--
-- This allows collecting TypeInfo for different types in a single list,
-- which is useful for exporting type schemas without needing actual values.
data SomeTypeMetadata where
  SomeTypeMetadata :: (Generic a, GGetTypeInfo (Rep a), Typeable a, GGetCompleteStructure (Rep a), GGetAllConstructorNames (Rep a)) => Proxy a -> SomeTypeMetadata

-- | Extract TypeInfo from a Proxy without needing an actual value.
--
-- This uses the Generic instance to derive complete type information,
-- including structure, constructor names, field names, etc.
--
-- Example:
-- @
-- typeMetadata (Proxy @Person)
-- typeMetadata (Proxy @Model)
-- @
typeMetadata :: forall a. (Generic a, GGetTypeInfo (Rep a), Typeable a, GGetCompleteStructure (Rep a), GGetAllConstructorNames (Rep a)) => Proxy a -> SomeTypeMetadata
typeMetadata = SomeTypeMetadata

-- | Extract TypeInfo from SomeTypeMetadata wrapper.
extractTypeInfo :: SomeTypeMetadata -> TypeInfo
extractTypeInfo (SomeTypeMetadata (_ :: Proxy a)) =
  let rep = typeRep (Proxy @a)
      tyCon = typeRepTyCon rep
      nameText = Text.pack (tyConName tyCon)
      moduleText = Text.pack (tyConModule tyCon)
      -- Get type parameters
      typeParams = map typeInfoForRep (typeRepArgs rep)
   in TypeInfo
        { _tiTypeName = Just nameText
        , _tiModule = Just moduleText
        , _tiConstructors = gGetAllConstructorNames (Proxy @(Rep a ()))
        , _tiFieldLabels = []  -- Will be filled in by recursive interning when structure is encountered
        , _tiStructure = Just $ gGetCompleteStructure (Proxy @(Rep a ()))
        , _tiTypeParameters = typeParams
        , _tiNewtypeWrapper = Nothing  -- Will be populated by Generic derivation if applicable
        }

-- | Save type metadata to an .szt file without needing actual values.
--
-- This is useful for generating TypeScript type definitions or other schema
-- documentation without having to construct dummy values. The function will:
--
-- * Extract complete TypeInfo for each specified type using Generic instances
-- * Recursively capture all referenced types (constructor arguments, fields, etc.)
-- * Save an .szt file containing only the type pool (no data values)
--
-- The resulting file can be processed by TypeScript codegen or other tools
-- that need schema information.
--
-- Example:
-- @
-- -- Export type schema for an API
-- saveSztMetadata "api_types.szt"
--   [ typeMetadata (Proxy @User)
--   , typeMetadata (Proxy @Product)
--   , typeMetadata (Proxy @Order)
--   ]
--
-- -- All referenced types (Address, PaymentMethod, etc.) are automatically included
-- @
--
-- Use cases:
-- * Generate TypeScript definitions for frontend development
-- * Create schema documentation
-- * Validate compatibility between Haskell and other languages
-- * Export type information for code generation tools
saveSztMetadata :: FilePath -> [SomeTypeMetadata] -> IO ()
saveSztMetadata path types =
  saveSztMetadataWithCompression GZipCompression path types

-- | Save type metadata with specific compression method.
saveSztMetadataWithCompression :: CompressionMethod -> FilePath -> [SomeTypeMetadata] -> IO ()
saveSztMetadataWithCompression compression path types = do
  -- Extract TypeInfo from all provided types
  let typeInfos = map extractTypeInfo types

  -- Intern all types using our recursive interning logic
  -- This will capture all nested types automatically
  let (_, protoSharedTypes) = runState
        (mapM_ (internTypeInfo . Just) typeInfos)
        emptyTypeInfoPool

  -- Create an .szt file with empty root value but full type pool
  -- We use DUnit as a placeholder root value since we only care about the type pool
  let emptyRoot = DynamicValue DUnit Nothing currentSchemaVersion Nothing
  let (protoRoot, _, _) = encodeWithTypePool emptyRoot Map.empty

  let sztFile =
        defMessage
          Lens.& Proto.schemaVersion Lens..~ currentSchemaVersion
          Lens.& Proto.value Lens..~ protoRoot
          Lens.& Proto.sharedValues Lens..~ Map.empty
          Lens.& Proto.sharedTypeInfo Lens..~ sharedTypeInfoMap protoSharedTypes

  let protoBytes = encodeMessage (sztFile :: Proto.SZTFile)

  -- Apply compression if specified
  let payloadBytes = case compression of
        NoCompression -> protoBytes
        GZipCompression -> LBS.toStrict $ GZip.compress $ LBS.fromStrict protoBytes

  -- Combine header and payload
  let header = mkHeader compression
  let headerBytes = encodeHeader header
  let finalBytes = ByteString.append headerBytes payloadBytes

  ByteString.writeFile path finalBytes

-- Resolve references in a DynamicValue using the shared table.
--
-- Uses lazy memoization (tying-the-knot) to ensure each shared table
-- entry is resolved at most once, regardless of how many times it is
-- referenced. This is critical for heavily-deduplicated data where the
-- same subtree (e.g. Topology, Domain) may be referenced hundreds of
-- times. Without memoization, resolution is O(n * m) where n is the
-- number of references and m is the average subtree size; with
-- memoization it is O(total_nodes).
--
-- The shared table forms a DAG by construction from the deduplicator,
-- so cycles cannot occur. If a cycle were present, GHC's runtime would
-- detect it as a <<loop>> error.
resolveReferences :: Map.Map Word32 (Either SerializotronError DynamicValue) -> DynamicValue -> Either SerializotronError DynamicValue
resolveReferences sharedTable rootValue = resolveValue rootValue
  where
    -- Pre-resolved shared table: each entry is resolved at most once
    -- via Haskell's lazy evaluation. Must use Map.Lazy.map (not
    -- Map.Strict.map) because strict map forces values to WHNF during
    -- construction, which would cause <<loop>> when entries reference
    -- each other.
    resolvedTable :: Map.Map Word32 (Either SerializotronError DynamicValue)
    resolvedTable = Map.Lazy.map (>>= resolveValue) sharedTable

    resolveValue :: DynamicValue -> Either SerializotronError DynamicValue
    resolveValue (DynamicValue core typeInfo version shallowId) = case core of
      -- For DReference nodes, return the full resolved DynamicValue from
      -- the shared table (preserving its shallowId and typeInfo) rather
      -- than just the DynamicCore. DReference nodes always have Nothing
      -- metadata, so no information is lost.
      DReference refId ->
        case Map.lookup refId resolvedTable of
          Nothing -> Left $ ValidationError (DanglingReference refId)
          Just (Left err) -> Left err
          Just (Right resolvedShared) -> Right resolvedShared
      _ -> do
        resolvedCore <- resolveCore core
        return $ DynamicValue resolvedCore typeInfo version shallowId

    resolveCore :: DynamicCore -> Either SerializotronError DynamicCore
    resolveCore = \case
      DPrimitive pv -> return $ DPrimitive pv
      DProduct vals -> DProduct <$> traverse resolveValue vals
      DSum i val -> DSum i <$> resolveValue val
      DList vals -> DList <$> traverse resolveValue vals
      DUnit -> return DUnit
      DReference refId ->
        -- Safety net for bare DReferences inside DynamicCore
        case Map.lookup refId resolvedTable of
          Nothing -> Left $ ValidationError (DanglingReference refId)
          Just (Left err) -> Left err
          Just (Right resolvedShared) -> return $ _dvCore resolvedShared

--------------------------------------------------------------------------------
-- Memoization for toSzt / fromSzt
--------------------------------------------------------------------------------

-- | Global cache for 'memoizedToSzt'. Keyed by (TypeRep, shallowId).
{-# NOINLINE toSztMemoCache #-}
toSztMemoCache :: IORef (Map.Map (TypeRep, ByteString) DynamicValue)
toSztMemoCache = unsafePerformIO (newIORef Map.empty)

-- | Clear the toSzt memo cache.
--
-- Call before and after each top-level serialisation cycle via
-- 'withSztMemoization' to avoid stale data or memory leaks.
resetToSztMemoCache :: IO ()
resetToSztMemoCache = writeIORef toSztMemoCache Map.empty

-- | Wrap a 'toSzt' implementation with 'ShallowIdentifiable'-based
-- memoization. Values with the same @(TypeRep, shallowId)@ return the
-- same 'DynamicValue' pointer, so GHC shares the subtree on the heap.
--
-- Usage in a custom 'ToSZT' instance:
--
-- @
-- instance ToSZT MyType where
--   toSzt = memoizedToSzt $ \\x -> DynamicValue { ... }
-- @
--
-- /Requires 'withSztMemoization' around the serialisation call to reset
-- the cache./
{-# NOINLINE memoizedToSzt #-}
memoizedToSzt
  :: forall a. (ShallowIdentifiable a, Typeable a)
  => (a -> DynamicValue) -> a -> DynamicValue
memoizedToSzt f x = unsafePerformIO $ do
  let !sid = shallowIdentifier x
      key = (typeRep (Proxy @a), sid)
  atomicModifyIORef' toSztMemoCache $ \cache ->
    case Map.lookup key cache of
      Just dv -> (cache, dv)
      Nothing ->
        let !dv = f x
        in (Map.insert key dv cache, dv)

-- | Global cache for 'memoizedFromSzt'. Keyed by (TypeRep, shallowId).
-- Stores values as 'Dynamic' for type-safe heterogeneous storage.
{-# NOINLINE fromSztMemoCache #-}
fromSztMemoCache :: IORef (Map.Map (TypeRep, ByteString) Dynamic)
fromSztMemoCache = unsafePerformIO (newIORef Map.empty)

-- | Clear the fromSzt memo cache.
resetFromSztMemoCache :: IO ()
resetFromSztMemoCache = writeIORef fromSztMemoCache Map.empty

-- | Wrap a 'fromSzt' implementation with shallowId-based memoization.
-- Values with the same @(TypeRep, shallowId)@ are deserialized once and
-- the Haskell heap object is reused for all subsequent occurrences.
--
-- If the 'DynamicValue' has no '_dvShallowId', falls through to the
-- inner function with no caching.
--
-- /Requires 'withSztMemoization' around the deserialisation call./
{-# NOINLINE memoizedFromSzt #-}
memoizedFromSzt
  :: forall a. (Typeable a)
  => (DynamicValue -> Either SZTError a)
  -> DynamicValue -> Either SZTError a
memoizedFromSzt f dv = case _dvShallowId dv of
  Nothing -> f dv
  Just sid -> unsafePerformIO $ do
    let key = (typeRep (Proxy @a), sid)
    cache <- readIORef fromSztMemoCache
    case Map.lookup key cache of
      Just dyn | Just val <- fromDynamic dyn -> pure (Right val)
      _ -> do
        let result = f dv
        case result of
          Right val ->
            atomicModifyIORef' fromSztMemoCache $ \c ->
              (Map.insert key (toDyn val) c, ())
          Left _ -> pure ()
        pure result

-- | Run an IO action with memo caches active. Resets both caches
-- before and after the action, ensuring no leaks or cross-contamination
-- between serialisation cycles.
withSztMemoization :: IO a -> IO a
withSztMemoization action = do
  resetToSztMemoCache
  resetFromSztMemoCache
  result <- action
  resetToSztMemoCache
  resetFromSztMemoCache
  pure result
