{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}

-- | Example demonstrating shallow serialization with nested hash sets
module Serializotron.ShallowExample where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.HashSet (HashSet)
import Data.HashSet qualified as HS
import Data.Hashable (Hashable, hash)
import Data.List qualified as List
import Data.Text (Text, pack)
import GHC.Generics
import Serializotron
import Serializotron.Instances

-- | A cached identifier that serves as a precomputed hash/fingerprint for a value.
newtype CachedId = CachedId {unCachedId :: ByteString}
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (Hashable, ToSZT, FromSZT)

-- | A topology represents a collection of named regions (like open sets). This
-- will be shared across many Domain values.
data Topology = Topology
  { topologyCachedId :: CachedId, -- Precomputed identifier for fast comparison
    topologyName :: Text, -- Name of the topology
    topologyRegions :: HashSet Region -- The nested hash set structure
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToSZT, FromSZT)

-- | A region is a named collection of points.
data Region = Region
  { regionName :: Text,
    regionPoints :: HashSet Text -- Points in this region
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToSZT, FromSZT)

-- | Generate a cached identifier for a topology based on its structure. This walks
-- the whole structure so it's expensive - ideally do this just once.
generateTopologyCachedId :: Text -> HashSet Region -> CachedId
generateTopologyCachedId name regions =
  CachedId $
    BS.pack $
      "Topology:"
        <> show name
        <> ":"
        <> (show . hash . List.sort . HS.toList $ regions)

-- | Smart constructor for Topology that automatically generates the CachedId.
mkTopology :: Text -> HashSet Region -> Topology
mkTopology name regions =
  Topology
    { topologyCachedId = generateTopologyCachedId name regions,
      topologyName = name,
      topologyRegions = regions
    }

-- | Shallow identifier for fast deduplication during serialization. Uses the precomputed
-- CachedId to avoid deep hashing of topology structure.
instance ShallowIdentifiable Topology where
  shallowIdentifier t = unCachedId (topologyCachedId t)

-- | A domain represents a subset in a topological space; many domains can share the same underlying topology.
data Domain = Domain
  { domainCachedId :: CachedId, -- Precomputed identifier
    domainTopology :: Topology, -- The shared topology (this is what we want to deduplicate!)
    domainSubset :: HashSet Text -- Which points from the topology are in this domain
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable, ToSZT, FromSZT)

-- | Generate a cached identifier for a domain.
generateDomainCachedId :: Topology -> HashSet Text -> CachedId
generateDomainCachedId topology subset =
  CachedId $
    "Domain:"
      <> unCachedId (topologyCachedId topology)
      <> ":"
      <> (BS.pack . show . hash . List.sort . HS.toList $ subset)

-- | Smart constructor for Domain that automatically generates the CachedId
mkDomain :: Topology -> HashSet Text -> Domain
mkDomain topology subset =
  Domain
    { domainCachedId = generateDomainCachedId topology subset,
      domainTopology = topology,
      domainSubset = subset
    }

-- | Shallow identifier for fast deduplication during serialization.
instance ShallowIdentifiable Domain where
  shallowIdentifier d = unCachedId (domainCachedId d)

-- | A model contains many domains, often sharing the same topology
data Model = Model
  { modelName :: Text,
    modelDomains :: [Domain], -- Many domains, potentially sharing the same topology
    modelMetadata :: ModelMetadata
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data ModelMetadata = ModelMetadata
  { metaCreatedBy :: Text,
    metaTags :: [Text],
    metaVersion :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

-- | Create a simple region
mkRegion :: Text -> [Text] -> Region
mkRegion name points = Region name (HS.fromList points)

-- | Example topology: A simple 2D grid topology
exampleGridTopology :: Topology
exampleGridTopology =
  mkTopology "Grid2D" $
    HS.fromList
      [ mkRegion "NorthWest" ["(0,0)", "(0,1)", "(1,0)", "(1,1)"],
        mkRegion "NorthEast" ["(2,0)", "(2,1)", "(3,0)", "(3,1)"],
        mkRegion "SouthWest" ["(0,2)", "(0,3)", "(1,2)", "(1,3)"],
        mkRegion "SouthEast" ["(2,2)", "(2,3)", "(3,2)", "(3,3)"],
        mkRegion "Center" ["(1,1)", "(1,2)", "(2,1)", "(2,2)"]
      ]

-- | Example topology: A network topology
exampleNetworkTopology :: Topology
exampleNetworkTopology =
  mkTopology "Network" $
    HS.fromList
      [ mkRegion "CoreRouters" ["R1", "R2", "R3"],
        mkRegion "EdgeSwitches" ["S1", "S2", "S3", "S4"],
        mkRegion "Servers" ["SRV1", "SRV2", "SRV3", "SRV4", "SRV5"],
        mkRegion "Workstations" ["WS1", "WS2", "WS3", "WS4", "WS5", "WS6", "WS7", "WS8"]
      ]

-- | Create a model with many domains sharing the same topology.
exampleModelWithSharedTopology :: Model
exampleModelWithSharedTopology =
  Model
    { modelName = "Spatial Analysis Model",
      modelDomains = createDomainsWithSharedTopology exampleGridTopology 1000,
      modelMetadata = ModelMetadata "Example Generator" ["spatial", "grid", "test"] 1
    }
  where
    -- Create N domains, all using the same topology but with different subsets.
    createDomainsWithSharedTopology :: Topology -> Int -> [Domain]
    createDomainsWithSharedTopology topology n =
      [ mkDomain topology (HS.fromList [point i, point (i + 1), point (i + 2)])
      | i <- [0 .. n - 1]
      ]

    -- Generate point names.
    point :: Int -> Text
    point i =
      "("
        <> pack (show (i `mod` 4))
        <> ","
        <> pack (show ((i `div` 4) `mod` 4))
        <> ")"

-- | Create a model with network domains.
exampleNetworkModel :: Model
exampleNetworkModel =
  Model
    { modelName = "Network Topology Model",
      modelDomains = networkDomains,
      modelMetadata = ModelMetadata "Network Admin" ["network", "infrastructure"] 1
    }
  where
    -- Create multiple network domains that share the same topology
    networkDomains =
      [ mkDomain exampleNetworkTopology (HS.fromList ["R1", "S1", "SRV1"]),
        mkDomain exampleNetworkTopology (HS.fromList ["R2", "S2", "SRV2"]),
        mkDomain exampleNetworkTopology (HS.fromList ["R3", "S3", "SRV3"]),
        mkDomain exampleNetworkTopology (HS.fromList ["R1", "R2", "R3"]), -- All core routers
        mkDomain exampleNetworkTopology (HS.fromList ["S1", "S2", "S3", "S4"]) -- All switches
      ]

-- | Create a large model with extreme duplication. Thousands of domains all sharing the same topology value.
exampleLargeModelWithDuplication :: Model
exampleLargeModelWithDuplication =
  Model
    { modelName = "Large Scale Spatial Model",
      modelDomains = createManyDomains 5000,
      modelMetadata = ModelMetadata "Batch Generator" ["large-scale", "benchmark"] 2
    }
  where
    createManyDomains :: Int -> [Domain]
    createManyDomains n =
      [ mkDomain exampleGridTopology (HS.fromList [generatePoint i])
      | i <- [0 .. n - 1]
      ]

    generatePoint :: Int -> Text
    generatePoint i =
      "("
        <> pack (show (i `mod` 4))
        <> ","
        <> pack (show ((i `div` 4) `mod` 4))
        <> ")"
