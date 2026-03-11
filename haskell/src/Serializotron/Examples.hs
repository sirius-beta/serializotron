{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Serializotron.Examples where

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
import GHC.Generics
import Serializotron
import Serializotron.Instances ()

--------------------------------------------------------------------------------
-- Simple Types
--------------------------------------------------------------------------------

-- | Basic product type
data Point = Point Int Int
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

-- | Simple sum type
data Color = Red | Green | Blue | RGB Int Int Int
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

-- | Maybe-like type
data Optional a = None | Some a
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

-- | Either-like type
data Result e a = Error e | Success a
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

--------------------------------------------------------------------------------
-- Recursive Types
--------------------------------------------------------------------------------

-- | Binary tree
data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

-- | Linked list
data List a = Nil | Cons a (List a)
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

-- | Rose tree (variable branching)
data RoseTree a = RoseNode a [RoseTree a]
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

--------------------------------------------------------------------------------
-- Complex Product Types
--------------------------------------------------------------------------------

-- | Person record
data Person = Person
  { personName :: Text,
    personAge :: Int,
    personEmail :: Text,
    personActive :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

-- | Address with optional fields
data Address = Address
  { addressStreet :: Text,
    addressCity :: Text,
    addressState :: Optional Text,
    addressZip :: Text,
    addressCountry :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

-- | Large record for testing deduplication
data Employee = Employee
  { empId :: Int,
    empPerson :: Person,
    empAddress :: Address,
    empDepartment :: Text,
    empSalary :: Double,
    empManager :: Optional Int -- Manager's employee ID
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

--------------------------------------------------------------------------------
-- Nested Sum Types
--------------------------------------------------------------------------------

-- | Expression language
data Expr
  = Literal Int
  | Variable Text
  | Add Expr Expr
  | Multiply Expr Expr
  | IfZero Expr Expr Expr
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

-- | JSON-like structure
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString Text
  | JsonArray [JsonValue]
  | JsonObject [(Text, JsonValue)]
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

instance ToJSON JsonValue where
  toJSON JsonNull = Aeson.Null
  toJSON (JsonBool b) = Aeson.Bool b
  toJSON (JsonNumber n) = toJSON n
  toJSON (JsonString t) = toJSON t
  toJSON (JsonArray xs) = toJSON (map toJSON xs)
  toJSON (JsonObject fields) =
    Aeson.object [AesonKey.fromText key Aeson..= toJSON value | (key, value) <- fields]

--------------------------------------------------------------------------------
-- Enterprise Dataset Types
--------------------------------------------------------------------------------

data ProjectStatus
  = Planned
  | Active
  | OnHold
  | Completed
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

data Project = Project
  { projectCode :: Text,
    projectName :: Text,
    projectSummary :: Text,
    projectLeadId :: Int,
    projectTeamIds :: [Int],
    projectBudget :: Double,
    projectStatus :: ProjectStatus,
    projectTags :: [Text],
    projectMilestones :: [Text]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

data Department = Department
  { deptName :: Text,
    deptManagerId :: Int,
    deptEmployeeIds :: [Int],
    deptProjects :: [Project],
    deptQuarterlyBudget :: Double,
    deptObjectives :: [Text]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

data CompanySnapshot = CompanySnapshot
  { snapshotCompany :: Text,
    snapshotDate :: UTCTime,
    snapshotEmployees :: [Employee],
    snapshotDepartments :: [Department],
    snapshotCompanyWideAnnouncements :: [Text],
    snapshotPolicies :: Document,
    snapshotOffices :: [Address]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

--------------------------------------------------------------------------------
-- Types with Sharing Potential (for deduplication testing)
--------------------------------------------------------------------------------

-- | Document with potentially shared content
data Document = Document
  { docTitle :: Text,
    docAuthor :: Person,
    docSections :: [Section]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

data Section = Section
  { sectionTitle :: Text,
    sectionContent :: Text,
    sectionSubsections :: [Section]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

-- | Graph structure (good for testing cycles)
data Graph a = Graph
  { graphNodes :: [Node a],
    graphEdges :: [(Int, Int)] -- Node indices
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

data Node a = Node
  { nodeId :: Int,
    nodeData :: a
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT, ToJSON)

--------------------------------------------------------------------------------
-- Example Data
--------------------------------------------------------------------------------

-- | Simple examples
examplePoint :: Point
examplePoint = Point 10 20

exampleColor :: Color
exampleColor = RGB 255 128 0

exampleTree :: Tree Int
exampleTree =
  Branch
    1
    (Branch 2 Leaf (Branch 3 Leaf Leaf))
    (Branch 4 Leaf Leaf)

-- | Person examples
johnDoe :: Person
johnDoe = Person "John Doe" 30 "john@example.com" True

janeSmith :: Person
janeSmith = Person "Jane Smith" 28 "jane@example.com" True

-- | Address examples
address1 :: Address
address1 = Address "123 Main St" "Springfield" (Some "IL") "62701" "USA"

address2 :: Address
address2 = Address "456 Oak Ave" "Springfield" (Some "IL") "62702" "USA"

-- | Employee examples (demonstrate sharing)
employee1 :: Employee
employee1 = Employee 1001 johnDoe address1 "Engineering" 75000.0 (Some 2001)

employee2 :: Employee
employee2 = Employee 1002 janeSmith address2 "Engineering" 80000.0 (Some 2001)

-- | Expression examples
exampleExpr :: Expr
exampleExpr = Add (Literal 5) (Multiply (Variable "x") (Literal 3))

complexExpr :: Expr
complexExpr =
  IfZero
    (Add (Variable "a") (Literal (-10)))
    (Literal 0)
    (Multiply (Variable "a") (Variable "b"))

-- | JSON examples
exampleJson :: JsonValue
exampleJson =
  JsonObject
    [ ("name", JsonString "John"),
      ("age", JsonNumber 30),
      ("active", JsonBool True),
      ("scores", JsonArray [JsonNumber 85, JsonNumber 92, JsonNumber 78]),
      ( "address",
        JsonObject
          [ ("street", JsonString "123 Main St"),
            ("city", JsonString "Springfield")
          ]
      )
    ]

-- | Document with shared sections (good for deduplication testing)
sharedSection :: Section
sharedSection =
  Section
    "Common Disclaimer"
    "This document is confidential and proprietary."
    []

exampleDoc :: Document
exampleDoc =
  Document
    "Company Handbook"
    johnDoe
    [ Section "Introduction" "Welcome to the company" [sharedSection],
      Section "Policies" "Company policies and procedures" [sharedSection],
      Section "Benefits" "Employee benefits information" [sharedSection]
    ]

-- | Graph example (potential for cycles)
exampleGraph :: Graph Text
exampleGraph =
  Graph
    [ Node 0 "Start",
      Node 1 "Middle",
      Node 2 "End"
    ]
    [(0, 1), (1, 2), (2, 0)] -- Creates a cycle

enterpriseSnapshot :: CompanySnapshot
enterpriseSnapshot =
  CompanySnapshot
    { snapshotCompany = "Serializotron Analytics Group",
      snapshotDate = UTCTime (fromGregorian 2025 9 1) (timeOfDayToTime (TimeOfDay 9 0 0)),
      snapshotEmployees = allEmployees,
      snapshotDepartments = departments,
      snapshotCompanyWideAnnouncements =
        [ "Q4 planning cadence begins next week.",
          "Hybrid work travel stipend renewed for 2025.",
          "Mandatory security training refresh due by October 15th."
        ],
      snapshotPolicies = exampleDoc,
      snapshotOffices = officeLocations
    }
  where
    departmentsConfig :: [(Text, Int, Int, [Text], Optional Text, Text)]
    departmentsConfig =
      [ ("Platform Engineering", 3001, 64, ["San Francisco", "Oakland", "San Jose"], Some "CA", "9410"),
        ("Data Science", 4001, 48, ["New York", "Brooklyn", "Jersey City"], Some "NY", "1001"),
        ("Customer Success", 5001, 36, ["Austin", "Dallas", "Houston"], Some "TX", "7330"),
        ("Product Management", 6001, 32, ["Seattle", "Bellevue"], Some "WA", "9810"),
        ("Research & Prototyping", 7001, 28, ["Boston", "Cambridge"], Some "MA", "0213")
      ]

    buildDepartment (deptName, startId, count, cities, stateOpt, zipStem) =
      (deptName, deptEmployees, deptRecord)
      where
        deptEmployees =
          [ makeEmployee idx empId
          | (idx, empId) <- zip [0 ..] [startId .. startId + count - 1]
          ]

        managerId = startId
        employeeIds = map empId deptEmployees

        deptRecord =
          Department
            { deptName = deptName,
              deptManagerId = managerId,
              deptEmployeeIds = employeeIds,
              deptProjects = buildProjects deptName employeeIds,
              deptQuarterlyBudget = 2000000 + fromIntegral count * 45000,
              deptObjectives =
                [ deptName <> " OKR alignment",
                  "Improve onboarding time by " <> Text.pack (show (15 + count `div` 4)) <> "%",
                  "Launch knowledge base refresh"
                ]
            }

        makeEmployee idx empId =
          Employee
            { empId = empId,
              empPerson =
                Person
                  { personName = fullName,
                    personAge = 24 + (idx * 3 + empId) `mod` 23,
                    personEmail = Text.toLower (Text.replace " " "." fullName) <> "@serializotron.example",
                    personActive = idx `mod` 9 /= 0
                  },
              empAddress =
                Address
                  { addressStreet = Text.pack (show (120 + idx)) <> " " <> deptName <> " Way",
                    addressCity = cities !! (idx `mod` length cities),
                    addressState = stateOpt,
                    addressZip = zipStem <> showDigit idx,
                    addressCountry = "USA"
                  },
              empDepartment = deptName,
              empSalary = 85000 + fromIntegral ((idx * 3500 + empId) `mod` 65000),
              empManager = if idx == 0 then None else Some managerId
            }
          where
            fullName = firstName idx <> " " <> lastName (idx + empId)

    firstNames =
      ["Alex", "Blair", "Casey", "Dana", "Evan", "Frankie", "Georgie", "Harper", "Indy", "Jules", "Kai", "Logan", "Morgan", "Nico", "Oakley", "Parker", "Quinn", "Riley", "Sydney", "Taylor"]
    lastNames =
      ["Nguyen", "Patel", "Garcia", "Hernandez", "Okafor", "Ibrahim", "Chen", "Kowalski", "Sato", "Dubois", "Fischer", "Larsen", "O'Neill", "Ibarra", "Silva", "Petrov", "Hansen", "Yoon", "Ueda", "Zhou"]

    firstName idx = firstNames !! (idx `mod` length firstNames)
    lastName idx = lastNames !! (idx `mod` length lastNames)

    buildProjects deptName employeeIds =
      zipWith makeProject [0 ..] (chunk 12 employeeIds)
      where
        makeProject projectIdx teamIds =
          let leadId = head teamIds
              statusCycle = [Active, Planned, OnHold, Active, Completed]
              statusValue = statusCycle !! (projectIdx `mod` length statusCycle)
           in Project
                { projectCode = deptCode <> Text.pack (show (100 + projectIdx)),
                  projectName = deptName <> " Initiative " <> Text.pack (show (projectIdx + 1)),
                  projectSummary =
                    "Quarterly delivery stream focusing on "
                      <> Text.toLower deptName
                      <> " outcomes and platform readiness.",
                  projectLeadId = leadId,
                  projectTeamIds = teamIds,
                  projectBudget = 450000 + fromIntegral (length teamIds) * 52500,
                  projectStatus = statusValue,
                  projectTags =
                    ["modernization", Text.toLower deptName, "2025 roadmap"],
                  projectMilestones =
                    [ "Design sign-off (" <> quarterLabel projectIdx <> ")",
                      "Pilot deployment (" <> quarterLabel (projectIdx + 1) <> ")",
                      "General availability (" <> quarterLabel (projectIdx + 2) <> ")"
                    ]
                }

        deptCode = Text.concat (map (Text.take 1) (Text.words deptName))

    quarterLabel n =
      let year = 2025 + n `div` 4
          quarter = (n `mod` 4) + 1
       in "Q" <> Text.pack (show quarter) <> " " <> Text.pack (show year)

    chunk _ [] = []
    chunk size xs =
      let (h, t) = splitAt size xs
       in if null h then [] else h : chunk size t

    showDigit idx = Text.singleton (Char.intToDigit ((idx `mod` 9) + 1))

    builtDepartments = map buildDepartment departmentsConfig
    departments = [dept | (_, _, dept) <- builtDepartments]
    allEmployees = concat [emps | (_, emps, _) <- builtDepartments]

    officeLocations =
      [ Address "1400 Market St" "San Francisco" (Some "CA") "94103" "USA",
        Address "55 Water St" "New York" (Some "NY") "10041" "USA",
        Address "500 West 2nd St" "Austin" (Some "TX") "78701" "USA"
      ]

--------------------------------------------------------------------------------
-- Utility Functions for REPL Testing
--------------------------------------------------------------------------------

-- | Test roundtrip serialization
testRoundtrip :: (ToSZT a, FromSZT a, Show a, Eq a) => a -> IO Bool
testRoundtrip value = do
  let dynValue = toSzt value
  case fromSzt dynValue of
    Left err -> do
      putStrLn $ "Deserialization failed: " ++ show err
      return False
    Right result -> do
      let success = value == result
      if success
        then putStrLn "Roundtrip successful"
        else putStrLn $ "Roundtrip failed:\nOriginal: " ++ show value ++ "\nResult: " ++ show result
      return success

-- | Test file I/O roundtrip
testFileRoundtrip :: (ToSZT a, FromSZT a, Show a, Eq a) => FilePath -> a -> IO Bool
testFileRoundtrip path value = do
  saveSzt path value
  result <- loadSzt path
  case result of
    Left err -> do
      putStrLn $ "File load failed: " ++ show err
      return False
    Right loaded -> do
      let success = value == loaded
      if success
        then putStrLn "File roundtrip successful"
        else putStrLn $ "File roundtrip failed:\nOriginal: " ++ show value ++ "\nLoaded: " ++ show loaded
      return success

-- | Test with deduplication
testWithDeduplication :: (ToSZT a) => a -> IO ()
testWithDeduplication value = do
  let dynValue = toSzt value
  let (dedupedValue, sharedTable) = deduplicateValue defaultDeduplicationStrategy dynValue
  putStrLn $ "Original size estimate: " ++ show (estimateSize dynValue) ++ " bytes"
  putStrLn $ "Deduplicated size estimate: " ++ show (estimateSize dedupedValue) ++ " bytes"
  putStrLn $ "Shared values: " ++ show (length sharedTable)

-- | Run fsck on a file
testFsck :: FilePath -> IO ()
testFsck path = do
  result <- fsckSzt path
  putStrLn $ "Fsck result: " ++ show result

--------------------------------------------------------------------------------
-- Quick Test Suite
--------------------------------------------------------------------------------

-- | Run all basic tests
runAllTests :: IO ()
runAllTests = do
  putStrLn "=== Running Basic Roundtrip Tests ==="
  _ <- testRoundtrip examplePoint
  _ <- testRoundtrip exampleColor
  _ <- testRoundtrip johnDoe
  _ <- testRoundtrip employee1
  _ <- testRoundtrip exampleTree
  _ <- testRoundtrip exampleExpr
  _ <- testRoundtrip exampleJson

  putStrLn "\n=== Testing Deduplication ==="
  testWithDeduplication exampleDoc
  testWithDeduplication [employee1, employee2] -- Should share Person data
  putStrLn "\n=== Testing File I/O ==="
  _ <- testFileRoundtrip "/tmp/test.szt" exampleDoc
  testFsck "/tmp/test.szt"

  putStrLn "\nAll tests complete!"
