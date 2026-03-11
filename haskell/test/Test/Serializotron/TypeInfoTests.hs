{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests for Generic TypeInfo extraction, particularly the product type instance
module Test.Serializotron.TypeInfoTests where

import Control.Lens ((^.))
import Data.Text (Text)
import GHC.Generics
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Serializotron
import Serializotron.Instances ()

-- For Maybe and Either instances

--------------------------------------------------------------------------------
-- Test Data Types
--------------------------------------------------------------------------------

-- Simple product type (Text :*: Int)
data Person = Person Text Int
  deriving stock (Generic, Eq, Show)

instance ToSZT Person

instance FromSZT Person

-- Product with three fields
data Employee = Employee
  { empName :: Text,
    empAge :: Int,
    empSalary :: Double
  }
  deriving stock (Generic, Eq, Show)

instance ToSZT Employee

instance FromSZT Employee

-- Single field record
newtype Solo = Solo Int
  deriving stock (Generic, Eq, Show)

instance ToSZT Solo

instance FromSZT Solo

-- Empty constructor
data Unit = Unit
  deriving stock (Generic, Eq, Show)

instance ToSZT Unit

instance FromSZT Unit

-- Sum type with products
data Shape
  = Circle Double
  | Rectangle Double Double
  | Triangle Double Double Double
  deriving stock (Generic, Eq, Show)

instance ToSZT Shape

instance FromSZT Shape

-- Simple sum type for testing constructor completeness
data Color = Red | Green | Blue
  deriving stock (Generic, Eq, Show)

instance ToSZT Color

instance FromSZT Color

-- Newtype wrappers for testing newtype wrapper tracking
newtype UserId = UserId Int
  deriving stock (Generic, Eq, Show)

instance ToSZT UserId

instance FromSZT UserId

newtype UserName = UserName Text
  deriving stock (Generic, Eq, Show)

instance ToSZT UserName

instance FromSZT UserName

-- Chained newtypes: WrapperId wraps UserId which wraps Int
newtype WrapperId = WrapperId UserId
  deriving stock (Generic, Eq, Show)

instance ToSZT WrapperId

instance FromSZT WrapperId

-- Product type containing newtypes
data User = User
  { userId :: UserId,
    userName :: UserName
  }
  deriving stock (Generic, Eq, Show)

instance ToSZT User

instance FromSZT User

--------------------------------------------------------------------------------
-- Generators
--------------------------------------------------------------------------------

genText :: Gen Text
genText = Gen.text (Range.linear 0 50) Gen.unicode

genPerson :: Gen Person
genPerson = Person <$> genText <*> Gen.int (Range.linear 0 120)

genEmployee :: Gen Employee
genEmployee =
  Employee
    <$> genText
    <*> Gen.int (Range.linear 18 100)
    <*> Gen.double (Range.exponentialFloat 0 1000000)

genSolo :: Gen Solo
genSolo = Solo <$> Gen.int (Range.linear minBound maxBound)

genUnit :: Gen Unit
genUnit = pure Unit

genShape :: Gen Shape
genShape =
  Gen.choice
    [ Circle <$> Gen.double (Range.exponentialFloat 0.1 100),
      Rectangle
        <$> Gen.double (Range.exponentialFloat 0.1 100)
        <*> Gen.double (Range.exponentialFloat 0.1 100),
      Triangle
        <$> Gen.double (Range.exponentialFloat 0.1 100)
        <*> Gen.double (Range.exponentialFloat 0.1 100)
        <*> Gen.double (Range.exponentialFloat 0.1 100)
    ]

genColor :: Gen Color
genColor = Gen.choice [pure Red, pure Green, pure Blue]

genUserId :: Gen UserId
genUserId = UserId <$> Gen.int (Range.linear 1 100000)

genUserName :: Gen UserName
genUserName = UserName <$> genText

genWrapperId :: Gen WrapperId
genWrapperId = WrapperId <$> genUserId

genUser :: Gen User
genUser = User <$> genUserId <*> genUserName

--------------------------------------------------------------------------------
-- TypeInfo Extraction Tests
--------------------------------------------------------------------------------

-- | Test that Person (with Text :*: Int product) extracts correct type info
prop_person_typeinfo :: Property
prop_person_typeinfo = property $ do
  person <- forAll genPerson
  let dynVal = toSzt person
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      -- The top-level type should have the Person type name
      annotate $ "TypeInfo: " ++ show typeInfo
      typeInfo ^. tiTypeName === Just "Person"
      typeInfo ^. tiModule === Just "Test.Serializotron.TypeInfoTests"
      typeInfo ^. tiConstructors === ["Person"]

      -- Check that the structure shows a product
      case typeInfo ^. tiStructure of
        Just (TSProduct components) -> do
          annotate $ "Product has " ++ show (length components) ++ " components"
          -- Person has one constructor with a product of fields
          success
        Just other -> do
          annotate $ "Expected TSProduct, got: " ++ show other
          failure
        Nothing -> do
          annotate "No type structure found"
          failure
    Nothing -> do
      annotate "No type info found"
      failure

-- | Test Employee with three fields
prop_employee_typeinfo :: Property
prop_employee_typeinfo = property $ do
  employee <- forAll genEmployee
  let dynVal = toSzt employee
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      typeInfo ^. tiTypeName === Just "Employee"
      typeInfo ^. tiConstructors === ["Employee"]

      -- Verify the structure
      case typeInfo ^. tiStructure of
        Just (TSProduct _) -> success
        _ -> failure
    Nothing -> failure

-- | Test Solo with single field
prop_solo_typeinfo :: Property
prop_solo_typeinfo = property $ do
  solo <- forAll genSolo
  let dynVal = toSzt solo
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      typeInfo ^. tiTypeName === Just "Solo"
      typeInfo ^. tiConstructors === ["Solo"]
      success
    Nothing -> failure

-- | Test Unit with no fields
prop_unit_typeinfo :: Property
prop_unit_typeinfo = property $ do
  let unit = Unit
  let dynVal = toSzt unit
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      typeInfo ^. tiTypeName === Just "Unit"
      typeInfo ^. tiConstructors === ["Unit"]
      success
    Nothing -> failure

-- | Test Shape sum type with products - UPDATED to test complete constructor info
prop_shape_typeinfo :: Property
prop_shape_typeinfo = property $ do
  shape <- forAll genShape
  let dynVal = toSzt shape
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      annotate $ "TypeInfo: " ++ show typeInfo
      annotate $ "Shape value: " ++ show shape
      typeInfo ^. tiTypeName === Just "Shape"

      -- ALL constructors should be present regardless of which one is active
      typeInfo ^. tiConstructors === ["Circle", "Rectangle", "Triangle"]

      -- The structure should be a sum type with binary tree structure
      case typeInfo ^. tiStructure of
        Just (TSSum constructorTypes) -> do
          annotate $ "Sum type has " ++ show (length constructorTypes) ++ " constructor types"
          -- Generic representation uses binary trees, so we get 2 at the top level
          length constructorTypes === 2 -- Binary tree: Circle :+: (Rectangle :+: Triangle)
        Just other -> do
          annotate $ "Expected TSSum but got: " ++ show other
          failure
        Nothing -> do
          annotate "Expected type structure but got Nothing"
          failure
    Nothing -> do
      annotate "No type info found"
      failure

-- | Test that Color sum type captures ALL constructors regardless of which is serialized
prop_color_complete_constructors :: Property
prop_color_complete_constructors = property $ do
  color <- forAll genColor
  let dynVal = toSzt color
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      annotate $ "Color value: " ++ show color
      annotate $ "TypeInfo: " ++ show typeInfo

      -- Should have all constructors regardless of which color we serialized
      typeInfo ^. tiConstructors === ["Red", "Green", "Blue"]
      typeInfo ^. tiTypeName === Just "Color"

      -- Structure should be sum type with binary tree structure
      case typeInfo ^. tiStructure of
        Just (TSSum constructorTypes) -> do
          length constructorTypes === 2 -- Binary tree: Red :+: (Green :+: Blue)
        Just other -> do
          annotate $ "Expected TSSum but got: " ++ show other
          failure
        Nothing -> failure
    Nothing -> failure

-- | Test that different Color values produce identical type info (constructor completeness)
prop_color_typeinfo_consistency :: Property
prop_color_typeinfo_consistency = property $ do
  let redInfo = toSzt Red ^. dvTypeInfo
  let greenInfo = toSzt Green ^. dvTypeInfo
  let blueInfo = toSzt Blue ^. dvTypeInfo

  case (redInfo, greenInfo, blueInfo) of
    (Just red, Just green, Just blue) -> do
      -- All three should have identical constructor lists
      red ^. tiConstructors === green ^. tiConstructors
      green ^. tiConstructors === blue ^. tiConstructors
      red ^. tiConstructors === ["Red", "Green", "Blue"]

      -- All should have same type name
      red ^. tiTypeName === green ^. tiTypeName
      green ^. tiTypeName === blue ^. tiTypeName
      red ^. tiTypeName === Just "Color"
    _ -> failure

--------------------------------------------------------------------------------
-- Newtype Wrapper Tests
--------------------------------------------------------------------------------

-- | Test that UserId newtype captures wrapped type info
prop_userid_newtype_wrapper :: Property
prop_userid_newtype_wrapper = property $ do
  userId <- forAll genUserId
  let dynVal = toSzt userId
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      annotate $ "TypeInfo: " ++ show typeInfo
      typeInfo ^. tiTypeName === Just "UserId"
      typeInfo ^. tiModule === Just "Test.Serializotron.TypeInfoTests"
      typeInfo ^. tiConstructors === ["UserId"]

      -- Check that newtype wrapper is captured
      case typeInfo ^. tiNewtypeWrapper of
        Just wrapperInfo -> do
          annotate $ "Wrapper TypeInfo: " ++ show wrapperInfo
          -- Should wrap Int
          wrapperInfo ^. tiTypeName === Just "Int"
          wrapperInfo ^. tiModule === Just "GHC.Types"
        Nothing -> do
          annotate "Expected newtype wrapper info but got Nothing"
          failure
    Nothing -> do
      annotate "No type info found"
      failure

-- | Test that UserName newtype captures wrapped Text type
prop_username_newtype_wrapper :: Property
prop_username_newtype_wrapper = property $ do
  userName <- forAll genUserName
  let dynVal = toSzt userName
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      typeInfo ^. tiTypeName === Just "UserName"
      typeInfo ^. tiConstructors === ["UserName"]

      -- Check wrapper captures Text
      case typeInfo ^. tiNewtypeWrapper of
        Just wrapperInfo -> do
          wrapperInfo ^. tiTypeName === Just "Text"
          -- Note: actual module is Data.Text.Internal
          wrapperInfo ^. tiModule === Just "Data.Text.Internal"
        Nothing -> failure
    Nothing -> failure

-- | Test chained newtype wrappers (WrapperId -> UserId)
-- Note: Currently only captures one level deep due to Typeable limitation.
-- The wrapped type (UserId) is extracted via Typeable which doesn't include
-- Generic derivation info, so it won't have its own wrapper info populated.
prop_chained_newtype_wrapper :: Property
prop_chained_newtype_wrapper = property $ do
  wrapperId <- forAll genWrapperId
  let dynVal = toSzt wrapperId
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      annotate $ "TypeInfo: " ++ show typeInfo
      typeInfo ^. tiTypeName === Just "WrapperId"
      typeInfo ^. tiConstructors === ["WrapperId"]

      -- First level: should wrap UserId
      case typeInfo ^. tiNewtypeWrapper of
        Just level1 -> do
          annotate $ "Level 1 wrapper: " ++ show level1
          level1 ^. tiTypeName === Just "UserId"

          -- Note: UserId's own wrapper (Int) is not captured because
          -- the wrapper type is extracted via Typeable, not Generic.
          -- This is a known limitation - to get full chain info, each
          -- type needs to be serialized with its ToSZT instance.
          level1 ^. tiNewtypeWrapper === Nothing
        Nothing -> do
          annotate "Expected first level wrapper (UserId) but got Nothing"
          failure
    Nothing -> failure

-- | Test that product type containing newtypes preserves wrapper info in fields
prop_user_product_with_newtypes :: Property
prop_user_product_with_newtypes = property $ do
  user <- forAll genUser
  let dynVal = toSzt user
  case dynVal ^. dvTypeInfo of
    Just typeInfo -> do
      annotate $ "User TypeInfo: " ++ show typeInfo
      typeInfo ^. tiTypeName === Just "User"
      typeInfo ^. tiConstructors === ["User"]

      -- Check that structure contains fields with newtype info
      case typeInfo ^. tiStructure of
        Just (TSProduct fields) -> do
          annotate $ "Product has " ++ show (length fields) ++ " fields"
          length fields === 2

          -- Check that field types have wrapper info
          -- Note: The field types should still be UserId and UserName
          case fields of
            [field1, field2] -> do
              let fieldType1 = field1 ^. fiFieldType
              let fieldType2 = field2 ^. fiFieldType

              annotate $ "Field 1 type: " ++ show fieldType1
              annotate $ "Field 2 type: " ++ show fieldType2

              -- First field should be UserId with wrapper
              fieldType1 ^. tiTypeName === Just "UserId"
              case fieldType1 ^. tiNewtypeWrapper of
                Just wrapper1 -> wrapper1 ^. tiTypeName === Just "Int"
                Nothing -> do
                  annotate "Expected UserId to have wrapper info"
                  failure

              -- Second field should be UserName with wrapper
              fieldType2 ^. tiTypeName === Just "UserName"
              case fieldType2 ^. tiNewtypeWrapper of
                Just wrapper2 -> do
                  wrapper2 ^. tiTypeName === Just "Text"
                  -- Module is Data.Text.Internal
                  wrapper2 ^. tiModule === Just "Data.Text.Internal"
                Nothing -> do
                  annotate "Expected UserName to have wrapper info"
                  failure
            _ -> failure
        _ -> do
          annotate "Expected TSProduct structure"
          failure
    Nothing -> failure

-- | Test that newtype wrappers don't affect round-trip serialization
prop_newtype_roundtrip_userid :: Property
prop_newtype_roundtrip_userid = property $ do
  userId <- forAll genUserId
  let encoded = toSzt userId
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> userId === decoded

prop_newtype_roundtrip_username :: Property
prop_newtype_roundtrip_username = property $ do
  userName <- forAll genUserName
  let encoded = toSzt userName
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> userName === decoded

prop_newtype_roundtrip_chained :: Property
prop_newtype_roundtrip_chained = property $ do
  wrapperId <- forAll genWrapperId
  let encoded = toSzt wrapperId
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> wrapperId === decoded

prop_newtype_roundtrip_user :: Property
prop_newtype_roundtrip_user = property $ do
  user <- forAll genUser
  let encoded = toSzt user
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> user === decoded

--------------------------------------------------------------------------------
-- Round-trip Tests
--------------------------------------------------------------------------------

-- | Test that Person round-trips correctly
prop_person_roundtrip :: Property
prop_person_roundtrip = property $ do
  person <- forAll genPerson
  let encoded = toSzt person
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> person === decoded

-- | Test that Employee round-trips correctly
prop_employee_roundtrip :: Property
prop_employee_roundtrip = property $ do
  employee <- forAll genEmployee
  let encoded = toSzt employee
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> employee === decoded

-- | Test that Shape round-trips correctly
prop_shape_roundtrip :: Property
prop_shape_roundtrip = property $ do
  shape <- forAll genShape
  let encoded = toSzt shape
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> shape === decoded

-- | Test that Color round-trips correctly
prop_color_roundtrip :: Property
prop_color_roundtrip = property $ do
  color <- forAll genColor
  let encoded = toSzt color
  case fromSzt encoded of
    Left err -> do
      annotate $ "Failed to decode: " ++ show err
      failure
    Right decoded -> color === decoded
