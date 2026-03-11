{-# LANGUAGE GADTs #-}

-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DeriveAnyClass #-}

module Main where

import GHC.Generics
import Serializotron

-- | A typed expression GADT demonstrating Generic's limitations
--
-- This demonstrates that Serializotron's Generic-based approach
-- cannot handle GADTs because GHC cannot derive Generic instances
-- for GADT constructors.
--
-- The error you'll see when trying to build:
--   • Can't make a derived instance of 'Generic (Expr a)':
--       Constructor 'LitInt' is a GADT
--       Constructor 'LitBool' is a GADT
--       ...
--
-- For GADTs, you would need to manually implement ToSZT/FromSZT
-- or use a different serialization approach.
data Expr a where
  LitInt :: Int -> Expr Int
  LitBool :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  IsZero :: Expr Int -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

-- deriving (Generic, Show, ToSZT, FromSZT)  -- ❌ This will fail!

exampleExpr :: Expr Int
exampleExpr = If (IsZero (LitInt 0)) (LitInt 42) (Add (LitInt 1) (LitInt 2))

main :: IO ()
main = do
  putStrLn "Trying to serialize a GADT..."
  -- saveSzt "gadt.szt" exampleExpr
  putStrLn "Success!"
