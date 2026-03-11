module Main where

import Data.Text (unpack)
import GHC.Generics
import Serializotron

data Foo = B Int | A Int -- Constructor order changed!
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

main :: IO ()
main = do
  putStrLn "Attempting to load foo.szt..."
  result <- loadSzt "foo.szt" :: IO (Either SerializotronError Foo)
  case result of
    Left err -> putStrLn $ "Error (as expected): " ++ unpack (formatError err)
    Right value -> putStrLn $ "Loaded: " ++ show value
