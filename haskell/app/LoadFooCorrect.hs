module Main where

import Data.Text (unpack)
import GHC.Generics
import Serializotron

data Foo = A Int | B Int -- Same constructor order as SaveFoo
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

main :: IO ()
main = do
  putStrLn "Attempting to load foo.szt with correct constructor order..."
  result <- loadSzt "foo.szt" :: IO (Either SerializotronError Foo)
  case result of
    Left err -> putStrLn $ "Error: " ++ unpack (formatError err)
    Right value -> putStrLn $ "Successfully loaded: " ++ show value
