module Main where

import Serializotron
import Serializotron.Examples qualified as E
import Serializotron.Instances ()
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  putStrLn "==== Example: Just 42 ===="
  pPrint (toSzt (Just 42 :: Maybe Int))

  putStrLn "==== Example: Nothing :: Int ===="
  pPrint (toSzt (Nothing :: Maybe Int))

  putStrLn "==== Example: exampleTree ===="
  pPrint (toSzt E.exampleTree)
