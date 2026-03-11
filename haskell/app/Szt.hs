{-# LANGUAGE OverloadedStrings #-}

module Main where

import Codec.Compression.GZip qualified as GZip
import Control.Monad (unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (chr)
import Data.Text qualified as Text
import Data.Word (Word8)
import Serializotron
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr, stdout)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> usage >> exitFailure
    ["cat", file] -> cmdCat file
    ["pretty", file] -> cmdPretty file
    ["inspect", file] -> cmdInspect file
    ["fsck", file] -> cmdFsck file
    ["help"] -> usage >> exitSuccess
    ["--help"] -> usage >> exitSuccess
    ["-h"] -> usage >> exitSuccess
    _ -> do
      hPutStrLn stderr "Error: Invalid command or arguments"
      usage
      exitFailure

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <command> [args]"
  putStrLn ""
  putStrLn "Commands:"
  putStrLn "  cat <file>       Emit raw protobuf bytes (after decompression)"
  putStrLn "  pretty <file>    Pretty-print file using protoc"
  putStrLn "  inspect <file>   Show header and file information"
  putStrLn "  fsck <file>      Verify file integrity"
  putStrLn ""
  putStrLn "Options:"
  putStrLn "  help, --help, -h Show this help message"

-- | Read and decompress a .szt file, returning the raw protobuf bytes
readSztFile :: FilePath -> IO (Either String BS.ByteString)
readSztFile path = do
  fileBytes <- BS.readFile path

  if BS.length fileBytes < 8
    then return $ Left "File too short to contain header"
    else do
      -- Parse header
      case decodeHeader (BS.take 8 fileBytes) of
        Left err -> return $ Left $ Text.unpack err
        Right header -> do
          let payloadBytes = BS.drop 8 fileBytes
          -- Decompress if needed
          case _headerCompression header of
            NoCompression -> return $ Right payloadBytes
            GZipCompression ->
              return $ Right $ LBS.toStrict $ GZip.decompress $ LBS.fromStrict payloadBytes

-- | cat command: output raw protobuf bytes
cmdCat :: FilePath -> IO ()
cmdCat file = do
  result <- readSztFile file
  case result of
    Left err -> do
      hPutStrLn stderr $ "Error reading file: " ++ err
      exitFailure
    Right protoBytes -> do
      BS.hPut stdout protoBytes
      exitSuccess

-- | pretty command: pretty-print using protoc
-- We use shell piping since readProcessWithExitCode doesn't handle binary stdin well
cmdPretty :: FilePath -> IO ()
cmdPretty file = do
  -- Use cabal run to invoke our own cat command via shell
  let catCmd = "cabal run -v0 szt -- cat " ++ file
  let protocCmd = "protoc -I proto --decode=szt.SZTFile proto/serializotron.proto"
  let fullCmd = catCmd ++ " | " ++ protocCmd

  (exitCode, output, errOutput) <-
    readProcessWithExitCode "sh" ["-c", fullCmd] ""

  putStr output
  unless (null errOutput) $
    hPutStrLn stderr errOutput

  case exitCode of
    ExitSuccess -> exitSuccess
    ExitFailure _ -> exitFailure

-- | inspect command: show file information
cmdInspect :: FilePath -> IO ()
cmdInspect file = do
  fileBytes <- BS.readFile file

  let fileSize = BS.length fileBytes

  if fileSize < 8
    then do
      hPutStrLn stderr "Error: File too short to contain header"
      exitFailure
    else do
      case decodeHeader (BS.take 8 fileBytes) of
        Left err -> do
          hPutStrLn stderr $ "Error: " ++ Text.unpack err
          exitFailure
        Right header -> do
          let payloadSize = fileSize - 8
          let compressionName = case _headerCompression header of
                NoCompression -> "none" :: String
                GZipCompression -> "gzip" :: String

          printf "File: %s\n" file
          printf "Format version: %d\n" (_headerVersion header :: Word8)
          printf "Compression: %s\n" (compressionName :: String)
          printf "Total size: %d bytes\n" fileSize
          printf "Header size: 8 bytes\n"
          printf "Payload size: %d bytes\n" payloadSize

          when (_headerCompression header /= NoCompression) $ do
            result <- readSztFile file
            case result of
              Right protoBytes ->
                printf "Decompressed size: %d bytes\n" (BS.length protoBytes)
              Left _ -> return ()

          exitSuccess

-- | fsck command: verify file integrity
cmdFsck :: FilePath -> IO ()
cmdFsck file = do
  result <- fsckSzt file

  let passed = _fsckPassed result
  let errors = _fsckErrors result
  let warnings = _fsckWarnings result
  let stats = _fsckStats result

  printf "Checking: %s\n" file
  printf "Status: %s\n" (if passed then "OK" :: String else "FAILED")
  printf "\n"
  printf "Statistics:\n"
  printf "  Shared values: %d\n" (_fsckSharedValues stats)
  printf "  Total references: %d\n" (_fsckTotalReferences stats)
  printf "  Dangling references: %d\n" (_fsckDanglingReferences stats)

  unless (null errors) $ do
    printf "\nErrors (%d):\n" (length errors)
    mapM_ (putStrLn . ("  - " ++) . show) errors

  unless (null warnings) $ do
    printf "\nWarnings (%d):\n" (length warnings)
    mapM_ (putStrLn . ("  - " ++) . show) warnings

  if passed
    then exitSuccess
    else exitFailure
