module Main where

import Control.Conditional (ifM, notM, (<||>))
import Control.Monad (when)
import Data.Char (isSpace)
import Git (gitAdd)
import Lib
import Options
import System.Directory (createDirectoryIfMissing, doesFileExist, getDirectoryContents)

main :: IO ()
main = do
  (Opts mode dir overwrite addToGit) <- parseOpts
  fileDataEither <- toFileData mode dir
  case fileDataEither of
    Right (FileData dir path content) -> do
      createDirectoryIfMissing False dir
      ifM
        (return overwrite <||> (notM . doesFileExist $ path))
        performWrite
        (putStrLn $ "File \"" ++ path ++ "\" already exists. Use overwrite option to force overwrite.")
      where
        performWrite = do
          writeFile path content
          Control.Monad.when addToGit $ gitAdd path
    Left error -> putStrLn error

toFileData :: Mode -> String -> IO (Either String FileData)
toFileData mode dir = case mode of
  Infer branch -> inferred branch dir
  Ticket ticket message -> return . Right $ explicit dir ticket message
