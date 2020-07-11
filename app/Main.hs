module Main where

import Control.Monad (when)
import Data.Char (isSpace)
import Git (gitAdd)
import Lib
import Options
import System.Directory (createDirectoryIfMissing, getDirectoryContents)

main :: IO ()
main = do
  opts <- parseOpts
  fileDataEither <- optsToFileData opts
  case fileDataEither of
    Right (FileData dir path content) -> do
      createDirectoryIfMissing False dir
      writeFile path content
      Control.Monad.when (addToGit opts) $ gitAdd path
    Left error -> putStrLn error

optsToFileData :: Opts -> IO (Either String FileData)
optsToFileData (Opts mode dir _) = case mode of
  Infer -> inferred dir
  (Ticket ticket message) -> return . Right $ explicit dir ticket message
