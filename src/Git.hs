module Git
  ( latestCommitMessage,
    branchName,
    gitAdd,
  )
where

import Data.Char (isSpace)
import System.Process (callProcess, readProcess)

latestCommitMessage :: IO String
latestCommitMessage = trim <$> readProcess "git" ["show-branch", "--no-name", "HEAD"] ""

branchName :: IO String
branchName = trim <$> readProcess "git" ["rev-parse", "--abbrev-ref", "HEAD"] ""

gitAdd :: String -> IO ()
gitAdd path = callProcess "git" ["add", path]

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile isSpace
