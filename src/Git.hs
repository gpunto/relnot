module Git
  ( latestCommitMessage,
    gitAdd,
  )
where

import Data.Char (isSpace)
import System.Process (callProcess, readProcess)

latestCommitMessage :: IO String
latestCommitMessage = trim <$> readProcess "git" ["show-branch", "--no-name", "HEAD"] ""
  where
    trim = f . f
    f = reverse . dropWhile isSpace

gitAdd :: String -> IO ()
gitAdd path = callProcess "git" ["add", path]
