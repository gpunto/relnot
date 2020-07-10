module Main where

import Control.Monad (when)
import Data.Char (isSpace, toUpper)
import Options.Applicative
import System.Directory (createDirectoryIfMissing, getDirectoryContents)
import System.Process (callProcess, readProcess)

data Mode = Infer | Ticket {name :: String} deriving (Show)

data Opts = Opts {mode :: Mode, directory :: FilePath, addToGit :: Bool} deriving (Show)

data FileData = FileData {dir :: FilePath, path :: FilePath, content :: String}

main :: IO ()
main = do
  opts <- execParser (info optsParser (progDesc "Create release notes file"))
  fileDataEither <- optsToFileData opts
  case fileDataEither of
    Right (FileData dir path content) -> do
      createDirectoryIfMissing False dir
      writeFile path content
      Control.Monad.when (addToGit opts) $ gitAdd path
    Left error -> putStrLn error

optsParser :: Parser Opts
optsParser = Opts <$> modeParser <*> pathParser <*> gitAddParser

modeParser :: Parser Mode
modeParser = inferParser <|> ticketParser

inferParser :: Parser Mode
inferParser =
  flag' Infer $
    long "infer"
      <> short 'i'
      <> help "Try to infer ticket and message"

ticketParser :: Parser Mode
ticketParser = Ticket <$> nameParser

nameParser :: Parser String
nameParser =
  strArgument $
    metavar "NAME"
      <> help "ticket name"

defaultPath :: FilePath
defaultPath = "./release_notes"

pathParser :: Parser FilePath
pathParser =
  strOption $
    value defaultPath
      <> long "dir"
      <> short 'd'
      <> metavar "DIRECTORY"
      <> help "path to directory"

gitAddParser :: Parser Bool
gitAddParser =
  switch $
    long "git-add"
      <> short 'g'
      <> help "Whether to run git add on the generated file"

optsToFileData :: Opts -> IO (Either String FileData)
optsToFileData (Opts mode dir gitAdd) = case mode of
  Infer -> do
    fdata <$> latestCommitMessage
    where
      fdata s = buildFileData dir <$> extractTicketMessage s
  (Ticket ticket) -> return . Right $ buildFileData dir (ticket, "")

buildFileData :: FilePath -> (String, String) -> FileData
buildFileData dir (ticket, message) = FileData dir p c
  where
    p = dir ++ "/" ++ ticket
    upper = map toUpper ticket
    (team, number) = split '-' upper
    c = "[" ++ capHack team ++ "] " ++ message ++ " (" ++ upper ++ ")\n"

split :: Eq a => a -> [a] -> ([a], [a])
split c = go []
  where
    go acc [] = (reverse acc, [])
    go acc (h : hs) =
      if h == c
        then (reverse acc, hs)
        else go (h : acc) hs

capHack :: String -> String
capHack "CAP" = "CaP"
capHack s = s

latestCommitMessage :: IO String
latestCommitMessage = trim <$> readProcess "git" ["show-branch", "--no-name", "HEAD"] ""
  where
    trim = f . f
    f = reverse . dropWhile isSpace

extractTicketMessage :: String -> Either String (String, String)
extractTicketMessage ('[' : cs) = case split ']' cs of
  (_, "") -> Left $ "Couldn't parse the commit, did you forget a bracket or the message?\nCommit message: [" ++ cs
  (ticket, ' ' : message) -> Right (ticket, message)
  pair -> Right pair
extractTicketMessage commit =
  Left $ "Couldn't parse the commit, did you forget the ticket?\nCommit message: " ++ commit

gitAdd :: String -> IO ()
gitAdd path = callProcess "git" ["add", path]
