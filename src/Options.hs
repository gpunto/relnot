module Options
  ( parseOpts,
    Opts (..),
    Mode (..),
  )
where

import Options.Applicative

data Mode = Infer | Ticket {name :: String, message :: String} deriving (Show)

data Opts = Opts {mode :: Mode, directory :: FilePath, addToGit :: Bool} deriving (Show)

parseOpts :: IO Opts
parseOpts = execParser (info optsParser (progDesc "Create release notes file"))

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
ticketParser = Ticket <$> nameParser <*> messageParser

nameParser :: Parser String
nameParser =
  strArgument $
    metavar "TICKET"
      <> help "Ticket name"

messageParser :: Parser String
messageParser =
  strArgument $
    metavar "MESSAGE"
      <> value ""
      <> help "Release notes message"

defaultPath :: FilePath
defaultPath = "./release_notes"

pathParser :: Parser FilePath
pathParser =
  strOption $
    value defaultPath
      <> long "dir"
      <> short 'd'
      <> metavar "DIRECTORY"
      <> help "Path to directory"

gitAddParser :: Parser Bool
gitAddParser =
  switch $
    long "git-add"
      <> short 'g'
      <> help "Whether to run git add on the generated file"
