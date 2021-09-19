module Options
  ( parseOpts,
    Opts (Opts),
    Mode (..),
    OnConflict (..),
  )
where

import Options.Applicative

data Mode
  = Infer
      { branch :: Bool
      }
  | Ticket
      { originalName :: String,
        message :: String
      }
  deriving (Show)

data OnConflict
  = Abort
  | Overwrite
  | CreateNew
  deriving (Show)

data Opts = Opts
  { mode :: Mode,
    directory :: FilePath,
    onConflict :: OnConflict,
    addToGit :: Bool
  }
  deriving (Show)

parseOpts :: IO Opts
parseOpts = execParser (info (optsParser <**> helper) (progDesc "Create release notes file"))

optsParser :: Parser Opts
optsParser = Opts <$> modeParser <*> pathParser <*> onConflictParser <*> gitAddParser

modeParser :: Parser Mode
modeParser = inferParser <|> ticketParser

inferParser :: Parser Mode
inferParser =
  Infer
    <$> ( flag'
            ()
            ( long "infer"
                <> short 'i'
                <> help "Try to infer ticket and message"
            )
            *> branchParser
        )

ticketParser :: Parser Mode
ticketParser = Ticket <$> nameParser <*> messageParser

branchParser :: Parser Bool
branchParser =
  switch $
    long "branch"
      <> short 'b'
      <> help "Whether to use the branch name for inferring"

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

onConflictParser :: Parser OnConflict
onConflictParser = overwriteParser <|> createNewParser <|> pure Abort

overwriteParser :: Parser OnConflict
overwriteParser =
  flag' Overwrite $
    long "overwrite"
      <> short 'o'
      <> help "Whether the generated file should overwrite the existing one in case of conflict"

createNewParser :: Parser OnConflict
createNewParser =
  flag' CreateNew $
    long "create-new"
      <> short 'n'
      <> help "Whether the generated file should be created alongside the existing one in case of conflict"
