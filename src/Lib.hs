module Lib
  ( FileData (..),
    inferred,
    explicit,
  )
where

import Data.Char (toUpper)
import Git

data FileData = FileData {dir :: FilePath, path :: FilePath, content :: String}

inferred :: String -> IO (Either String FileData)
inferred dir =
  fdata <$> latestCommitMessage
  where
    fdata s = buildFileData dir <$> extractTicketMessage s

explicit :: FilePath -> String -> String -> FileData
explicit dir ticket message = buildFileData dir (ticket, message)

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

extractTicketMessage :: String -> Either String (String, String)
extractTicketMessage ('[' : cs) = case split ']' cs of
  (_, "") ->
    Left $
      "Couldn't parse the commit, did you forget a bracket or the message?\n"
        ++ "Commit message: ["
        ++ cs
  (ticket, ' ' : message) -> Right (ticket, message)
  pair -> Right pair
extractTicketMessage commit =
  Left $ "Couldn't parse the commit, did you forget the ticket?\nCommit message: " ++ commit
