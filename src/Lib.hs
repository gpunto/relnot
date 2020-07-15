{-# LANGUAGE TupleSections #-}

module Lib
  ( FileData (..),
    inferred,
    explicit,
  )
where

import Data.Char (toUpper)
import Git

data FileData = FileData {dir :: FilePath, path :: FilePath, content :: String}

inferred :: Bool -> String -> IO (Either String FileData)
inferred useBranch = if useBranch then branchInfer else commitInfer

commitInfer :: String -> IO (Either String FileData)
commitInfer dir =
  fdata <$> latestCommitMessage
  where
    fdata s = buildFileData dir <$> commitExtractTicketMessage s

branchInfer :: String -> IO (Either String FileData)
branchInfer dir =
  fdata <$> branchName
  where
    fdata s = buildFileData dir . (,"") <$> branchExtractTicket s

explicit :: FilePath -> String -> String -> FileData
explicit dir ticket message = buildFileData dir (ticket, message)

buildFileData :: FilePath -> (String, String) -> FileData
buildFileData dir (ticket, message) = FileData dir p c
  where
    p = dir ++ "/" ++ ticket ++ ".md"
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

commitExtractTicketMessage :: String -> Either String (String, String)
commitExtractTicketMessage ('[' : cs) = case split ']' cs of
  (_, "") ->
    Left $
      "Couldn't parse the commit, did you forget a bracket or the message?\n"
        ++ "Commit message: ["
        ++ cs
  (ticket, ' ' : message) -> Right (ticket, message)
  pair -> Right pair
commitExtractTicketMessage commit =
  Left $ "Couldn't parse the commit, did you forget the ticket?\nCommit message: " ++ commit

branchExtractTicket :: String -> Either String String
branchExtractTicket name = case split '/' name of
  (name, "") -> Left "Couldn't parse the branch name, is it in the \"[type/]ticket/name\" format?"
  ("feat", rest) -> branchExtractTicket rest
  ("feature", rest) -> branchExtractTicket rest
  ("fix", rest) -> branchExtractTicket rest
  ("tech", rest) -> branchExtractTicket rest
  (ticket, _) -> Right ticket
