module Main where

import Control.Monad (when)
import Git (gitAdd)
import Lib
import Options
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory)

main :: IO ()
main = do
  (Opts mode targetDir onConflict addToGit) <- parseOpts
  fileDataEither <- toFileData mode targetDir
  case fileDataEither of
    Left errorMsg -> putStrLn errorMsg
    Right fileData -> do
      pathEither <- validate onConflict (directory fileData) (fileName fileData)

      case pathEither of
        Left errorMsg -> putStrLn errorMsg
        Right newName -> createReleaseNotesFile fileData {fileName = newName} addToGit

createReleaseNotesFile :: FileData -> Bool -> IO ()
createReleaseNotesFile (FileData dir desiredName fileContent) addToGit = do
  createDirectoryIfMissing False dir
  writeFile path fileContent
  Control.Monad.when addToGit $ gitAdd path
  where
    path = buildFilePath dir desiredName

toFileData :: Mode -> String -> IO (Either String FileData)
toFileData mode dir = case mode of
  Infer useBranch -> inferred useBranch dir
  Ticket ticket msg -> return . Right $ explicit dir ticket msg

validate :: OnConflict -> FilePath -> FileName -> IO (Either String FileName)
validate onConflict dir desiredName = doesFileExist path >>= validated
  where
    path = buildFilePath dir desiredName

    validated :: Bool -> IO (Either String FileName)
    validated False = return . Right $ desiredName
    validated True = case onConflict of
      Abort ->
        return . Left $
          "File \"" ++ path ++ "\" already exists.\n"
            ++ "Use overwrite option to force overwrite or create-new option to add alongside the existing one."
      Overwrite -> return . Right $ desiredName
      CreateNew -> Right . findFreeName desiredName <$> listDirectory dir
