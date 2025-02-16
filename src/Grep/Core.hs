{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Grep.Core where

import qualified System.Environment as Sys
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           System.Directory
import           Control.Exception
import           Data.Text.Internal.Search
import           System.FilePath ((</>))

data LocationType = DirectoryType | FileType deriving (Eq,Show)

data SearchType = SearchType {
      searchTerm :: Text
    , searchLocation :: FilePath
    , searchLocationType :: LocationType
} deriving (Eq,Show)

fetchVals :: IO (Either Text SearchType)
fetchVals = do
    argList <- Sys.getArgs
    if (length argList < 2) then pure $ Left "Please provide searchTerm and searchLocation as arguments" else do
        let searchLocation = argList !! 1
        fb <- doesFileExist searchLocation
        db <- doesDirectoryExist searchLocation
        if (fb) then pure $ Right $ SearchType (T.pack (head argList)) searchLocation FileType 
            else if (db) then pure $ Right $ SearchType (T.pack(head argList)) searchLocation DirectoryType
                else pure $ Left $ "Search Location is invalid " <> (T.pack searchLocation)

main' :: IO ()
main' = do
    eRes <- fetchVals
    case eRes of
        Left e -> T.putStrLn e
        Right r -> processSearching r

processSearching :: SearchType -> IO ()
processSearching SearchType{..} = do
    case searchLocationType of
        DirectoryType -> do
            files <- listDirectory searchLocation
            mapM_ (\file -> do 
                res <- doesDirectoryExist (searchLocation </> file)
                if res then processSearching (SearchType searchTerm (searchLocation </> file) DirectoryType) else searchFile searchTerm (searchLocation </> file)) files
        FileType -> searchFile searchTerm searchLocation

searchFile :: Text -> FilePath -> IO ()
searchFile searchTerm searchLocation = do
    eContent <- try $ T.lines <$> T.readFile searchLocation :: IO (Either IOError [Text] )
    case eContent of
        Left _ -> pure ()
        Right content -> searchString searchTerm content

searchString :: Text -> [Text] -> IO ()
searchString searchTerm content = searchString_ searchTerm content 1 

searchString_ :: Text -> [Text] -> Int -> IO ()
searchString_ _ [] _ = pure ()
searchString_ searchTerm (hayStack:remainingLines) lineNumber = do
    case indices searchTerm hayStack of
        [] -> searchString_ searchTerm remainingLines (lineNumber + 1)
        _ -> (T.putStrLn $ (T.pack $ show lineNumber) <> " " <> hayStack) >> searchString_ searchTerm remainingLines (lineNumber + 1)