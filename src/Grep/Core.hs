{-# LANGUAGE OverloadedStrings #-}
module Grep.Core where

import qualified System.Environment as Sys
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           System.Directory

data LocationType = DirectoryType | FileType deriving (Eq,Show)

data SearchType = SearchType {
      searchTerm :: Text
    , searchLocation :: filepath
    , searchLocationType :: LocationType
} deriving (Eq,Show)

fetchVals :: IO (Either Text SearchType)
fetchVals = do
    argList <- Sys.getArgs
    if (length argList < 2) then pure $ Left "Please provide searchTerm and searchLocation as arguments" else do
        let searchLocation = argList !! 1
        fb <- doesFileExist searchLocation
        db <- doesDirectoryExist searchLocation
        if (fb) then pure $ Right $ SearchType (head argList) searchLocation FileType
        if (fb || db) then pure $ Left ("Search Location not valid, must be either filepath or directory path " <> (T.pack searchLocation)) else pure $ Right argList

main' :: IO ()
main' = do
    eRes <- fetchVals
    case eRes of
        Left e -> T.putStrLn e
        Right r -> processSearching r

processSearching :: [String] -> IO ()
processSearching = undefined