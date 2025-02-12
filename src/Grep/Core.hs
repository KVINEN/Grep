module Grep.Core where

import qualified System.Environment as Sys

fetchVals :: IO (Either text [String])
fetchVals = do
    argList <- Sys.getArgs
    if (length argList < 2) then putStrLn "Please provide correct arguments"


main' :: IO ()
main' = do
    eRes = fetchVals
    case eRes of
        Left e -> T.putStrLn e
        Right 