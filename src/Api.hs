{-# LANGUAGE OverloadedStrings #-}
module Api ( fetchCategories, fetchAbbreviation ) where

import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit (simpleHttp)

import Parsing

type Url = String
type Json = IO ByteString

testUrl :: Url
testUrl = "http://www.speedrun.com/api/v1/games/j1l7pvdg/categories"

fetchJSON :: Url -> Json
fetchJSON = simpleHttp


fetchCategories :: Url -> IO (Either String [String])
fetchCategories url = do
    let json = fetchJSON url
    parseCategories json


fetchAbbreviation :: Url -> IO (Either String String)
fetchAbbreviation url = do
    let json = fetchJSON url
    parseGameWith abbreviation json

fetchLeaderboard :: String -> Url -> IO (Either String String)
fetchLeaderboard catName url = do
    let json = fetchJSON url
    parseLeaderboard catName json

-- needs IO for fetching a name, converts a Run into a tuple of info
type Rank = String  -- I.E. WR, 1st place time
runToString :: IO (Either String Run) -> Rank -> IO (Either String String)
runToString parsedRun rank= do
    run <- parsedRun
    return $ case run of
        Left err -> Left err
        Right runInfo -> Right $ process runInfo
        where
          process :: Run -> String
          process run = "The " ++ rank ++ "is " ++ timeString ++ " by " ++ name
              where
                name = fromMaybe "fetchName $ userLink run" $ playerName run
                timeString = "formatTime $ time run"
