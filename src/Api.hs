{-# LANGUAGE OverloadedStrings #-}
module Api ( fetchCategories, fetchAbbreviation
           , fetchLeaderboard, fetchTime ) where

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

fetchLeaderboard :: String -> Url -> IO (Either String Url)
fetchLeaderboard catName url = do
    let json = fetchJSON url
    parseLeaderboard catName json

-- needs IO for fetching a name, converts a Run into a tuple of info
type Rank = String  -- I.E. WR, 1st place time
runToString :: IO (Either String Run) -> Rank -> IO (Either String String)
runToString parsedRun rank= do
    run <- parsedRun
    case run of
        Left err -> return $ Left err
        Right runInfo -> process runInfo
        where
          process :: Run -> IO (Either String String)
          process run = do
              name <- fetchName (playerName run) (userLink run)
              let timeString = formatTime $ time run
              return $
                (++) <$> ((++) <$>
                Right ("The " ++ rank ++ " is " ++ timeString ++ " by ")
                <*> name) <*> Right ("\n" ++ video run)


fetchName :: Maybe String -> Url -> IO (Either String String)
fetchName (Just name) _ = return $ Right name
fetchName Nothing url = do
    let json = fetchJSON url
    parseUser json


fetchTime :: Int -> Url -> IO (Either String String)
fetchTime place url = do
    let json = fetchJSON url
    let runData = parseRun place json
    let rankString = rank place
    runToString runData rankString
