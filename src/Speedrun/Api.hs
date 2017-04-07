{-# LANGUAGE OverloadedStrings #-}
module Speedrun.Api ( fetchCategories, fetchAbbreviation
                    , fetchLeaderboard, fetchTime ) where

import Data.Bifunctor
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Conduit (simpleHttp)
import Text.Printf

import Speedrun.Parsing

type Url = String
type Json = IO ByteString

fetchJSON :: Url -> Json
fetchJSON = simpleHttp


fetchCategories :: Url -> IO (Either String [String])
fetchCategories = parseCategories . fetchJSON


fetchAbbreviation :: Url -> IO (Either String String)
fetchAbbreviation = parseGameWith abbreviation . fetchJSON

fetchLeaderboard :: String -> Url -> IO (Either String Url)
fetchLeaderboard catName = parseLeaderboard catName . fetchJSON


-- needs IO for fetching a name, converts a Run into a tuple of info
type Rank = String  -- I.E. WR, 1st place time
runToString :: IO (Either String Run) -> Rank -> IO (Either String String)
runToString parsedRun rank = parsedRun >>= either (return . Left) process
  where
    process :: Run -> IO (Either String String)
    process run =
      let timeString = formatTime $ time run
      in fmap (\name ->
               printf "The %s is %s by %s\n%s" rank timeString name (video run))
         <$> fetchName (playerName run) (userLink run)


fetchName :: Maybe String -> Url -> IO (Either String String)
fetchName mayName url = maybe (parseUser $ fetchJSON url) (return . Right) mayName


fetchTime :: Int -> Url -> IO (Either String String)
fetchTime place = (flip runToString (rank place) . parseRun place) . fetchJSON
