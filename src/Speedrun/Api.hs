{-# LANGUAGE OverloadedStrings #-}
module Speedrun.Api ( fetchCategories, fetchAbbreviation
                    , fetchLeaderboard, fetchTime, ParseResult(..)) where

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


fetchCategories :: Url -> ParseResult [String]
fetchCategories = parseCategories . fetchJSON


fetchAbbreviation :: Url -> ParseResult String
fetchAbbreviation = parseGameWith abbreviation . fetchJSON

fetchLeaderboard :: String -> Url -> ParseResult Url
fetchLeaderboard catName = parseLeaderboard catName . fetchJSON


-- Returns a function capable of printing a run
fmtRun :: Rank -> Run -> ParseResult String
fmtRun rank run = fmtName <$> fetchName (playerName run) (userLink run)
  where
    timeString = formatTime $ time run
    fmtName n =
      printf "The %s is %s by %s\n%s" rank timeString n (video run)

-- needs IO for fetching a name, converts a Run into a tuple of info
type Rank = String  -- I.E. WR, 1st place time
runToString :: Rank -> ParseResult Run -> ParseResult String
runToString = (=<<) . fmtRun



fetchName :: Maybe String -> Url -> ParseResult String
fetchName mayName url =
  maybe (parseUser $ fetchJSON url) return mayName


fetchTime :: Int -> Url -> ParseResult String
fetchTime place = (runToString (rank place) . parseRun place) . fetchJSON
