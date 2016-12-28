{-# LANGUAGE OverloadedStrings #-}
module Api ( fetchCategories, fetchAbbreviation ) where

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)

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
