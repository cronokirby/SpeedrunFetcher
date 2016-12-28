{-# LANGUAGE OverloadedStrings #-}
module Api where

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Parsing

type Url = String

testUrl :: Url
testUrl = "http://www.speedrun.com/api/v1/games/j1l7pvdg/categories"

fetchJSON :: Url -> IO B.ByteString
fetchJSON = simpleHttp


fetchCategories :: Url -> IO (Either String [String])
fetchCategories url = do
    let json = fetchJSON url
    parseCategories json
