{-# LANGUAGE OverloadedStrings #-}
module Parsing ( parseCategories
               , Game (..)
               , parseGameWith
               , parseLeaderboard
               , Run (..)
               , parseRun) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)
-- Just imported for resolvename, which has to be here cause of Circular depends
import Network.HTTP.Conduit (simpleHttp)


type Json = IO ByteString
type Url = String

fetchJSON :: Url -> Json
fetchJSON = simpleHttp

data CategoryData = CategoryData { cats :: [Category] } deriving (Show)
instance FromJSON CategoryData where
    parseJSON (Object o) = CategoryData <$> (o .: "data")


data Category = Category { catName :: String
                         , leaderboard :: Url} deriving (Show)
instance FromJSON Category where
    parseJSON (Object o) = Category
        <$> (o .: "name")
        <*> (last <$> o .: "links" >>= (.: "uri"))


parseCategories :: Json -> IO (Either String [String])
parseCategories json = do
    catData <- (eitherDecode <$> json) :: IO (Either String CategoryData)
    return $ catData >>= \catData -> return $ map catName $ cats catData

parseLeaderboard :: String -> Json -> IO (Either String String)
parseLeaderboard cat json = do
    catData <- (eitherDecode <$> json) :: IO (Either String CategoryData)
    return $ catData >>= \catData ->
        case filter (\x -> catName x == cat) (cats catData) of
            []         -> Left "That category doesn't exist"
            [category] -> Right $ leaderboard category


data Game = Game { id :: String, abbreviation :: String
                 , gameName :: String} deriving (Show)
instance FromJSON Game where
    parseJSON (Object o) = Game
        <$> (gameData >>= (.: "id"))
        <*> (gameData >>= (.: "abbreviation"))
        <*> (gameData >>= (.: "names") >>= (.: "twitch"))
        where gameData = head <$> o .: "data"

-- Needs to be given one of the record functions
parseGameWith :: (Game -> String) -> Json -> IO (Either String String)
parseGameWith info json = do
    gameData <- (eitherDecode <$> json) :: IO (Either String Game)
    return $ gameData >>= \gameData -> return $ info gameData


data RunData = RunData { runs :: [Run]} deriving (Show)
instance FromJSON RunData where
    parseJSON (Object o) = RunData
        <$> ((o .: "data") >>= (.: "runs"))

type Seconds = Int
data Run = Run { playerName :: Maybe String, userLink :: Url
               , time :: Seconds, video :: Url} deriving (Show)
instance FromJSON Run where
    parseJSON (Object o) = Run
        <$> (player >>= (.:? "name"))
        <*> (player >>= (.: "uri"))
        <*> ((o .: "times") >>= (.: "primary_t"))
        <*> ((head <$> (o .: "videos") >>= (.: "links")) >>= (.: "uri"))
        where
          player = head <$> o .: "players"

parseRun :: Int -> Json -> IO (Either String Run)
parseRun place json = do
    runData <- (eitherDecode <$> json) :: IO (Either String RunData)
    return $ runData >>= \runData -> return $ runs runData !! place
