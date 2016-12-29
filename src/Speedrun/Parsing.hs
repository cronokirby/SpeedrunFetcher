{-# LANGUAGE OverloadedStrings #-}
module Speedrun.Parsing ( parseCategories
               , Game (..)
               , parseGameWith
               , parseLeaderboard
               , Run (..)
               , parseRun
               , formatTime
               , parseUser
               , rank) where

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

parseLeaderboard :: String -> Json -> IO (Either String Url)
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
        <*> (run >>= (.: "times") >>= (.: "primary_t"))
        <*> ((head <$> (run >>= (.: "videos") >>= (.: "links")))
            >>= (.: "uri"))
        where
          run    = o .: "run"
          player = head <$> (o .: "run" >>= (.: "players"))

parseRun :: Int -> Json -> IO (Either String Run)
parseRun place json = do
    runData <- (eitherDecode <$> json) :: IO (Either String RunData)
    return $ runData >>= \runData -> return $ runs runData !! place


formatTime :: Seconds -> String
formatTime n = let (m, secs)     = n `divMod` 60
                   (hours, mins) = m `divMod` 60
                   pad :: Int -> String
                   pad n
                       | n < 10    = "0" ++ show n
                       | otherwise = show n
                   addColon :: String -> String
                   addColon "0" = ""
                   addColon n = n ++ ":"
                in addColon (show hours) ++ addColon (pad mins) ++ pad secs


data User = User { userName :: String }
instance FromJSON User where
    parseJSON (Object o) = User
        <$> ((o .: "data") >>= (.: "names") >>= (.: "international"))

parseUser :: Json -> IO (Either String String)
parseUser json = do
    userData <- (eitherDecode <$> json) :: IO (Either String User)
    return $ userData >>= \userData -> return $ userName userData

rank :: Int -> String   --shifted, because place is used as the run Index
rank 0 = "WR"
rank 1 = "2nd place time"
rank 2 = "3rd place time"
rank n = show (n + 1) ++ "th place time"
