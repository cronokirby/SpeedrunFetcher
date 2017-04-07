{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Speedrun.Parsing ( parseCategories
               , Game (..)
               , parseGameWith
               , parseLeaderboard
               , Run (..)
               , parseRun
               , formatTime
               , parseUser
               , rank) where

import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
-- Just imported for resolvename, which has to be here cause of Circular depends
import Network.HTTP.Conduit (simpleHttp)


type Json = IO ByteString
type Url = String

fetchJSON :: Url -> Json
fetchJSON = simpleHttp


liftJson :: FromJSON a => (a -> b) -> Json -> IO (Either String b)
liftJson = fmap . (. eitherDecode) . fmap


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
parseCategories = liftJson (map catName . cats)


parseLeaderboard :: String -> Json -> IO (Either String Url)
parseLeaderboard cat = fmap (eitherDecode >=> getCat)
  where
    getCat catData = case filter ((== cat) . catName) (cats catData) of
      [] -> Left "That category doesn't exist"
      [cat] -> Right (leaderboard cat)


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
parseGameWith = liftJson


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
        <*> (run    >>= (.: "times")  >>= (.: "primary_t"))
        <*> ((.: "uri") =<< (head <$>
            (run    >>= (.: "videos") >>= (.: "links"))))
        where
          run    = o .: "run"
          player = head <$> (o .: "run" >>= (.: "players"))

parseRun :: Int -> Json -> IO (Either String Run)
parseRun place = liftJson ((!! place) . runs)


formatTime :: Seconds -> String
formatTime n = addColon (show hours) ++
               addColon (pad mins) ++
               pad secs
  where
    (m, secs)     = n `divMod` 60
    (hours, mins) = m `divMod` 60
    pad :: Int -> String
    pad n = (if n < 10 then "0" else "") ++ show n
    addColon :: String -> String
    addColon "0" = ""
    addColon n = n ++ ":"


data User = User { userName :: String }
instance FromJSON User where
    parseJSON (Object o) = User
        <$> ((o .: "data") >>= (.: "names") >>= (.: "international"))

parseUser :: Json -> IO (Either String String)
parseUser = liftJson userName


rank :: Int -> String   --shifted, because place is used as the run Index
rank 0 = "WR"
rank 1 = "2nd place time"
rank 2 = "3rd place time"
rank n = show (n + 1) ++ "th place time"
