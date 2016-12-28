{-# LANGUAGE OverloadedStrings #-}
module Parsing ( parseCategories
               , Game (..)
               , parseGameWith) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)

type Json = IO ByteString

data CategoryData = CategoryData { cats :: [Category] } deriving (Show)
instance FromJSON CategoryData where
    parseJSON (Object o) = CategoryData <$> (o .: "data")


data Category = Category { catName :: String} deriving (Show)
instance FromJSON Category where
    parseJSON (Object o) = Category <$> (o .: "name")


parseCategories :: Json -> IO (Either String [String])
parseCategories json = do
    catData <- (eitherDecode <$> json) :: IO (Either String CategoryData)
    return $ catData >>= \catData -> return $ map catName $ cats catData


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
