{-# LANGUAGE OverloadedStrings #-}
module Parsing (parseCategories) where

import Data.Aeson
import Data.ByteString.Lazy (ByteString)

type Json = IO ByteString

data CategoryData = CategoryData { cats :: [Category] } deriving (Show)
instance FromJSON CategoryData where
    parseJSON (Object o) = CategoryData <$> (o .: "data")


data Category = Category { name :: String} deriving (Show)
instance FromJSON Category where
    parseJSON (Object o) = Category <$> (o .: "name")


parseCategories :: Json -> IO (Either String [String])
parseCategories json = do
    catData <- (eitherDecode <$> json) :: IO (Either String CategoryData)
    return $ catData >>= \catData -> return $ map name $ cats catData
