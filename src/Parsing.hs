{-# LANGUAGE OverloadedStrings #-}
module Parsing
    ( CategoryData
    , Category)
    where

import Data.Aeson


data CategoryData = CategoryData { cData :: !Array }
instance FromJSON CategoryData where
    parseJSON (Object o) = CategoryData <$> (o .: "data")


data Category = Category { name :: String}
instance FromJSON Category where
    parseJSON (Object o) = Category <$> (o .: "name")
