{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Card.CardMedia where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data CardMedia = CardMedia {
    cardMediaActAsExpander :: !(Maybe Bool)
    , cardMediaExpandable :: !(Maybe Bool)
} deriving (Generic, Show)

instance ToJSON CardMedia where
  toJSON = genericToJSON $ aesonDrop (length ("CardMedia" :: String)) snakeCase


defCardMedia ::
 CardMedia
defCardMedia  =
  CardMedia {
      cardMediaActAsExpander = Nothing
      , cardMediaExpandable = Nothing
  }

cardMedia_ ::
  CardMedia ->
  [PropertyOrHandler handler] ->
  ReactElementM handler () ->
  ReactElementM handler ()
cardMedia_ args props =
   foreign_
   "CardMedia"
   (fromMaybe [] (toProps args) ++ props)
