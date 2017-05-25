{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Card.CardHeader where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data CardHeader = CardHeader
  { cardHeaderActAsExpander :: !(Maybe Bool)
  , cardHeaderExpandable :: !(Maybe Bool)
  , cardHeaderShowExpandableButton :: !(Maybe Bool)
  , cardHeaderSubtitleColor :: !(Maybe Text)
  , cardHeaderTitleColor :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON CardHeader where
  toJSON = genericToJSON $ aesonDrop (length ("CardHeader" :: String)) snakeCase

defCardHeader :: CardHeader
defCardHeader =
  CardHeader
  { cardHeaderActAsExpander = Nothing
  , cardHeaderExpandable = Nothing
  , cardHeaderShowExpandableButton = Nothing
  , cardHeaderSubtitleColor = Nothing
  , cardHeaderTitleColor = Nothing
  }

cardHeader_ ::
     CardHeader
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
cardHeader_ args props =
  foreign_ "CardHeader" (fromMaybe [] (toProps args) ++ props)
