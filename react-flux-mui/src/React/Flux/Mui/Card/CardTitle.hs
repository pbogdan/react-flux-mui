{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Card.CardTitle where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data CardTitle = CardTitle
  { cardTitleActAsExpander :: !(Maybe Bool)
  , cardTitleExpandable :: !(Maybe Bool)
  , cardTitleShowExpandableButton :: !(Maybe Bool)
  , cardTitleSubtitleColor :: !(Maybe Text)
  , cardTitleTitleColor :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON CardTitle where
  toJSON = genericToJSON $ aesonDrop (length ("CardTitle" :: String)) camelCase

defCardTitle :: CardTitle
defCardTitle =
  CardTitle
  { cardTitleActAsExpander = Nothing
  , cardTitleExpandable = Nothing
  , cardTitleShowExpandableButton = Nothing
  , cardTitleSubtitleColor = Nothing
  , cardTitleTitleColor = Nothing
  }

cardTitle_ ::
     CardTitle
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
cardTitle_ args props =
  foreign_ "CardTitle" (fromMaybe [] (toProps args) ++ props)
