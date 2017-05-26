{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Card.CardText where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data CardText = CardText
  { cardTextActAsExpander :: !(Maybe Bool)
  , cardTextColor :: !(Maybe Text)
  , cardTextExpandable :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON CardText where
  toJSON = genericToJSON $ aesonDrop (length ("CardText" :: String)) camelCase

defCardText :: CardText
defCardText =
  CardText
  { cardTextActAsExpander = Nothing
  , cardTextColor = Nothing
  , cardTextExpandable = Nothing
  }

cardText_ ::
     CardText
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
cardText_ args props =
  foreign_ "CardText" (fromMaybe [] (toProps args) ++ props)
