{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Card.CardExpandable where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data CardExpandable = CardExpandable
  { cardExpandableExpanded :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON CardExpandable where
  toJSON =
    genericToJSON $ aesonDrop (length ("CardExpandable" :: String)) snakeCase

defCardExpandable :: CardExpandable
defCardExpandable = CardExpandable {cardExpandableExpanded = Nothing}

cardExpandable_ ::
     CardExpandable
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
cardExpandable_ args props =
  foreign_ "CardExpandable" (fromMaybe [] (toProps args) ++ props)
