{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Card.CardTitle where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data CardTitle = CardTitle
  { cardTitleActAsExpander :: !(Maybe Bool)
  , cardTitleExpandable :: !(Maybe Bool)
  , cardTitleShowExpandableButton :: !(Maybe Bool)
  , cardTitleSubtitleColor :: !(Maybe Text)
  , cardTitleTitleColor :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON CardTitle where
  toJSON = genericToJSON $ aesonDrop (length ("CardTitle" :: String)) snakeCase

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
