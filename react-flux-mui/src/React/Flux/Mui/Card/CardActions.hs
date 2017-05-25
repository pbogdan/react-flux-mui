{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Card.CardActions where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data CardActions = CardActions
  { cardActionsActAsExpander :: !(Maybe Bool)
  , cardActionsExpandable :: !(Maybe Bool)
  , cardActionsShowExpandableButton :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON CardActions where
  toJSON =
    genericToJSON $ aesonDrop (length ("CardActions" :: String)) snakeCase

defCardActions :: CardActions
defCardActions =
  CardActions
  { cardActionsActAsExpander = Nothing
  , cardActionsExpandable = Nothing
  , cardActionsShowExpandableButton = Nothing
  }

cardActions_ ::
     CardActions
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
cardActions_ args props =
  foreign_ "CardActions" (fromMaybe [] (toProps args) ++ props)
