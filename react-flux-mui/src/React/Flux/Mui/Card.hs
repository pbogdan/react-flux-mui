{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Card where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data Card = Card
  { cardExpandable :: !(Maybe Bool)
  , cardExpanded :: !(Maybe Bool)
  , cardInitiallyExpanded :: !(Maybe Bool)
  , cardShowExpandableButton :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON Card where
  toJSON = genericToJSON $ aesonDrop (length ("Card" :: String)) snakeCase

defCard :: Card
defCard =
  Card
  { cardExpandable = Just False
  , cardExpanded = Nothing
  , cardInitiallyExpanded = Just False
  , cardShowExpandableButton = Nothing
  }

card_ ::
     Card
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
card_ args props = foreign_ "Card" (fromMaybe [] (toProps args) ++ props)
