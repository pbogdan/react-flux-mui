{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.FontIcon where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data FontIcon = FontIcon
  { fontIconColor :: !(Maybe Text)
  , fontIconHoverColor :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON FontIcon where
  toJSON = genericToJSON $ aesonDrop (length ("FontIcon" :: String)) camelCase

defFontIcon :: FontIcon
defFontIcon = FontIcon {fontIconColor = Nothing, fontIconHoverColor = Nothing}

fontIcon_ ::
     FontIcon
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
fontIcon_ args props =
  foreign_ "FontIcon" (fromMaybe [] (toProps args) ++ props)
