{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.SvgIcon where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data SvgIcon = SvgIcon
  { svgIconColor :: !(Maybe Text)
  , svgIconHoverColor :: !(Maybe Text)
  , svgIconViewBox :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON SvgIcon where
  toJSON = genericToJSON $ aesonDrop (length ("SvgIcon" :: String)) camelCase

defSvgIcon :: SvgIcon
defSvgIcon =
  SvgIcon
  { svgIconColor = Nothing
  , svgIconHoverColor = Nothing
  , svgIconViewBox = Just "0 0 24 24"
  }

svgIcon_ ::
     SvgIcon
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
svgIcon_ args props = foreign_ "SvgIcon" (fromMaybe [] (toProps args) ++ props)
