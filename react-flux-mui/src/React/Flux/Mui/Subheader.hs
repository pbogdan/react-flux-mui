{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Subheader where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data Subheader = Subheader
  { subheaderInset :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON Subheader where
  toJSON = genericToJSON $ aesonDrop (length ("Subheader" :: String)) snakeCase

defSubheader :: Subheader
defSubheader = Subheader {subheaderInset = Just False}

subheader_ ::
     Subheader
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
subheader_ args props =
  foreign_ "Subheader" (fromMaybe [] (toProps args) ++ props)
