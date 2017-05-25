{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.FloatingActionButton where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data FloatingActionButton = FloatingActionButton
  { floatingActionButtonBackgroundColor :: !(Maybe Text)
  , floatingActionButtonClassName :: !(Maybe Text)
  , floatingActionButtonDisabled :: !(Maybe Bool)
  , floatingActionButtonDisabledColor :: !(Maybe Text)
  , floatingActionButtonHref :: !(Maybe Text)
  , floatingActionButtonIconClassName :: !(Maybe Text)
  , floatingActionButtonMini :: !(Maybe Bool)
  , floatingActionButtonSecondary :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON FloatingActionButton where
  toJSON =
    genericToJSON $
    aesonDrop (length ("FloatingActionButton" :: String)) snakeCase

defFloatingActionButton :: FloatingActionButton
defFloatingActionButton =
  FloatingActionButton
  { floatingActionButtonBackgroundColor = Nothing
  , floatingActionButtonClassName = Nothing
  , floatingActionButtonDisabled = Just False
  , floatingActionButtonDisabledColor = Nothing
  , floatingActionButtonHref = Nothing
  , floatingActionButtonIconClassName = Nothing
  , floatingActionButtonMini = Just False
  , floatingActionButtonSecondary = Just False
  }

floatingActionButton_ ::
     FloatingActionButton
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
floatingActionButton_ args props =
  foreign_ "FloatingActionButton" (fromMaybe [] (toProps args) ++ props)
