{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Menu where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data Menu = Menu
  { menuAutoWidth :: !(Maybe Bool)
  , menuDesktop :: !(Maybe Bool)
  , menuDisableAutoFocus :: !(Maybe Bool)
  , menuInitiallyKeyboardFocused :: !(Maybe Bool)
  , menuMaxHeight :: !(Maybe Integer)
  , menuMultiple :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON Menu where
  toJSON = genericToJSON $ aesonDrop (length ("Menu" :: String)) snakeCase

defMenu :: Menu
defMenu =
  Menu
  { menuAutoWidth = Just True
  , menuDesktop = Just False
  , menuDisableAutoFocus = Just False
  , menuInitiallyKeyboardFocused = Just False
  , menuMaxHeight = Nothing
  , menuMultiple = Just False
  }

menu_ ::
     Menu
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
menu_ args props = foreign_ "Menu" (fromMaybe [] (toProps args) ++ props)
