{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Tabs where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data Tabs = Tabs
  { tabsClassName :: !(Maybe Text)
  , tabsContentContainerClassName :: !(Maybe Text)
  , tabsInitialSelectedIndex :: !(Maybe Integer)
  } deriving (Generic, Show)

instance ToJSON Tabs where
  toJSON = genericToJSON $ aesonDrop (length ("Tabs" :: String)) snakeCase

defTabs :: Tabs
defTabs =
  Tabs
  { tabsClassName = Nothing
  , tabsContentContainerClassName = Nothing
  , tabsInitialSelectedIndex = Just 0
  }

tabs_ ::
     Tabs
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
tabs_ args props = foreign_ "Tabs" (fromMaybe [] (toProps args) ++ props)
