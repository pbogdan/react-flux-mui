{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Tabs.Tab where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data Tab = Tab
  { tabClassName :: !(Maybe Text)
  , tabSelected :: !(Maybe Bool)
  , tabWidth :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON Tab where
  toJSON = genericToJSON $ aesonDrop (length ("Tab" :: String)) snakeCase

defTab :: Tab
defTab = Tab {tabClassName = Nothing, tabSelected = Nothing, tabWidth = Nothing}

tab_ ::
     Tab
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
tab_ args props = foreign_ "Tab" (fromMaybe [] (toProps args) ++ props)
