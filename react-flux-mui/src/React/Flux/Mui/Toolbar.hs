{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Toolbar where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data Toolbar = Toolbar
  { toolbarClassName :: !(Maybe Text)
  , toolbarNoGutter :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON Toolbar where
  toJSON = genericToJSON $ aesonDrop (length ("Toolbar" :: String)) snakeCase

defToolbar :: Toolbar
defToolbar = Toolbar {toolbarClassName = Nothing, toolbarNoGutter = Just False}

toolbar_ ::
     Toolbar
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
toolbar_ args props = foreign_ "Toolbar" (fromMaybe [] (toProps args) ++ props)
