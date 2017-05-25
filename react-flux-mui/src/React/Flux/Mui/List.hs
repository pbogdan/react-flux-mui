{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.List where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data List = List {
} deriving (Generic, Show)

instance ToJSON List where
  toJSON = genericToJSON $ aesonDrop (length ("List" :: String)) snakeCase


defList ::
 List
defList  =
  List {
  }

list_ ::
  List ->
  [PropertyOrHandler handler] ->
  ReactElementM handler () ->
  ReactElementM handler ()
list_ args props =
   foreign_
   "List"
   (fromMaybe [] (toProps args) ++ props)
