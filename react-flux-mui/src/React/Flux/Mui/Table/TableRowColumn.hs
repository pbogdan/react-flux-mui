{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Table.TableRowColumn where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data TableRowColumn = TableRowColumn
  { tableRowColumnClassName :: !(Maybe Text)
  , tableRowColumnColumnNumber :: !(Maybe Integer)
  , tableRowColumnHoverable :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON TableRowColumn where
  toJSON =
    genericToJSON $ aesonDrop (length ("TableRowColumn" :: String)) camelCase

defTableRowColumn :: TableRowColumn
defTableRowColumn =
  TableRowColumn
  { tableRowColumnClassName = Nothing
  , tableRowColumnColumnNumber = Nothing
  , tableRowColumnHoverable = Just False
  }

tableRowColumn_ ::
     TableRowColumn
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
tableRowColumn_ args props =
  foreign_ "TableRowColumn" (fromMaybe [] (toProps args) ++ props)
