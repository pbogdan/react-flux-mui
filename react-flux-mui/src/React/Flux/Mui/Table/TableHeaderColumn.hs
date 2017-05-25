{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Table.TableHeaderColumn where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data TableHeaderColumn = TableHeaderColumn
  { tableHeaderColumnClassName :: !(Maybe Text)
  , tableHeaderColumnColumnNumber :: !(Maybe Integer)
  , tableHeaderColumnHoverable :: !(Maybe Bool)
  , tableHeaderColumnTooltip :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON TableHeaderColumn where
  toJSON =
    genericToJSON $ aesonDrop (length ("TableHeaderColumn" :: String)) snakeCase

defTableHeaderColumn :: TableHeaderColumn
defTableHeaderColumn =
  TableHeaderColumn
  { tableHeaderColumnClassName = Nothing
  , tableHeaderColumnColumnNumber = Nothing
  , tableHeaderColumnHoverable = Nothing
  , tableHeaderColumnTooltip = Nothing
  }

tableHeaderColumn_ ::
     TableHeaderColumn
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
tableHeaderColumn_ args props =
  foreign_ "TableHeaderColumn" (fromMaybe [] (toProps args) ++ props)
