{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Table.TableRow where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data TableRow = TableRow
  { tableRowClassName :: !(Maybe Text)
  , tableRowDisplayBorder :: !(Maybe Bool)
  , tableRowHoverable :: !(Maybe Bool)
  , tableRowHovered :: !(Maybe Bool)
  , tableRowRowNumber :: !(Maybe Integer)
  , tableRowSelectable :: !(Maybe Bool)
  , tableRowSelected :: !(Maybe Bool)
  , tableRowStriped :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON TableRow where
  toJSON = genericToJSON $ aesonDrop (length ("TableRow" :: String)) camelCase

defTableRow :: TableRow
defTableRow =
  TableRow
  { tableRowClassName = Nothing
  , tableRowDisplayBorder = Just True
  , tableRowHoverable = Just False
  , tableRowHovered = Just False
  , tableRowRowNumber = Nothing
  , tableRowSelectable = Just True
  , tableRowSelected = Just False
  , tableRowStriped = Just False
  }

tableRow_ ::
     TableRow
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
tableRow_ args props =
  foreign_ "TableRow" (fromMaybe [] (toProps args) ++ props)
