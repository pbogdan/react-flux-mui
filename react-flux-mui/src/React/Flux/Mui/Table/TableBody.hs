{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Table.TableBody where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data TableBody = TableBody
  { tableBodyAllRowsSelected :: !(Maybe Bool)
  , tableBodyClassName :: !(Maybe Text)
  , tableBodyDeselectOnClickaway :: !(Maybe Bool)
  , tableBodyDisplayRowCheckbox :: !(Maybe Bool)
  , tableBodyMultiSelectable :: !(Maybe Bool)
  , tableBodyPreScanRows :: !(Maybe Bool)
  , tableBodySelectable :: !(Maybe Bool)
  , tableBodyShowRowHover :: !(Maybe Bool)
  , tableBodyStripedRows :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON TableBody where
  toJSON = genericToJSON $ aesonDrop (length ("TableBody" :: String)) snakeCase

defTableBody :: TableBody
defTableBody =
  TableBody
  { tableBodyAllRowsSelected = Just False
  , tableBodyClassName = Nothing
  , tableBodyDeselectOnClickaway = Just True
  , tableBodyDisplayRowCheckbox = Just True
  , tableBodyMultiSelectable = Just False
  , tableBodyPreScanRows = Just True
  , tableBodySelectable = Just True
  , tableBodyShowRowHover = Nothing
  , tableBodyStripedRows = Nothing
  }

tableBody_ ::
     TableBody
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
tableBody_ args props =
  foreign_ "TableBody" (fromMaybe [] (toProps args) ++ props)
