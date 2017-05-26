{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module React.Flux.Mui.Table.TableHeader where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Util

data TableHeader = TableHeader
  { tableHeaderAdjustForCheckbox :: !(Maybe Bool)
  , tableHeaderClassName :: !(Maybe Text)
  , tableHeaderDisplaySelectAll :: !(Maybe Bool)
  , tableHeaderEnableSelectAll :: !(Maybe Bool)
  , tableHeaderSelectAllSelected :: !(Maybe Bool)
  } deriving (Generic, Show)

instance ToJSON TableHeader where
  toJSON =
    genericToJSON $ aesonDrop (length ("TableHeader" :: String)) camelCase

defTableHeader :: TableHeader
defTableHeader =
  TableHeader
  { tableHeaderAdjustForCheckbox = Just True
  , tableHeaderClassName = Nothing
  , tableHeaderDisplaySelectAll = Just True
  , tableHeaderEnableSelectAll = Just True
  , tableHeaderSelectAllSelected = Just False
  }

tableHeader_ ::
     TableHeader
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
tableHeader_ args props =
  foreign_ "TableHeader" (fromMaybe [] (toProps args) ++ props)
