{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Table.TableFooter where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data TableFooter = TableFooter
  { tableFooterAdjustForCheckbox :: !(Maybe Bool)
  , tableFooterClassName :: !(Maybe Text)
  } deriving (Generic, Show)

instance ToJSON TableFooter where
  toJSON =
    genericToJSON $ aesonDrop (length ("TableFooter" :: String)) snakeCase

defTableFooter :: TableFooter
defTableFooter =
  TableFooter
  {tableFooterAdjustForCheckbox = Just True, tableFooterClassName = Nothing}

tableFooter_ ::
     TableFooter
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
tableFooter_ args props =
  foreign_ "TableFooter" (fromMaybe [] (toProps args) ++ props)
