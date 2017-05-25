{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.GridList where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data GridList = GridList
  { gridListCellHeight :: !(Maybe ((Integer :|: MuiSymbolEnum '[ "auto"])))
  , gridListCols :: !(Maybe Integer)
  , gridListPadding :: !(Maybe Integer)
  } deriving (Generic, Show)

instance ToJSON GridList where
  toJSON = genericToJSON $ aesonDrop (length ("GridList" :: String)) snakeCase

defGridList :: (Maybe ((Integer :|: MuiSymbolEnum '[ "auto"]))) -> GridList
defGridList gridListCellHeight_ =
  GridList
  { gridListCellHeight = gridListCellHeight_
  , gridListCols = Just 2
  , gridListPadding = Just 4
  }

gridList_ ::
     GridList
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
gridList_ args props =
  foreign_ "GridList" (fromMaybe [] (toProps args) ++ props)
