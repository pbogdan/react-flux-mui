{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.GridList.GridTile where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data GridTile = GridTile
  { gridTileActionPosition :: !(Maybe (MuiSymbolEnum '[ "left", "right"]))
  , gridTileCols :: !(Maybe Integer)
  , gridTileRows :: !(Maybe Integer)
  , gridTileTitleBackground :: !(Maybe Text)
  , gridTileTitlePosition :: !(Maybe (MuiSymbolEnum '[ "top", "bottom"]))
  } deriving (Generic, Show)

instance ToJSON GridTile where
  toJSON = genericToJSON $ aesonDrop (length ("GridTile" :: String)) camelCase

defGridTile :: GridTile
defGridTile =
  GridTile
  { gridTileActionPosition = Just (MuiSymbolEnum (Proxy :: Proxy "right"))
  , gridTileCols = Just 1
  , gridTileRows = Just 1
  , gridTileTitleBackground = Just ("rgba(0, 0, 0, 0.4)")
  , gridTileTitlePosition = Just (MuiSymbolEnum (Proxy :: Proxy "bottom"))
  }

gridTile_ ::
     GridTile
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
gridTile_ args props =
  foreign_ "GridTile" (fromMaybe [] (toProps args) ++ props)
