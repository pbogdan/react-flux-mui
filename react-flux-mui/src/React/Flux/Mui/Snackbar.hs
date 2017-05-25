{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module React.Flux.Mui.Snackbar where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data Snackbar = Snackbar
  { snackbarAutoHideDuration :: !(Maybe Integer)
  , snackbarClassName :: !(Maybe Text)
  , snackbarOpen :: !Bool
  } deriving (Generic, Show)

instance ToJSON Snackbar where
  toJSON = genericToJSON $ aesonDrop (length ("Snackbar" :: String)) snakeCase

defSnackbar :: Bool -> Snackbar
defSnackbar snackbarOpen_ =
  Snackbar
  { snackbarAutoHideDuration = Nothing
  , snackbarClassName = Nothing
  , snackbarOpen = snackbarOpen_
  }

snackbar_ ::
     Snackbar
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
snackbar_ args props =
  foreign_ "Snackbar" (fromMaybe [] (toProps args) ++ props)
