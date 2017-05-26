{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module React.Flux.Mui.CircularProgress where

import Protolude

import Data.Aeson
import Data.Aeson.Casing
import Data.String (String)
import React.Flux
import React.Flux.Mui.Types
import React.Flux.Mui.Util

data CircularProgress = CircularProgress
  { circularProgressColor :: !(Maybe Text)
  , circularProgressMax :: !(Maybe Integer)
  , circularProgressMin :: !(Maybe Integer)
  , circularProgressMode :: !(Maybe (MuiSymbolEnum '[ "determinate", "indeterminate"]))
  , circularProgressSize :: !(Maybe Integer)
  , circularProgressThickness :: !(Maybe Integer)
  , circularProgressValue :: !(Maybe Integer)
  } deriving (Generic, Show)

instance ToJSON CircularProgress where
  toJSON =
    genericToJSON $ aesonDrop (length ("CircularProgress" :: String)) camelCase

defCircularProgress :: (Maybe Integer) -> CircularProgress
defCircularProgress circularProgressThickness_ =
  CircularProgress
  { circularProgressColor = Nothing
  , circularProgressMax = Just 100
  , circularProgressMin = Just 0
  , circularProgressMode = Just (MuiSymbolEnum (Proxy :: Proxy "indeterminate"))
  , circularProgressSize = Just 40
  , circularProgressThickness = circularProgressThickness_
  , circularProgressValue = Just 0
  }

circularProgress_ ::
     CircularProgress
  -> [PropertyOrHandler handler]
  -> ReactElementM handler ()
  -> ReactElementM handler ()
circularProgress_ args props =
  foreign_ "CircularProgress" (fromMaybe [] (toProps args) ++ props)
