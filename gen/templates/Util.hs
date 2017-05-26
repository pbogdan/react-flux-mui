{-# LANGUAGE CPP #-}

module React.Flux.Mui.Util where

import           Protolude

import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import           React.Flux
#ifdef __GHCJS__
import           Data.JSString.Text
import           Data.JSString (JSString)
#else
import           Data.String (String)
#endif

#ifdef __GHCJS__
toKey :: Text -> JSString
toKey = textToJSString
#else
toKey ::  Text -> String
toKey = toS
#endif

toProps
  :: (ToJSON a)
  => a -> Maybe [PropertyOrHandler handler]
toProps j =
  case toJSON j of
    Object o -> Just . map (\(k, v) -> toKey k &= v) . HashMap.toList $ o
    _ -> Nothing
