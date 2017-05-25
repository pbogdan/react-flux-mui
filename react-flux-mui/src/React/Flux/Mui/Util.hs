module React.Flux.Mui.Util where

import           Protolude

import           Data.Aeson
import qualified Data.HashMap.Strict as HashMap
import           React.Flux

toProps
  :: (ToJSON a)
  => a -> Maybe [PropertyOrHandler handler]
toProps j =
  case toJSON j of
    Object o -> Just . map (\(k, v) -> toS k &= v) . HashMap.toList $ o
    _ -> Nothing
