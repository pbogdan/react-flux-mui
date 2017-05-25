{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test where

import Protolude

import Data.Aeson
import Data.Kind (Constraint)
import GHC.TypeLits

type family Or (a :: Bool) (b :: Bool) :: Bool where
  Or 'False 'False = 'False
  Or 'True 'False = 'True
  Or 'False 'True = 'True
  Or 'True 'True = 'True

type family IsOneOf (n :: k) (xs :: [k]) :: Bool where
  IsOneOf n '[] = 'False
  IsOneOf n (x ': xs) = Or (n == x) (IsOneOf n xs)

type family Member n xs :: Constraint where
  Member n xs = 'True ~ IsOneOf n xs

data MuiNatEnum (xs :: [Nat]) where
        MuiNatEnum :: (KnownNat n, Member n xs) => Proxy n -> MuiNatEnum xs

deriving instance Show (MuiNatEnum xs)

instance ToJSON (MuiNatEnum xs) where
  toJSON (MuiNatEnum n) = Number . fromIntegral . natVal $ n

data MuiSymbolEnum (xs :: [Symbol]) where
        MuiSymbolEnum ::
            (KnownSymbol s, Member s xs) => Proxy s -> MuiSymbolEnum xs

deriving instance Show (MuiSymbolEnum xs)

instance ToJSON (MuiSymbolEnum xs) where
  toJSON (MuiSymbolEnum s) = String . toS . symbolVal $ s
