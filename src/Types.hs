{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types 
    ( AuthResponse(..)
    , ApiConfig(..)
    , CotizacionDetalle(..)
    , Punta(..)
    , EstadoCuenta(..)
    , Cuenta(..)
    , SaldoDetalle(..)
    ) where

import Data.Aeson (FromJSON(..), Value(Object), (.:), genericParseJSON, defaultOptions)
import Control.Monad (mzero)
import GHC.Generics

-- Tipo de dato para almacenar el token
data AuthResponse = AuthResponse { accessToken :: String }
    deriving (Show, Generic)

instance FromJSON AuthResponse where
    parseJSON (Object v) = AuthResponse <$> v .: "access_token"
    parseJSON _ = mzero

-- Tipos para la cotización
data Punta = Punta
    { precioCompra :: Double
    , precioVenta :: Double
    } deriving (Show, Generic)

instance FromJSON Punta where
    parseJSON = genericParseJSON defaultOptions

data CotizacionDetalle = CotizacionDetalle
    { ultimoPrecio :: Double
    , puntas :: [Punta]
    } deriving (Show, Generic)

instance FromJSON CotizacionDetalle where
    parseJSON = genericParseJSON defaultOptions

-- Configuración para las llamadas a la API
data ApiConfig = ApiConfig 
    { username :: String
    , password :: String
    }

-- Tipos para el estado de cuenta
data SaldoDetalle = SaldoDetalle
    { saldo :: Double
    , comprometido :: Double
    , disponible :: Double
    } deriving (Show, Generic)

instance FromJSON SaldoDetalle where
    parseJSON = genericParseJSON defaultOptions

data Cuenta = Cuenta
    { numero :: String
    , tipo :: String
    , moneda :: String
    , saldos :: [SaldoDetalle]
    } deriving (Show, Generic)

instance FromJSON Cuenta where
    parseJSON = genericParseJSON defaultOptions

data EstadoCuenta = EstadoCuenta
    { cuentas :: [Cuenta]
    } deriving (Show, Generic)

instance FromJSON EstadoCuenta where
    parseJSON = genericParseJSON defaultOptions
