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
    , Estado(..)
    , Precios(..)
    , Ticket(..)
    , ComprarRequest(..)
    , montoOperacion
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(Object), (.:), genericParseJSON, genericToJSON, defaultOptions)
import Control.Monad (mzero)
import GHC.Generics
import Data.Time.Clock (UTCTime)

-- Tipo de dato para almacenar el token
newtype AuthResponse = AuthResponse { accessToken :: String }
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

-- Tipos para el sistema de trading de tickets
data Estado
    = Waiting
    | FirstBuy
    | SecondBuy
    | FirstSell
    | SecondSell
    | StopLoss
    | TakeProfit
    deriving (Show, Read, Eq)

data Precios = Precios
    { compra1       :: Double
    , compra2       :: Double
    , venta1        :: Double
    , venta2        :: Double
    , takeProfit    :: Double
    , stopLoss      :: Double
    } deriving (Show, Read, Eq)

data Ticket = Ticket
    { ticketName    :: String
    , estado        :: Estado
    , precios       :: Precios
    , puntaCompra   :: Double
    , puntaVenta    :: Double
    , lastUpdate    :: UTCTime
    } deriving (Show, Read, Eq)

-- Tipo para la orden de compra
data ComprarRequest = ComprarRequest
    { compraMercado :: String
    , compraSimbolo :: String
    , compraCantidad :: Int
    , compraPrecio :: Double
    , compraPlazo :: String
    , compraValidez :: String
    , compraTipoOrden :: String
    } deriving (Show, Generic)

instance ToJSON ComprarRequest where
    toJSON = genericToJSON defaultOptions

-- Configuración global
montoOperacion :: Double
montoOperacion = 10000.0  -- Valor inicial, ajustar según necesidad