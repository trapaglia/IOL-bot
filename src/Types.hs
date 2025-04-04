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
    , OrdenRequest(..)
    , PortfolioResponse(..)
    , PortfolioAsset(..)
    , AssetTitle(..)
    , AssetParking(..)
    , DolarMEP(..)
    , montoOperacion
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(Object), (.:), genericParseJSON, genericToJSON, defaultOptions)
import Control.Monad (mzero)
import GHC.Generics
import Data.Time.Clock (UTCTime)

-- Tipos para el portafolio
data AssetTitle = AssetTitle
    { tituloSimbolo :: String
    , tituloDescripcion :: String
    , tituloPais :: String
    , tituloMercado :: String
    , tituloTipo :: String
    , tituloPlazo :: String
    , tituloMoneda :: String
    } deriving (Show, Generic)

instance FromJSON AssetTitle where
    parseJSON = genericParseJSON defaultOptions

data AssetParking = AssetParking
    { disponibleInmediato :: Double
    } deriving (Show, Generic)

instance FromJSON AssetParking where
    parseJSON = genericParseJSON defaultOptions

data PortfolioAsset = PortfolioAsset
    { assetCantidad :: Double
    , assetComprometido :: Double
    , assetPuntosVariacion :: Double
    , assetVariacionDiaria :: Double
    , assetUltimoPrecio :: Double
    , assetPpc :: Double
    , assetGananciaPorcentaje :: Double
    , assetGananciaDinero :: Double
    , assetValorizado :: Double
    , assetTitulo :: AssetTitle
    , assetParking :: AssetParking
    } deriving (Show, Generic)

instance FromJSON PortfolioAsset where
    parseJSON = genericParseJSON defaultOptions

data PortfolioResponse = PortfolioResponse
    { portfolioPais :: String
    , portfolioActivos :: [PortfolioAsset]
    } deriving (Show, Generic)

instance FromJSON PortfolioResponse where
    parseJSON = genericParseJSON defaultOptions

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
data OrdenRequest = OrdenRequest
    { ordenMercado :: String
    , ordenSimbolo :: String
    , ordenCantidad :: Int
    , ordenPrecio :: Double
    , ordenPlazo :: String
    , ordenValidez :: String
    , ordenTipoOrden :: String
    } deriving (Show, Generic)

instance ToJSON OrdenRequest where
    toJSON = genericToJSON defaultOptions

-- Configuración global
montoOperacion :: Double
montoOperacion = 120000.0  -- Valor inicial, ajustar según necesidad

-- Tipo para la cotización del dólar MEP
newtype DolarMEP = DolarMEP Double
    deriving (Show, Generic)

instance FromJSON DolarMEP where
    parseJSON = genericParseJSON defaultOptions