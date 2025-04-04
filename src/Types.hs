{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types 
    ( AuthResponse(..)
    , ApiConfig(..)
    , CotizacionDetalle(..)
    , Punta(..)
    , PuntaInstrumento(..)
    , Instrumento(..)
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
    , Operacion(..)
    , CotizacionesResponse(..)
    , montoOperacion
    ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(Object), (.:), genericParseJSON, genericToJSON, defaultOptions, Options(..))
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
        { fieldLabelModifier = \s -> case drop 6 s of  -- quita el prefijo "titulo"
            "Simbolo" -> "simbolo"
            "Descripcion" -> "descripcion"
            "Pais" -> "pais"
            "Mercado" -> "mercado"
            "Tipo" -> "tipo"
            "Plazo" -> "plazo"
            "Moneda" -> "moneda"
            x -> x
        }

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
    , assetParking :: Maybe AssetParking
    } deriving (Show, Generic)

instance FromJSON PortfolioAsset where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \s -> case drop 5 s of  -- quita el prefijo "asset"
            "Cantidad" -> "cantidad"
            "Comprometido" -> "comprometido"
            "PuntosVariacion" -> "puntosVariacion"
            "VariacionDiaria" -> "variacionDiaria"
            "UltimoPrecio" -> "ultimoPrecio"
            "Ppc" -> "ppc"
            "GananciaPorcentaje" -> "gananciaPorcentaje"
            "GananciaDinero" -> "gananciaDinero"
            "Valorizado" -> "valorizado"
            "Titulo" -> "titulo"
            "Parking" -> "parking"
            x -> x
        }

data PortfolioResponse = PortfolioResponse
    { portfolioPais :: String
    , portfolioActivos :: [PortfolioAsset]
    } deriving (Show, Generic)

instance FromJSON PortfolioResponse where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \s -> case drop 9 s of  -- quita el prefijo "portfolio"
            "Pais" -> "pais"
            "Activos" -> "activos"
            x -> x
        }

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

data PuntaInstrumento = PuntaInstrumento
    { puntaInstCantidadCompra :: Int
    , puntaInstPrecioCompra :: Double
    , puntaInstPrecioVenta :: Double
    , puntaInstCantidadVenta :: Int
    } deriving (Show, Generic)

instance FromJSON PuntaInstrumento where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \s -> case drop 8 s of  -- quita el prefijo "puntaInst"
            "CantidadCompra" -> "cantidadCompra"
            "PrecioCompra" -> "precioCompra"
            "PrecioVenta" -> "precioVenta"
            "CantidadVenta" -> "cantidadVenta"
            x -> x
        }

data Instrumento = Instrumento
    { instSimbolo :: String
    , instPuntas :: PuntaInstrumento
    , instUltimoPrecio :: Double
    , instVariacionPorcentual :: Double
    , instApertura :: Double
    , instMaximo :: Double
    , instMinimo :: Double
    , instUltimoCierre :: Double
    , instVolumen :: Int
    , instCantidadOperaciones :: Int
    , instFecha :: String
    , instTipoOpcion :: Maybe String
    , instPrecioEjercicio :: Maybe Double
    , instFechaVencimiento :: Maybe String
    , instMercado :: String
    , instMoneda :: String
    , instDescripcion :: String
    , instPlazo :: String
    , instLaminaMinima :: Int
    , instLote :: Int
    } deriving (Show, Generic)

instance FromJSON Instrumento where
    parseJSON = genericParseJSON defaultOptions
        { fieldLabelModifier = \s -> case drop 4 s of  -- quita el prefijo "inst"
            "Simbolo" -> "simbolo"
            "Puntas" -> "puntas"
            "UltimoPrecio" -> "ultimoPrecio"
            "VariacionPorcentual" -> "variacionPorcentual"
            "Apertura" -> "apertura"
            "Maximo" -> "maximo"
            "Minimo" -> "minimo"
            "UltimoCierre" -> "ultimoCierre"
            "Volumen" -> "volumen"
            "CantidadOperaciones" -> "cantidadOperaciones"
            "Fecha" -> "fecha"
            "TipoOpcion" -> "tipoOpcion"
            "PrecioEjercicio" -> "precioEjercicio"
            "FechaVencimiento" -> "fechaVencimiento"
            "Mercado" -> "mercado"
            "Moneda" -> "moneda"
            "Descripcion" -> "descripcion"
            "Plazo" -> "plazo"
            "LaminaMinima" -> "laminaMinima"
            "Lote" -> "lote"
            x -> x
        }

data CotizacionDetalle = CotizacionDetalle
    { ultimoPrecio :: Double
    , puntas :: [Punta]
    } deriving (Show, Generic)

instance FromJSON CotizacionDetalle where
    parseJSON = genericParseJSON defaultOptions

newtype CotizacionesResponse = CotizacionesResponse
    { titulos :: [Instrumento]
    } deriving (Show, Generic)

instance FromJSON CotizacionesResponse where
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

newtype EstadoCuenta = EstadoCuenta { cuentas :: [Cuenta] } 
    deriving (Show, Generic)

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
    { ordenMercado :: String      -- ['bCBA', 'nYSE', 'nASDAQ', 'aMEX', 'bCS', 'rOFX']
    , ordenSimbolo :: String
    , ordenCantidad :: Maybe Int  -- opcional
    , ordenPrecio :: Double
    , ordenPlazo :: String        -- ['t0', 't1', 't2', 't3']
    , ordenValidez :: String
    , ordenTipoOrden :: Maybe String  -- opcional: ['precioLimite', 'precioMercado']
    , ordenMonto :: Maybe Double      -- opcional
    , ordenIdFuente :: Maybe Int      -- opcional
    } deriving (Show, Generic)

instance ToJSON OrdenRequest where
    toJSON = genericToJSON defaultOptions
        { fieldLabelModifier = \s -> case drop 5 s of  -- quita el prefijo "orden"
            "Mercado" -> "mercado"
            "Simbolo" -> "simbolo"
            "Cantidad" -> "cantidad"
            "Precio" -> "precio"
            "Plazo" -> "plazo"
            "Validez" -> "validez"
            "TipoOrden" -> "tipoOrden"
            "Monto" -> "monto"
            "IdFuente" -> "idFuente"
            x -> x
        , omitNothingFields = True  -- omite campos que son Nothing
        }

-- Tipo para operaciones
data Operacion = Operacion
    { operacionTicket :: String
    , operacionTimestamp :: UTCTime
    , operacionPrecio :: Double
    , operacionDolarMEP :: Double
    , operacionDolarSym :: Double
    , operacionCantidad :: Int
    , operacionTipo :: String
    , operacionEstado :: String
    } deriving (Show, Generic)

-- Configuración global
montoOperacion :: Double
montoOperacion = 120000.0  -- Valor inicial, ajustar según necesidad

-- Tipo para la cotización del dólar MEP
newtype DolarMEP = DolarMEP Double
    deriving (Show, Generic)

instance FromJSON DolarMEP where
    parseJSON = genericParseJSON defaultOptions