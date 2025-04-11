{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database
    ( initializeDatabase
    , resetDatabase
    , connectDatabase
    , insertTenencia
    , insertEstadoCuenta
    , insertToken
    , insertTicket
    , getTicket
    , getAllTickets
    , updateTicket
    , getLatestTenencias
    , getLatestEstadoCuenta
    , getLastEstadoCuenta
    , getLatestToken
    , DBTenencia(..)
    , DBEstadoCuenta(..)
    , DBToken(..)
    , DBTicket(..)
    , DBPuntaHistorial(..)
    , insertOperacion
    , insertPuntaHistorial
    , getLatestPuntasHistorial
    , getPuntasHistorialByTicket
    , deleteTable
    ) where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow()
import Database.SQLite.Simple.ToRow()
import qualified Data.Text as T
import Data.Time
import Types
import Utils (getCurrentTimeArgentina)

-- Tipos de datos para la base de datos
data DBTenencia = DBTenencia
    { tenenciaSymbol :: T.Text
    , tenenciaCantidad :: Int
    , tenenciaPrecioCompra :: Double
    , tenenciaTimestamp :: UTCTime
    } deriving (Show)

data DBEstadoCuenta = DBEstadoCuenta
    { cuentaNumero :: T.Text
    , cuentaTipo :: T.Text
    , cuentaMoneda :: T.Text
    , cuentaSaldo :: Double
    , cuentaComprometido :: Double
    , cuentaDisponible :: Double
    , cuentaTimestamp :: UTCTime
    } deriving (Show)

data DBToken = DBToken
    { tokenValue :: T.Text
    , tokenTimestamp :: UTCTime
    } deriving (Show)

data DBTicket = DBTicket
    { dbTicketName    :: T.Text
    , dbEstado        :: T.Text
    , dbCompra1       :: Double
    , dbCompra2       :: Double
    , dbVenta1        :: Double
    , dbVenta2        :: Double
    , dbTakeProfit    :: Double
    , dbStopLoss      :: Double
    , dbPuntaCompra   :: Double
    , dbPuntaVenta    :: Double
    , dbLastUpdate    :: UTCTime
    } deriving (Show)

data DBOperaciones = DBOperaciones
    { dbOperacionTicket :: T.Text
    , dbOperacionTimestamp :: UTCTime
    , dbOperacionPrecio :: Double
    , dbOperacionDolarMEP :: Double
    , dbOperacionDolarSym :: Double
    , dbOperacionCantidad :: Int
    , dbOperacionTipo :: T.Text
    , dbOperacionEstado :: T.Text
    } deriving (Show)

data DBPuntaHistorial = DBPuntaHistorial
    { dbPuntaTicket :: T.Text
    , dbPuntaPrecioCompra :: Double
    , dbPuntaCantidadCompra :: Double
    , dbPuntaPrecioVenta :: Double
    , dbPuntaCantidadVenta :: Double
    , dbPuntaTimestamp :: UTCTime
    } deriving (Show)

instance FromRow DBOperaciones where
    fromRow = DBOperaciones <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DBOperaciones where
    toRow DBOperaciones{..} = [SQLText dbOperacionTicket,
                               SQLText $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" dbOperacionTimestamp,
                               SQLFloat dbOperacionPrecio,
                               SQLFloat dbOperacionDolarMEP,
                               SQLFloat dbOperacionDolarSym,
                               SQLInteger $ fromIntegral dbOperacionCantidad,
                               SQLText dbOperacionTipo,
                               SQLText dbOperacionEstado]

instance FromRow DBTenencia where
    fromRow = DBTenencia <$> field <*> field <*> field <*> field

instance ToRow DBTenencia where
    toRow t = [SQLText $ tenenciaSymbol t,
               SQLInteger $ fromIntegral $ tenenciaCantidad t,
               SQLFloat $ tenenciaPrecioCompra t,
               SQLText $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ tenenciaTimestamp t]

instance FromRow DBEstadoCuenta where
    fromRow = DBEstadoCuenta <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DBEstadoCuenta where
    toRow e = [SQLText $ cuentaNumero e,
               SQLText $ cuentaTipo e,
               SQLText $ cuentaMoneda e,
               SQLFloat $ cuentaSaldo e,
               SQLFloat $ cuentaComprometido e,
               SQLFloat $ cuentaDisponible e,
               SQLText $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ cuentaTimestamp e]

instance FromRow DBToken where
    fromRow = DBToken <$> field <*> field

instance ToRow DBToken where
    toRow t = [SQLText $ tokenValue t,
               SQLText $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ tokenTimestamp t]

instance FromRow DBTicket where
    fromRow = DBTicket <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DBTicket where
    toRow t = [ SQLText $ dbTicketName t
              , SQLText $ dbEstado t
              , SQLFloat $ dbCompra1 t
              , SQLFloat $ dbCompra2 t
              , SQLFloat $ dbVenta1 t
              , SQLFloat $ dbVenta2 t
              , SQLFloat $ dbTakeProfit t
              , SQLFloat $ dbStopLoss t
              , SQLFloat $ dbPuntaCompra t
              , SQLFloat $ dbPuntaVenta t
              , SQLText $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ dbLastUpdate t
              ]

instance FromRow DBPuntaHistorial where
    fromRow = DBPuntaHistorial <$> field <*> field <*> field <*> field <*> field <*> field

instance ToRow DBPuntaHistorial where
    toRow p = [ SQLText $ dbPuntaTicket p
              , SQLFloat $ dbPuntaPrecioCompra p
              , SQLFloat $ dbPuntaCantidadCompra p
              , SQLFloat $ dbPuntaPrecioVenta p
              , SQLFloat $ dbPuntaCantidadVenta p
              , SQLText $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $ dbPuntaTimestamp p
              ]

-- Conexión a la base de datos
connectDatabase :: IO Connection
connectDatabase = do
    conn <- open "iol.db"
    createTablesIfNotExist conn
    return conn

-- Crear tablas si no existen
createTablesIfNotExist :: Connection -> IO ()
createTablesIfNotExist conn = do
    execute_ conn "CREATE TABLE IF NOT EXISTS tenencias (\
        \symbol TEXT NOT NULL,\
        \cantidad INTEGER NOT NULL,\
        \precio_compra REAL NOT NULL,\
        \timestamp DATETIME NOT NULL,\
        \PRIMARY KEY (symbol, timestamp))"
    
    execute_ conn "CREATE TABLE IF NOT EXISTS estado_cuenta (\
        \numero_cuenta TEXT NOT NULL,\
        \tipo_cuenta TEXT NOT NULL,\
        \moneda TEXT NOT NULL,\
        \saldo REAL NOT NULL,\
        \comprometido REAL NOT NULL,\
        \disponible REAL NOT NULL,\
        \timestamp DATETIME NOT NULL,\
        \PRIMARY KEY (numero_cuenta, tipo_cuenta, timestamp))"
    
    execute_ conn "CREATE TABLE IF NOT EXISTS tokens (\
        \token TEXT NOT NULL,\
        \timestamp DATETIME NOT NULL PRIMARY KEY)"

    execute_ conn "CREATE TABLE IF NOT EXISTS tickets (\
        \ticket_name TEXT PRIMARY KEY,\
        \estado TEXT NOT NULL,\
        \compra1 REAL NOT NULL,\
        \compra2 REAL NOT NULL,\
        \venta1 REAL NOT NULL,\
        \venta2 REAL NOT NULL,\
        \take_profit REAL NOT NULL,\
        \stop_loss REAL NOT NULL,\
        \punta_compra REAL NOT NULL,\
        \punta_venta REAL NOT NULL,\
        \last_update DATETIME NOT NULL)"
    
    execute_ conn "CREATE TABLE IF NOT EXISTS operaciones (\
        \ticket_name TEXT NOT NULL,\
        \timestamp DATETIME NOT NULL,\
        \precio REAL NOT NULL,\
        \dolar_mep REAL NOT NULL,\
        \dolar_sym REAL NOT NULL,\
        \cantidad INTEGER NOT NULL,\
        \tipo TEXT NOT NULL,\
        \estado TEXT NOT NULL)"
        
    execute_ conn "CREATE TABLE IF NOT EXISTS puntas_historial (\
        \ticket TEXT NOT NULL,\
        \precio_compra REAL NOT NULL,\
        \cantidad_compra REAL NOT NULL,\
        \precio_venta REAL NOT NULL,\
        \cantidad_venta REAL NOT NULL,\
        \timestamp DATETIME NOT NULL,\
        \PRIMARY KEY (ticket, timestamp))"

-- Inicialización de la base de datos (mantiene compatibilidad hacia atrás)
initializeDatabase :: IO Connection
initializeDatabase = connectDatabase

-- Reset completo de la base de datos
resetDatabase :: IO Connection
resetDatabase = do
    conn <- open "iol.db"
    -- Eliminar todas las tablas existentes
    execute_ conn "DROP TABLE IF EXISTS cotizaciones"
    execute_ conn "DROP TABLE IF EXISTS tenencias"
    execute_ conn "DROP TABLE IF EXISTS estado_cuenta"
    execute_ conn "DROP TABLE IF EXISTS tokens"
    execute_ conn "DROP TABLE IF EXISTS tickets"
    
    -- Crear las tablas nuevamente
    createTablesIfNotExist conn
    return conn

-- Conversión entre tipos DB y dominio
ticketToDBTicket :: Ticket -> DBTicket
ticketToDBTicket ticket = DBTicket
    { dbTicketName = T.pack $ ticketName ticket
    , dbEstado = T.pack $ show $ estado ticket
    , dbCompra1 = compra1 $ precios ticket
    , dbCompra2 = compra2 $ precios ticket
    , dbVenta1 = venta1 $ precios ticket
    , dbVenta2 = venta2 $ precios ticket
    , dbTakeProfit = takeProfit $ precios ticket
    , dbStopLoss = stopLoss $ precios ticket
    , dbPuntaCompra = puntaCompra ticket
    , dbPuntaVenta = puntaVenta ticket
    , dbLastUpdate = lastUpdate ticket
    }

dbTicketToTicket :: DBTicket -> Ticket
dbTicketToTicket dbTicket = Ticket
    { ticketName = T.unpack $ dbTicketName dbTicket
    , estado = read $ T.unpack $ dbEstado dbTicket
    , precios = Precios
        { compra1 = dbCompra1 dbTicket
        , compra2 = dbCompra2 dbTicket
        , venta1 = dbVenta1 dbTicket
        , venta2 = dbVenta2 dbTicket
        , takeProfit = dbTakeProfit dbTicket
        , stopLoss = dbStopLoss dbTicket
        }
    , puntaCompra = dbPuntaCompra dbTicket
    , puntaVenta = dbPuntaVenta dbTicket
    , lastUpdate = dbLastUpdate dbTicket
    }

-- Funciones de inserción
insertTenencia :: Connection -> T.Text -> Int -> Double -> IO ()
insertTenencia conn symbol cantidad precio = do
    now <- getCurrentTimeArgentina
    execute conn "INSERT OR REPLACE INTO tenencias (symbol, cantidad, precio_compra, timestamp) VALUES (?, ?, ?, datetime(?))"
        (DBTenencia symbol cantidad precio now)

deleteTable :: Connection -> String -> IO ()
deleteTable conn tableName = 
    execute_ conn $ Query $ "DELETE FROM " <> T.pack tableName

insertEstadoCuenta :: Connection -> Cuenta -> IO ()
insertEstadoCuenta conn cuenta = do
    now <- getCurrentTimeArgentina
    case saldos cuenta of
        (s:_) -> do
            -- Insertar el nuevo estado
            execute conn "INSERT INTO estado_cuenta (numero_cuenta, tipo_cuenta, moneda, saldo, comprometido, disponible, timestamp) VALUES (?, ?, ?, ?, ?, ?, ?)"
                (DBEstadoCuenta
                    (T.pack $ numero cuenta)
                    (T.pack $ tipo cuenta)
                    (T.pack $ moneda cuenta)
                    (saldo s)
                    (comprometido s)
                    (disponible s)
                    now)
        [] -> return ()

insertOperacion :: Connection -> Operacion -> IO ()
insertOperacion conn operacion =
    execute conn "INSERT OR REPLACE INTO operaciones (ticket_name, timestamp, precio, dolar_mep, dolar_sym, cantidad, tipo, estado) VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
        (DBOperaciones
            (T.pack $ operacionTicket operacion)
            (operacionTimestamp operacion)
            (operacionPrecio operacion)
            (operacionDolarMEP operacion)
            (operacionDolarSym operacion)
            (operacionCantidad operacion)
            (T.pack $ operacionTipo operacion)
            (T.pack $ operacionEstado operacion))

insertToken :: Connection -> String -> IO ()
insertToken conn token = do
    now <- getCurrentTimeArgentina
    execute conn "INSERT OR REPLACE INTO tokens (token, timestamp) VALUES (?, datetime(?))"
        (DBToken (T.pack token) now)

insertTicket :: Connection -> Ticket -> IO ()
insertTicket conn ticket = do
    execute conn "INSERT OR REPLACE INTO tickets \
        \(ticket_name, estado, compra1, compra2, venta1, venta2, take_profit, stop_loss, punta_compra, punta_venta, last_update) \
        \VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, datetime(?))"
        (ticketToDBTicket ticket)

insertPuntaHistorial :: Connection -> PuntaHistorial -> IO ()
insertPuntaHistorial conn punta = do
    execute conn "INSERT INTO puntas_historial (ticket, precio_compra, cantidad_compra, precio_venta, cantidad_venta, timestamp) VALUES (?, ?, ?, ?, ?, ?)"
        (DBPuntaHistorial
            (T.pack $ puntaHistorialTicket punta)
            (puntaHistorialPrecioCompra punta)
            (puntaHistorialCantidadCompra punta)
            (puntaHistorialPrecioVenta punta)
            (puntaHistorialCantidadVenta punta)
            (puntaHistorialTimestamp punta))

-- Funciones de consulta
getLatestTenencias :: Connection -> IO [DBTenencia]
getLatestTenencias conn = do
    query_ conn
        "SELECT DISTINCT t1.* FROM tenencias t1 \
        \INNER JOIN (SELECT symbol, MAX(timestamp) as max_ts FROM tenencias GROUP BY symbol) t2 \
        \ON t1.symbol = t2.symbol AND t1.timestamp = t2.max_ts"

getLatestEstadoCuenta :: Connection -> IO [DBEstadoCuenta]
getLatestEstadoCuenta conn = do
    putStrLn "Ejecutando consulta SQL para estado de cuenta..."
    results <- query_ conn
        "SELECT e1.* FROM estado_cuenta e1 \
        \INNER JOIN (SELECT numero_cuenta, tipo_cuenta, MAX(timestamp) as max_ts FROM estado_cuenta GROUP BY numero_cuenta, tipo_cuenta) e2 \
        \ON e1.numero_cuenta = e2.numero_cuenta AND e1.tipo_cuenta = e2.tipo_cuenta AND e1.timestamp = e2.max_ts \
        \ORDER BY e1.numero_cuenta"
    putStrLn $ "Número de resultados encontrados: " ++ show (length results)
    return results

getLastEstadoCuenta :: Connection -> IO [DBEstadoCuenta]
getLastEstadoCuenta conn = query_ conn
    "SELECT e1.* FROM estado_cuenta e1 \
    \INNER JOIN ( \
    \    SELECT numero_cuenta, tipo_cuenta, MAX(timestamp) as max_timestamp \
    \    FROM estado_cuenta \
    \    GROUP BY numero_cuenta, tipo_cuenta \
    \) e2 \
    \ON e1.numero_cuenta = e2.numero_cuenta \
    \AND e1.tipo_cuenta = e2.tipo_cuenta \
    \AND e1.timestamp = e2.max_timestamp"

getLatestToken :: Connection -> IO (Maybe DBToken)
getLatestToken conn = do
    results <- query_ conn
        "SELECT * FROM tokens ORDER BY timestamp DESC LIMIT 1" :: IO [DBToken]
    return $ case results of
        (x:_) -> Just x
        [] -> Nothing

getTicket :: Connection -> String -> IO (Maybe Ticket)
getTicket conn name = do
    results <- query conn
        "SELECT * FROM tickets WHERE ticket_name = ?"
        (Only $ T.pack name) :: IO [DBTicket]
    return $ case results of
        (x:_) -> Just $ dbTicketToTicket x
        [] -> Nothing

getAllTickets :: Connection -> IO [Ticket]
getAllTickets conn = do
    results <- query_ conn "SELECT * FROM tickets" :: IO [DBTicket]
    return $ map dbTicketToTicket results

getLatestPuntasHistorial :: Connection -> Int -> IO [PuntaHistorial]
getLatestPuntasHistorial conn limit = do
    rows <- query conn "SELECT ticket, precio_compra, cantidad_compra, precio_venta, cantidad_venta, timestamp FROM puntas_historial ORDER BY timestamp DESC LIMIT ?" (Only limit) :: IO [DBPuntaHistorial]
    return $ map dbToPuntaHistorial rows
  where
    dbToPuntaHistorial :: DBPuntaHistorial -> PuntaHistorial
    dbToPuntaHistorial db = PuntaHistorial
        { puntaHistorialTicket = T.unpack $ dbPuntaTicket db
        , puntaHistorialPrecioCompra = dbPuntaPrecioCompra db
        , puntaHistorialCantidadCompra = dbPuntaCantidadCompra db
        , puntaHistorialPrecioVenta = dbPuntaPrecioVenta db
        , puntaHistorialCantidadVenta = dbPuntaCantidadVenta db
        , puntaHistorialTimestamp = dbPuntaTimestamp db
        }

getPuntasHistorialByTicket :: Connection -> String -> Int -> IO [PuntaHistorial]
getPuntasHistorialByTicket conn ticket limit = do
    rows <- query conn "SELECT ticket, precio_compra, cantidad_compra, precio_venta, cantidad_venta, timestamp FROM puntas_historial WHERE ticket = ? ORDER BY timestamp DESC LIMIT ?" (T.pack ticket, limit) :: IO [DBPuntaHistorial]
    return $ map dbToPuntaHistorial rows
  where
    dbToPuntaHistorial :: DBPuntaHistorial -> PuntaHistorial
    dbToPuntaHistorial db = PuntaHistorial
        { puntaHistorialTicket = T.unpack $ dbPuntaTicket db
        , puntaHistorialPrecioCompra = dbPuntaPrecioCompra db
        , puntaHistorialCantidadCompra = dbPuntaCantidadCompra db
        , puntaHistorialPrecioVenta = dbPuntaPrecioVenta db
        , puntaHistorialCantidadVenta = dbPuntaCantidadVenta db
        , puntaHistorialTimestamp = dbPuntaTimestamp db
        }

updateTicket :: Connection -> Ticket -> IO ()
updateTicket = insertTicket -- Using REPLACE INTO in insertTicket makes this equivalent
